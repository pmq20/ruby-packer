# Copyright (c) 2017 Minqi Pan <pmq2001@gmail.com>
# 
# This file is part of Ruby Compiler, distributed under the MIT License
# For full terms see the included LICENSE file

require "compiler/constants"
require "compiler/error"
require "compiler/utils"
require "compiler/gem_package"
require 'shellwords'
require 'tmpdir'
require 'fileutils'
require 'open3'
require 'pathname'
require 'uri'

class Compiler
  def self.ruby_api_version
    @ruby_api_version ||= peek_ruby_api_version
  end
  
  def self.ruby_version
    @ruby_version ||= peek_ruby_version
  end
  
  def self.peek_ruby_version
    version_info = File.read(File.join(PRJ_ROOT, 'ruby/version.h'))
    if version_info =~ /RUBY_VERSION\s+"([^"]+)"\s*$/
      return $1.dup
    else
      raise 'Cannot peek RUBY_VERSION'
    end
  end
  
  def self.peek_ruby_api_version
    version_info = File.read(File.join(PRJ_ROOT, 'ruby/include/ruby/version.h'))
    versions = []
    if version_info =~ /RUBY_API_VERSION_MAJOR\s+(\d+)/
      versions << $1.dup
    else
      raise 'Cannot peek RUBY_API_VERSION_MAJOR'
    end
    if version_info =~ /RUBY_API_VERSION_MINOR\s+(\d+)/
      versions << $1.dup
    else
      raise 'Cannot peek RUBY_API_VERSION_MINOR'
    end
    if version_info =~ /RUBY_API_VERSION_TEENY\s+(\d+)/
      versions << $1.dup
    else
      raise 'Cannot peek RUBY_API_VERSION_TEENY'
    end
    versions.join('.')
  end
  
  def initialize(entrance, options = {})
    if entrance
      if File.exists?(File.expand_path(entrance))
        @entrance = File.expand_path(entrance)
      else
        @entrance = entrance
      end
    end
    @options = options
    @utils = Utils.new(options)

    init_options
    init_entrance if entrance
    init_tmpdir

    STDERR.puts "Ruby Compiler (rubyc) v#{::Compiler::VERSION}" unless @options[:quiet]
    if entrance
      STDERR.puts "- entrance: #{@entrance}" unless @options[:quiet]
    else
      STDERR.puts "- entrance: not provided, a single Ruby interpreter executable will be produced." unless @options[:quiet]
      STDERR.puts "- HINT: call rubyc with --help to see more options and use case examples" unless @options[:quiet]
    end
    STDERR.puts "- options: #{@options}" unless @options[:quiet]
    STDERR.puts unless @options[:quiet]

    prepare_flags1
    stuff_tmpdir
  end

  def init_options
    @options[:make_args] ||= '-j4'
    if Gem.win_platform?
      @options[:output] ||= 'a.exe'
    else
      @options[:output] ||= 'a.out'
    end
    @options[:output] = File.expand_path(@options[:output])
    @options[:tmpdir] ||= File.expand_path("rubyc", Dir.tmpdir)
    @options[:tmpdir] = File.expand_path(@options[:tmpdir])
    @gem_package = GemPackage.new(@entrance, @options, @utils) if @options[:gem]
    if @options[:auto_update_url] || @options[:auto_update_base]
      unless @options[:auto_update_url].length > 0 && @options[:auto_update_base].length > 0
        raise Error, "Please provide both --auto-update-url and --auto-update-base"
      end
    end

    @ruby_dir = File.join(@options[:tmpdir], "ruby-#{::Compiler.ruby_version}-#{::Compiler::VERSION}")
  end

  def init_entrance
    if @gem_package
      @root = @gem_package.work_dir
      return
    end
    if @options[:root]
      @root = File.expand_path(@options[:root])
    else
      @root = @entrance.dup
      while true
        @root = File.expand_path('..', @root)
        if File.expand_path('..', @root) == @root
          @root = Dir.pwd
          break
        end
        if File.exist?(File.join(@root, 'Gemfile')) || Dir.exist?(File.join(@root, '.git'))
          break 
        end
      end
      STDERR.puts "-> Project root not supplied, #{@root} assumed." unless @options[:quiet]
    end
  end

  def init_tmpdir
    if @root && @options[:tmpdir].include?(@root)
      raise Error, "Tempdir #{@options[:tmpdir]} cannot reside inside #{@root}."
    end
  end
  
  def stuff_zlib
    target = File.join(@options[:tmpdir], 'zlib')
    unless Dir.exist?(target)
      @utils.cp_r(File.join(PRJ_ROOT, 'vendor', 'zlib'), target, preserve: true)
      @utils.chdir(target) do
        Dir['**/configure.ac'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        Dir['**/*.m4'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        if Gem.win_platform?
          @utils.run(@compile_env, 'nmake /f win32\\Makefile.msc')
        else
          @utils.run(@compile_env, './configure --static')
          @utils.run(@compile_env, "make #{@options[:make_args]}")
        end
        Dir['*.{dylib,so,dll}'].each do |thisdl|
          @utils.rm_f(thisdl)
        end
      end
    end
  end

  def stuff_openssl
    return if Gem.win_platform? # TODO
    target = File.join(@options[:tmpdir], 'openssl')
    unless Dir.exist?(target)
      @utils.cp_r(File.join(PRJ_ROOT, 'vendor', 'openssl'), target, preserve: true)
      @utils.chdir(target) do
        Dir['**/configure.ac'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        Dir['**/*.m4'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        if Gem.win_platform?
          # TODO
        else
          @utils.run(@compile_env, './config')
          @utils.run(@compile_env, "make #{@options[:make_args]}")
        end
        Dir['*.{dylib,so,dll}'].each do |thisdl|
          @utils.rm_f(thisdl)
        end
      end
    end
  end
  
  def stuff_gdbm
    return if Gem.win_platform? # TODO
    target = File.join(@options[:tmpdir], 'gdbm')
    unless Dir.exist?(target)
      @utils.cp_r(File.join(PRJ_ROOT, 'vendor', 'gdbm'), target, preserve: true)
      @utils.chdir(target) do
        Dir['**/configure.ac'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        Dir['**/*.m4'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        if Gem.win_platform?
          # TODO
        else
          @utils.run(@compile_env, "./configure --with-pic --enable-libgdbm-compat --disable-shared --enable-static --without-readline --prefix=#{@utils.escape File.join(@options[:tmpdir], 'gdbm', 'build')}")
          @utils.run(@compile_env, "make #{@options[:make_args]}")
          @utils.run(@compile_env, "make install")
        end
      end
    end
  end
  
  def stuff_yaml
    return if Gem.win_platform? # TODO
    target = File.join(@options[:tmpdir], 'yaml')
    unless Dir.exist?(target)
      @utils.cp_r(File.join(PRJ_ROOT, 'vendor', 'yaml'), target, preserve: true)
      @utils.chdir(target) do
        Dir['**/configure.ac'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        Dir['**/*.m4'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        if Gem.win_platform?
          # TODO
        else
          @utils.run(@compile_env, "./configure --with-pic --disable-shared --enable-static --prefix=#{@utils.escape File.join(@options[:tmpdir], 'yaml', 'build')}")
          @utils.run(@compile_env, "make #{@options[:make_args]}")
          @utils.run(@compile_env, "make install")
        end
      end
    end
  end

  def stuff_libffi
    return if Gem.win_platform? # TODO
    target = File.join(@options[:tmpdir], 'libffi')
    unless Dir.exist?(target)
      @utils.cp_r(File.join(PRJ_ROOT, 'vendor', 'libffi'), target, preserve: true)
      @utils.chdir(target) do
        Dir['**/configure.ac'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        Dir['**/*.m4'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        if Gem.win_platform?
          # TODO
        else
          @utils.run(@compile_env, "./configure --with-pic --disable-shared --enable-static --prefix=#{@utils.escape File.join(@options[:tmpdir], 'libffi', 'build')}")
          @utils.run(@compile_env, "make #{@options[:make_args]}")
          @utils.run(@compile_env, "make install")
        end
      end
    end
  end
  
  def stuff_ncurses
    return if Gem.win_platform? # TODO
    target = File.join(@options[:tmpdir], 'ncurses')
    unless Dir.exist?(target)
      @utils.cp_r(File.join(PRJ_ROOT, 'vendor', 'ncurses'), target, preserve: true)
      @utils.chdir(target) do
        Dir['**/configure.ac'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        Dir['**/*.m4'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        if Gem.win_platform?
          # TODO
        else
          @utils.run(@compile_env, "./configure --without-shared --without-cxx-shared --prefix=#{@utils.escape File.join(@options[:tmpdir], 'ncurses', 'build')}")
          @utils.run(@compile_env, "make #{@options[:make_args]}")
          @utils.run(@compile_env, "make install")
        end
      end
    end
  end
  
  def stuff_readline
    return if Gem.win_platform? # TODO
    target = File.join(@options[:tmpdir], 'readline')
    unless Dir.exist?(target)
      @utils.cp_r(File.join(PRJ_ROOT, 'vendor', 'readline'), target, preserve: true)
      @utils.chdir(target) do
        Dir['**/configure.ac'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        Dir['**/*.m4'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        if Gem.win_platform?
          # TODO
        else
          @utils.run(@compile_env, "./configure --with-pic --disable-shared --enable-static --prefix=#{@utils.escape File.join(@options[:tmpdir], 'readline', 'build')}")
          @utils.run(@compile_env, "make #{@options[:make_args]}")
          @utils.run(@compile_env, "make install")
        end
      end
    end
  end
  
  def stuff_tmpdir
    @utils.rm_rf(@options[:tmpdir]) if @options[:clean_tmpdir]
    @utils.mkdir_p(@options[:tmpdir])

    stuff_zlib
    stuff_openssl
    stuff_gdbm
    stuff_yaml
    stuff_libffi
    stuff_ncurses
    stuff_readline
    prepare_flags2

    target = @ruby_dir
    @ruby_build_1 = File.join(@options[:tmpdir], 'ruby_build_1')
    @ruby_build_2 = File.join(@options[:tmpdir], 'ruby_build_2')

    unless Dir.exist?(target)
      @utils.cp_r(File.join(PRJ_ROOT, 'ruby'), target, preserve: true)
      @utils.chdir(target) do
        Dir['**/configure.ac'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
        Dir['**/*.m4'].each do |x|
          File.utime(Time.at(0), Time.at(0), x)
        end
      end
      # PATCH common.mk
      target = File.join(@ruby_dir, 'common.mk')
      target_content = File.read(target)
      found = false
      File.open(target, 'w') do |f|
        target_content.each_line do |line|
          if !found && (line =~ /^INCFLAGS = (.*)$/)
            found = true
            f.puts "INCFLAGS = #{$1} #{@cflags}"
          else
            f.print line
          end
        end
      end
      raise "Failed to patch INCFLAGS of #{target}" unless found
      
      # PATCH win32\Makefile.sub
      if Gem.win_platform?
        target = File.join(@ruby_dir, 'win32', 'Makefile.sub')
        target_content = File.read(target)
        found = 0
        File.open(target, 'w') do |f|
          target_content.each_line do |line|
            if 0 == found && (line =~ /^CFLAGS = (.*)$/)
              found = 1
              f.puts "CFLAGS = #{$1} #{@cflags}"
            elsif 1 == found && (line =~ /^LDFLAGS = (.*)$/)
              found = 2
              f.puts "LDFLAGS = #{$1} #{@ldflags}"
            else
              f.print line
            end
          end
        end
        raise "Failed to patch CFLAGS and LDFLAGS of #{target}" unless 2 == found
      end
    end

    @vendor_ruby = @ruby_dir
    @gem_package.stuff_tmpdir if @gem_package
  end

  def run!
    pass_1 = File.join(@options[:tmpdir], 'build_pass_1')
    @utils.mkdir_p(pass_1)
    pass_2 = File.join(@options[:tmpdir], 'build_pass_2')
    @utils.mkdir_p(pass_2)

    @utils.chdir(@vendor_ruby) do
      sep = Gem.win_platform? ? ';' : ':'
      if Gem.win_platform?
        unless File.exist?(@ruby_build_1)
          @compile_env['ENCLOSE_IO_RUBYC_1ST_PASS'] = '1'
          @compile_env['ENCLOSE_IO_RUBYC_2ND_PASS'] = nil
          # enclose_io_memfs.o - 1st pass
          Dir.chdir(pass_1) do
            @utils.run(@compile_env, "call #{@utils.escape @ruby_dir}\\win32\\configure.bat \
                                    --disable-install-doc \
                                    --prefix=#{@utils.escape @ruby_build_1}")
            @utils.run(@compile_env, "nmake #{@options[:nmake_args]}")
            @utils.run(@compile_env, "nmake install")
            File.open(File.join(@ruby_dir, 'ext', 'Setup'), 'w') do |f|
              f.puts 'option nodynamic'
            end
            # TODO make those win32 ext work
            @utils.chdir('ext') do
              @utils.rm_rf('dbm')
              @utils.rm_rf('digest')
              @utils.rm_rf('etc')
              @utils.rm_rf('fiddle')
              @utils.rm_rf('gdbm')
              @utils.rm_rf('mathn')
              @utils.rm_rf('openssl')
              @utils.rm_rf('pty')
              @utils.rm_rf('readline')
              @utils.rm_rf('ripper')
              @utils.rm_rf('socket')
              @utils.rm_rf('win32')
              @utils.rm_rf('win32ole')
            end
            # PATCH win32\Makefile.sub for 2nd pass
            if Gem.win_platform?
              target = File.join(@ruby_dir, 'win32', 'Makefile.sub')
              target_content = File.read(target)
              found = 0
              File.open(target, 'w') do |f|
                target_content.each_line do |line|
                  if 0 == found && (line =~ /^#define LOAD_RELATIVE 1$/)
                    found = 1
                    f.puts ""
                  else
                    f.print line
                  end
                end
              end
              raise "Failed to patch CFLAGS and LDFLAGS of #{target}" unless 1 == found
            end
          end
        end
        # enclose_io_memfs.o - 2nd pass
        prepare_work_dir
        prepare_local if @entrance
        @utils.rm_f('include/enclose_io.h')
        @utils.rm_f('enclose_io_memfs.c')
        @compile_env['ENCLOSE_IO_RUBYC_1ST_PASS'] = nil
        @compile_env['ENCLOSE_IO_RUBYC_2ND_PASS'] = '1'
        Dir.chdir(pass_2) do
          @utils.run(@compile_env, "call #{@utils.escape @ruby_dir}\\win32\\configure.bat \
                                  --prefix=#{@utils.escape @ruby_build_2} \
                                  --enable-bundled-libyaml \
                                  --enable-debug-env \
                                  --disable-install-doc \
                                  --with-static-linked-ext")
          make_enclose_io_memfs
          make_enclose_io_vars
          @utils.run_allow_failures(@compile_env, "nmake #{@options[:nmake_args]}")
          @utils.run(@compile_env, %Q{nmake #{@options[:nmake_args]} -f enc.mk V="0" UNICODE_HDR_DIR="./enc/unicode/9.0.0"  RUBY=".\\miniruby.exe -I./lib -I. " MINIRUBY=".\\miniruby.exe -I./lib -I. " -l libenc})
          @utils.run(@compile_env, %Q{nmake #{@options[:nmake_args]} -f enc.mk V="0" UNICODE_HDR_DIR="./enc/unicode/9.0.0"  RUBY=".\\miniruby.exe -I./lib -I. " MINIRUBY=".\\miniruby.exe -I./lib -I. " -l libtrans})
          @utils.run(@compile_env, "nmake #{@options[:nmake_args]}")
          @utils.run(@compile_env, "nmake #{@options[:nmake_args]} ruby_static.exe")
          @utils.cp('ruby_static.exe', @options[:output])
        end
      else
        unless File.exist?(@ruby_build_1)
          @compile_env['ENCLOSE_IO_RUBYC_1ST_PASS'] = '1'
          @compile_env['ENCLOSE_IO_RUBYC_2ND_PASS'] = nil
          # enclose_io_memfs.o - 1st pass
          Dir.chdir(pass_1) do
            @utils.run(@compile_env, "#{@utils.escape @ruby_dir}/configure \
                                    --prefix=#{@utils.escape @ruby_build_1} \
                                    --enable-bundled-libyaml \
                                    --without-gmp \
                                    --disable-dtrace \
                                    --enable-debug-env \
                                    --disable-install-rdoc")
            @utils.run(@compile_env, "make #{@options[:make_args]} -j1")
            @utils.run(@compile_env, "make install")
          end
          File.open(File.join(@ruby_dir, 'ext', 'Setup'), 'w') do |f|
            f.puts 'option nodynamic'
          end
        end
        # enclose_io_memfs.o - 2nd pass
        prepare_work_dir
        prepare_local if @entrance
        @utils.rm_f('include/enclose_io.h')
        @utils.rm_f('enclose_io_memfs.c')
        @compile_env['ENCLOSE_IO_RUBYC_1ST_PASS'] = nil
        @compile_env['ENCLOSE_IO_RUBYC_2ND_PASS'] = '1'
        Dir.chdir(pass_2) do
          @utils.run(@compile_env, "#{@utils.escape @ruby_dir}/configure \
                                  --prefix=#{@utils.escape @ruby_build_2} \
                                  --enable-bundled-libyaml \
                                  --without-gmp \
                                  --disable-dtrace \
                                  --enable-debug-env \
                                  --disable-install-rdoc \
                                  --with-static-linked-ext")
          make_enclose_io_memfs
          make_enclose_io_vars
          @utils.run(@compile_env, "make #{@options[:make_args]}")
          @utils.cp('ruby', @options[:output])
        end
      end
    end
  end

  def prepare_work_dir
    # Prepare /__enclose_io_memfs__
    @work_dir = File.join(@options[:tmpdir], 'rubyc_work_dir')
    unless @options[:keep_tmpdir]
      @utils.rm_rf(@work_dir)
      @utils.mkdir_p(@work_dir)
    end
    
    @work_dir_inner = File.join(@work_dir, '__enclose_io_memfs__')
    
    unless @options[:keep_tmpdir]
      @utils.cp_r(@ruby_build_1, @work_dir_inner, preserve: true)

      Dir["#{@work_dir_inner}/**/*.{a,dylib,so,dll,lib,bundle}"].each do |thisdl|
        @utils.rm_f(thisdl)
      end
    end

    @gems_dir = File.join(@work_dir_inner, "lib/ruby/gems/#{self.class.ruby_api_version}")
    
    @path_env = "#{File.join(@ruby_dir, 'build', 'bin')}:#{ENV['PATH']}"
    @local_toolchain = {
      'CI' => 'true',
      'PATH' => @path_env,
      'GEM_HOME' => nil,
      'GEM_PATH' => nil,
      'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1',
      'ENCLOSE_IO_RUBYC_1ST_PASS' => '1',
      'ENCLOSE_IO_RUBYC_2ND_PASS' => nil
    }
  end

  def prepare_local
    # Prepare /__enclose_io_memfs__/local
    @utils.chdir(@root) do
      gemspecs = Dir['./*.gemspec']
      gemfiles = Dir['./Gemfile']
      gems = Dir['./*.gem']
      the_bundler_gem = Dir["#{@vendor_ruby}/vendor/bundler-*.gem"].first
      if gemspecs.size > 0
        raise 'Multiple gemspecs detected' unless 1 == gemspecs.size
        @pre_prepare_dir = File.join(@options[:tmpdir], '__pre_prepare__')
        @utils.rm_rf(@pre_prepare_dir)
        @utils.cp_r(@root, @pre_prepare_dir)
        @utils.chdir(@pre_prepare_dir) do
          STDERR.puts "-> Detected a gemspec, trying to build the gem" unless @options[:quiet]
          @utils.rm_f('./*.gem')
          if gemfiles.size > 0
            @utils.run(@local_toolchain, "gem", "install", the_bundler_gem, '--verbose', '--no-rdoc', '--no-ri')
            @utils.run(@local_toolchain, "gem", "install", the_bundler_gem, '--verbose', '--no-rdoc', '--no-ri', '--install-dir', @gems_dir)
            @utils.run(@local_toolchain, "bundle", "install")
            @utils.run(@local_toolchain, "bundle", "exec", "gem", "build", gemspecs.first)
          else
            @utils.run(@local_toolchain, "gem", "build", gemspecs.first)
          end
          gems = Dir['./*.gem']
          raise 'gem building failed' unless 1 == gems.size
          the_gem = gems.first
          @utils.run(@local_toolchain, "gem", "install", the_gem, "--verbose", '--no-rdoc', '--no-ri', '--install-dir', @gems_dir)
          if File.exist?(File.join(@gems_dir, "bin/#{@entrance}"))
            @memfs_entrance = "#{MEMFS}/lib/ruby/gems/#{self.class.ruby_api_version}/bin/#{@entrance}"
          else
            if File.exist?(File.join(@gems_dir, "bin/#{File.basename(@entrance)}"))
              @entrance = File.basename(@entrance)
              @memfs_entrance = "#{MEMFS}/lib/ruby/gems/#{self.class.ruby_api_version}/bin/#{@entrance}"
            else
              @utils.chdir(File.join(@gems_dir, "bin")) do
                raise Error, "Cannot find entrance #{@entrance}, available entrances are #{ Dir['*'].join(', ') }."
              end
            end
          end
        end
        @utils.rm_rf(@pre_prepare_dir)
      elsif gemfiles.size > 0
        raise 'Multiple Gemfiles detected' unless 1 == gemfiles.size
        # gem install bundler
        @utils.run(@local_toolchain, "gem", "install", the_bundler_gem, '--verbose', '--no-rdoc', '--no-ri')
        @utils.run(@local_toolchain, "gem", "install", the_bundler_gem, '--verbose', '--no-rdoc', '--no-ri', '--install-dir', @gems_dir)
        # bundle install
        @work_dir_local = File.join(@work_dir_inner, 'local')
        @env_bundle_gemfile = '/__enclose_io_memfs__/local/Gemfile'
        unless @options[:keep_tmpdir]
          @utils.cp_r(@root, @work_dir_local)
        end
        @utils.chdir(@work_dir_local) do
          @utils.run(@local_toolchain, 'bundle', 'install', '--deployment')
          if 0 == @utils.run_allow_failures(@local_toolchain, 'bundle', 'show', 'rails')
            STDERR.puts "-> Detected a Rails project" unless @options[:quiet]
            @enclose_io_rails = true
            @utils.rm_rf('tmp')
            @utils.rm_rf('log')
            @utils.mkdir('tmp')
            @utils.mkdir('log')
          end
          if File.exist?(@entrance)
            @memfs_entrance = mempath(@entrance)
          else
            if File.exist?("bin/#{@entrance}")
              @memfs_entrance = "#{MEMFS}/local/bin/#{@entrance}"
            else
              @utils.run(@local_toolchain, 'bundle', 'install', '--deployment', '--binstubs')
              if File.exist?("bin/#{@entrance}")
                @memfs_entrance = "#{MEMFS}/local/bin/#{@entrance}"
              else
                if File.exist?("bin/#{File.basename(@entrance)}")
                  @entrance = File.basename(@entrance)
                  @memfs_entrance = "#{MEMFS}/local/bin/#{@entrance}"
                else
                  @utils.chdir('bin') do
                    raise Error, "Cannot find entrance #{@entrance}, available entrances are #{ Dir['*'].join(', ') }."
                  end
                end
              end
            end
          end
        end
      elsif gems.size > 0
        raise 'Multiple gem files detected' unless 1 == gems.size
        @pre_prepare_dir = File.join(@options[:tmpdir], '__pre_prepare__')
        @utils.rm_rf(@pre_prepare_dir)
        @utils.cp_r(@root, @pre_prepare_dir)
        @utils.chdir(@pre_prepare_dir) do
          STDERR.puts "-> Detected a gem file, trying to locally install the gem" unless @options[:quiet]
          the_gem = gems.first
          @utils.run(@local_toolchain, "gem", "install", the_gem, '--verbose',  '--no-rdoc', '--no-ri', '--install-dir', @gems_dir)
          if File.exist?(File.join(@gems_dir, "bin/#{@entrance}"))
            @memfs_entrance = "#{MEMFS}/lib/ruby/gems/#{self.class.ruby_api_version}/bin/#{@entrance}"
          else
            if File.exist?(File.join(@gems_dir, "bin/#{File.basename(@entrance)}"))
              @entrance = File.basename(@entrance)
              @memfs_entrance = "#{MEMFS}/lib/ruby/gems/#{self.class.ruby_api_version}/bin/#{@entrance}"
            else
              @utils.chdir(File.join(@gems_dir, "bin")) do
                raise Error, "Cannot find entrance #{@entrance}, available entrances are #{ Dir['*'].join(', ') }."
              end
            end
          end
        end
        @utils.rm_rf(@pre_prepare_dir)
      else
        @work_dir_local = File.join(@work_dir_inner, 'local')
        @utils.cp_r(@root, @work_dir_local)
        @utils.chdir(@work_dir_local) do
          x = Pathname.new @entrance
          y = Pathname.new @root
          if x.absolute?
            raise "Entrance #{@entrance} is not in the project root #{@root}" unless @entrance.include?(@root)
            @entrance = x.relative_path_from y
          end
          if File.exist?("#{@entrance}")
            @memfs_entrance = "#{MEMFS}/local/#{@entrance}"
          else
            @utils.chdir('bin') do
              raise Error, "Cannot find entrance #{@entrance}"
            end
          end
        end
      end
      
      if @work_dir_local
        @utils.chdir(@work_dir_local) do
          if Dir.exist?('.git')
            STDERR.puts `git status` unless @options[:quiet]
            @utils.rm_rf('.git')
          end
          if File.exist?('a.exe')
            STDERR.puts `dir a.exe` unless @options[:quiet]
            @utils.rm_rf('a.exe')
          end
          if File.exist?('a.out')
            STDERR.puts `ls -l a.out` unless @options[:quiet]
            @utils.rm_rf('a.out')
          end
        end
      end
      @utils.rm_rf(File.join(@gems_dir, 'cache'))
    end
  end

  def make_enclose_io_memfs
    @utils.chdir(@vendor_ruby) do
      @utils.rm_f('enclose_io_memfs.squashfs')
      @utils.rm_f('enclose_io_memfs.c')
      @utils.run("mksquashfs -version")
      @utils.run("mksquashfs #{@utils.escape @work_dir} enclose_io_memfs.squashfs")
      bytes = IO.binread('enclose_io_memfs.squashfs').bytes
      # TODO slow operation
      # remember to change libsquash's sample/enclose_io_memfs.c as well
      File.open("enclose_io_memfs.c", "w") do |f|
        f.puts '#include <stdint.h>'
        f.puts '#include <stddef.h>'
        f.puts ''
        f.puts "const uint8_t enclose_io_memfs[#{bytes.size}] = { #{bytes[0]}"
        i = 1
        while i < bytes.size
          f.print ','
          f.puts bytes[(i)..(i + 100)].join(',')
          i += 101
        end
        f.puts '};'
        f.puts ''
      end
    end
  end

  def make_enclose_io_vars
    @utils.chdir(@vendor_ruby) do
      File.open("include/enclose_io.h", "w") do |f|
        # remember to change libsquash's sample/enclose_io.h as well
        # might need to remove some object files at the 2nd pass  
        f.puts '#ifndef ENCLOSE_IO_H_999BC1DA'
        f.puts '#define ENCLOSE_IO_H_999BC1DA'
        f.puts ''
        f.puts '#include "enclose_io_prelude.h"'
        f.puts '#include "enclose_io_common.h"'
        f.puts '#include "enclose_io_win32.h"'
        f.puts '#include "enclose_io_unix.h"'
        f.puts ''
        f.puts "#define ENCLOSE_IO_ENV_BUNDLE_GEMFILE #{@env_bundle_gemfile.inspect}" if @env_bundle_gemfile
        f.puts "#define ENCLOSE_IO_ENTRANCE #{@memfs_entrance.inspect}" if @entrance
        f.puts "#define ENCLOSE_IO_RAILS 1" if @enclose_io_rails
        if @options[:auto_update_url] && @options[:auto_update_base]
          f.puts "#define ENCLOSE_IO_AUTO_UPDATE 1"
          f.puts "#define ENCLOSE_IO_AUTO_UPDATE_BASE #{@options[:auto_update_base].inspect}"
          urls = URI.split(@options[:auto_update_url])
          raise 'logic error' unless 9 == urls.length
          port = urls[3]
          if port.nil?
            if 'https' == urls[0]
              port = 443
            else
              port = 80
            end
          end
          f.puts "#define ENCLOSE_IO_AUTO_UPDATE_URL_Scheme #{urls[0].inspect}" if urls[0]
          f.puts "#define ENCLOSE_IO_AUTO_UPDATE_URL_Userinfo #{urls[1].inspect}" if urls[1]
          f.puts "#define ENCLOSE_IO_AUTO_UPDATE_URL_Host #{urls[2].inspect}" if urls[2]
          if Gem.win_platform?
            f.puts "#define ENCLOSE_IO_AUTO_UPDATE_URL_Port #{port.to_s.inspect}"
          else
            f.puts "#define ENCLOSE_IO_AUTO_UPDATE_URL_Port #{port}"
          end
          f.puts "#define ENCLOSE_IO_AUTO_UPDATE_URL_Registry #{urls[4].inspect}" if urls[4]
          f.puts "#define ENCLOSE_IO_AUTO_UPDATE_URL_Path #{urls[5].inspect}" if urls[5]
          f.puts "#define ENCLOSE_IO_AUTO_UPDATE_URL_Opaque #{urls[6].inspect}" if urls[6]
          f.puts "#define ENCLOSE_IO_AUTO_UPDATE_URL_Query #{urls[7].inspect}" if urls[7]
          f.puts "#define ENCLOSE_IO_AUTO_UPDATE_URL_Fragment #{urls[8].inspect}" if urls[8]
        end
        f.puts '#endif'
        f.puts ''
      end
    end
  end

  def mempath(path)
    path = File.expand_path(path)
    raise "path #{path} should start with #{@root}" unless @root == path[0...(@root.size)]
    "#{MEMFS}/local#{path[(@root.size)..-1]}"
  end
  
  def prepare_flags1
    @ldflags = ''
    @cflags = ''

    if Gem.win_platform?
      if @options[:debug]
        @cflags += ' -MD /DEBUG:FULL /Od -Zi '
      else
        @cflags += ' -MD /Ox '
      end
    else
      if @options[:debug]
        @cflags += ' -fPIC -g -O0 -pipe '
      else
        @cflags += ' -fPIC -O3 -fno-fast-math -ggdb3 -Os -fdata-sections -ffunction-sections -pipe '
      end
    end
    
    if Gem.win_platform?
      @compile_env = {
        'CI' => 'true',
        'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1'
      }
    else
      @compile_env = {
        'CI' => 'true',
        'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1',
        'CFLAGS' => @cflags,
        'LDFLAGS' => @ldflags
      }
    end
  end

  def prepare_flags2
    if Gem.win_platform?
      @ldflags += " -libpath:#{@utils.escape File.join(@options[:tmpdir], 'zlib').gsub('/', '\\')} #{@utils.escape File.join(@options[:tmpdir], 'zlib', 'zlib.lib')} "
      @cflags += " -I#{@utils.escape File.join(@options[:tmpdir], 'zlib')} "
    else
      @ldflags += " -L#{@utils.escape File.join(@options[:tmpdir], 'zlib')} #{@utils.escape File.join(@options[:tmpdir], 'zlib', 'libz.a')} "
      @cflags += " -I#{@utils.escape File.join(@options[:tmpdir], 'zlib')} "
      @ldflags += " -L#{@utils.escape File.join(@options[:tmpdir], 'openssl')} "
      @cflags += " -I#{@utils.escape File.join(@options[:tmpdir], 'openssl', 'include')} "
      @ldflags += " -L#{@utils.escape File.join(@options[:tmpdir], 'gdbm', 'build', 'lib')} "
      @cflags += " -I#{@utils.escape File.join(@options[:tmpdir], 'gdbm', 'build', 'include')} "
      @ldflags += " -L#{@utils.escape File.join(@options[:tmpdir], 'yaml', 'build', 'lib')} "
      @cflags += " -I#{@utils.escape File.join(@options[:tmpdir], 'yaml', 'build', 'include')} "
      lib64 = File.join(@options[:tmpdir], 'libffi', 'build', 'lib64')
      lib = File.join(@options[:tmpdir], 'libffi', 'build', 'lib')
      @ldflags += " -L#{@utils.escape lib} " if Dir.exist?(lib)
      @ldflags += " -L#{@utils.escape lib64} " if Dir.exist?(lib64)
      @cflags += " -I#{@utils.escape File.join(@options[:tmpdir], 'libffi', 'build', 'lib', 'libffi-3.2.1', 'include')} "
      @ldflags += " -L#{@utils.escape File.join(@options[:tmpdir], 'ncurses', 'build', 'lib')} "
      @cflags += " -I#{@utils.escape File.join(@options[:tmpdir], 'ncurses', 'build', 'include')} "
      @ldflags += " -L#{@utils.escape File.join(@options[:tmpdir], 'readline', 'build', 'lib')} "
      @cflags += " -I#{@utils.escape File.join(@options[:tmpdir], 'readline', 'build', 'include')} "
    end

    if Gem.win_platform?
      @compile_env = {
        'CI' => 'true',
        'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1'
      }
    else
      @compile_env = {
        'CI' => 'true',
        'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1',
        'CFLAGS' => @cflags,
        'LDFLAGS' => @ldflags
      }
    end
  end
end
