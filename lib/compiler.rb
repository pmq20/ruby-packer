# Copyright (c) 2017 Minqi Pan <pmq2001@gmail.com>
# 
# This file is part of Ruby Compiler, distributed under the MIT License
# For full terms see the included LICENSE file

require "compiler/constants"
require "compiler/error"
require "compiler/utils"
require 'shellwords'
require 'tmpdir'
require 'fileutils'
require 'open3'
require 'pathname'

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
  
  def prepare_flags1
    @ldflags = ''
    @cflags = ''

    if Gem.win_platform?
      if @options[:debug]
        @cflags += ' /DEBUG:FULL /Od -Zi '
      else
        @cflags += ' /Ox '
      end
    else
      if @options[:debug]
        @cflags += ' -fPIC -g -O0 -pipe '
      else
        @cflags += ' -fPIC -O3 -fno-fast-math -ggdb3 -Os -fdata-sections -ffunction-sections -pipe '
      end
    end

    @compile_env = {
      'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1',
      'CFLAGS' => @cflags,
      'LDFLAGS' => @ldflags
    }
  end

  def prepare_flags2
    if Gem.win_platform?
      @ldflags += " -libpath:#{Utils.escape File.join(@options[:tmpdir], 'zlib').gsub('/', '\\')} #{Utils.escape File.join(@options[:tmpdir], 'zlib', 'zlib.lib')} "
      @cflags += " -I#{Utils.escape File.join(@options[:tmpdir], 'zlib')} "
    else
      @ldflags += " -L#{Utils.escape File.join(@options[:tmpdir], 'zlib')} #{Utils.escape File.join(@options[:tmpdir], 'zlib', 'libz.a')} "
      @cflags += " -I#{Utils.escape File.join(@options[:tmpdir], 'zlib')} "
      @ldflags += " -L#{Utils.escape File.join(@options[:tmpdir], 'openssl')}  #{Utils.escape File.join(@options[:tmpdir], 'openssl', 'libcrypto.a')} #{Utils.escape File.join(@options[:tmpdir], 'openssl', 'libssl.a')} "
      @cflags += " -I#{Utils.escape File.join(@options[:tmpdir], 'openssl', 'include')} "
      @ldflags += " -L#{Utils.escape File.join(@options[:tmpdir], 'gdbm', 'build', 'lib')} #{Utils.escape File.join(@options[:tmpdir], 'gdbm', 'build', 'lib', 'libgdbm.a')} "
      @cflags += " -I#{Utils.escape File.join(@options[:tmpdir], 'gdbm', 'build', 'include')} "
      @ldflags += " -L#{Utils.escape File.join(@options[:tmpdir], 'yaml', 'build', 'lib')} #{Utils.escape File.join(@options[:tmpdir], 'yaml', 'build', 'lib', 'libyaml.a')} "
      @cflags += " -I#{Utils.escape File.join(@options[:tmpdir], 'yaml', 'build', 'include')} "
    end

    @compile_env = {
      'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1',
      'CFLAGS' => @cflags,
      'LDFLAGS' => @ldflags
    }
  end
  
  def initialize(entrance, options = {})
    @entrance = File.expand_path(entrance) if entrance
    @options = options

    init_options
    init_entrance if entrance
    init_tmpdir

    STDERR.puts "Ruby Compiler (nodec) v#{::Compiler::VERSION}"
    if entrance
      STDERR.puts "- entrance: #{@entrance}"
    else
      STDERR.puts "- entrance: not provided, a single Ruby interpreter executable will be produced."
    end
    STDERR.puts "- options: #{@options}"
    STDERR.puts

    prepare_flags1
    stuff_tmpdir
    prepare_flags2
  end

  def init_options
    @options[:make_args] ||= '-j4'
    if Gem.win_platform?
      @options[:output] ||= 'a.exe'
    else
      @options[:output] ||= 'a.out'
    end
    @options[:output] = File.expand_path(@options[:output])
  end

  def init_entrance
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
      STDERR.puts "-> Project root not supplied, #{@root} assumed."
    end
  end

  def init_tmpdir
    @options[:tmpdir] ||= File.expand_path("rubyc", Dir.tmpdir)
    @options[:tmpdir] = File.expand_path(@options[:tmpdir])
    if @root && @options[:tmpdir].include?(@root)
      raise Error, "Tempdir #{@options[:tmpdir]} cannot reside inside #{@root}."
    end
  end
  
  def stuff_zlib
    target = File.join(@options[:tmpdir], 'zlib')
    unless Dir.exist?(target)
      Utils.cp_r(File.join(PRJ_ROOT, 'vendor', 'zlib'), target, preserve: true)
      Utils.chdir(target) do
        if Gem.win_platform?
          Utils.run(@compile_env, 'nmake /f win32\\Makefile.msc')
        else
          Utils.run(@compile_env, './configure --static')
          Utils.run(@compile_env, "make #{@options[:make_args]}")
        end
        Dir['*.{dylib,so,dll}'].each do |thisdl|
          Utils.rm_f(thisdl)
        end
      end
    end
  end

  def stuff_openssl
    target = File.join(@options[:tmpdir], 'openssl')
    unless Dir.exist?(target)
      Utils.cp_r(File.join(PRJ_ROOT, 'vendor', 'openssl'), target, preserve: true)
      Utils.chdir(target) do
        if Gem.win_platform?
          # TODO
        else
          Utils.run(@compile_env, './config')
          Utils.run(@compile_env, "make #{@options[:make_args]}")
        end
        Dir['*.{dylib,so,dll}'].each do |thisdl|
          Utils.rm_f(thisdl)
        end
      end
    end
  end
  
  def stuff_gdbm
    target = File.join(@options[:tmpdir], 'gdbm')
    unless Dir.exist?(target)
      Utils.cp_r(File.join(PRJ_ROOT, 'vendor', 'gdbm'), target, preserve: true)
      Utils.chdir(target) do
        if Gem.win_platform?
          # TODO
        else
          Utils.run(@compile_env, "./configure --with-pic --enable-libgdbm-compat --disable-shared --enable-static --without-readline --prefix=#{Utils.escape File.join(@options[:tmpdir], 'gdbm', 'build')}")
          Utils.run(@compile_env, "make #{@options[:make_args]}")
          Utils.run(@compile_env, "make install")
        end
      end
    end
  end
  
  def stuff_yaml
    target = File.join(@options[:tmpdir], 'yaml')
    unless Dir.exist?(target)
      Utils.cp_r(File.join(PRJ_ROOT, 'vendor', 'yaml'), target, preserve: true)
      File.utime(Time.at(0), Time.at(0), File.join(target, 'configure.ac'))
      File.utime(Time.at(0), Time.at(0), File.join(target, 'aclocal.m4'))
      Utils.chdir(target) do
        if Gem.win_platform?
          # TODO
        else
          Utils.run(@compile_env, "./configure --with-pic --disable-shared --enable-static --prefix=#{Utils.escape File.join(@options[:tmpdir], 'yaml', 'build')}")
          Utils.run(@compile_env, "make #{@options[:make_args]}")
          Utils.run(@compile_env, "make install")
        end
      end
    end
  end
  
  def stuff_tmpdir
    Utils.rm_rf(@options[:tmpdir]) if @options[:clean_tmpdir]
    Utils.mkdir_p(@options[:tmpdir])

    stuff_zlib
    stuff_openssl
    stuff_gdbm
    stuff_yaml

    target = File.join(@options[:tmpdir], 'ruby')
    @ruby_build = File.join(@options[:tmpdir], 'ruby', 'build')
    unless Dir.exist?(target)
      Utils.cp_r(File.join(PRJ_ROOT, 'ruby'), target, preserve: true)

      # PATCH common.mk
      target = File.join(@options[:tmpdir], 'ruby', 'common.mk')
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
        target = File.join(@options[:tmpdir], 'ruby', 'win32', 'Makefile.sub')
        target_content = File.read(target)
        found = false
        File.open(target, 'w') do |f|
          target_content.each_line do |line|
            if !found && (line =~ /^LDFLAGS = (.*)$/)
              found = true
              f.puts "LDFLAGS = #{$1} #{@ldflags}"
            else
              f.print line
            end
          end
        end
        raise "Failed to patch LDFLAGS of #{target}" unless found
      end
    end

    @vendor_ruby = File.join(@options[:tmpdir], 'ruby')
    if Gem.win_platform?
      # TODO make those win32 ext work
      Utils.chdir(@vendor_ruby) do
        Utils.chdir('ext') do
          Utils.rm_rf('dbm')
          Utils.rm_rf('digest')
          Utils.rm_rf('etc')
          Utils.rm_rf('fiddle')
          Utils.rm_rf('gdbm')
          Utils.rm_rf('mathn')
          Utils.rm_rf('openssl')
          Utils.rm_rf('pty')
          Utils.rm_rf('readline')
          Utils.rm_rf('ripper')
          Utils.rm_rf('socket')
          Utils.rm_rf('win32')
          Utils.rm_rf('win32ole')
        end
      end
    end
  end

  def run!
    Utils.chdir(@vendor_ruby) do
      sep = Gem.win_platform? ? ';' : ':'
      if Gem.win_platform?
        unless File.exist?(@ruby_build)
          @compile_env['ENCLOSE_IO_RUBYC_1ST_PASS'] = '1'
          @compile_env['ENCLOSE_IO_RUBYC_2ND_PASS'] = nil
          # enclose_io_memfs.o - 1st pass
          Utils.run(@compile_env, "call win32\\configure.bat \
                                  --prefix=#{Utils.escape @ruby_build} \
                                  --enable-bundled-libyaml \
                                  --enable-debug-env \
                                  --disable-install-doc \
                                  --with-static-linked-ext")
          Utils.run(@compile_env, "nmake #{@options[:nmake_args]}")
          Utils.run(@compile_env, %Q{nmake #{@options[:nmake_args]} -f enc.mk V="0" UNICODE_HDR_DIR="./enc/unicode/9.0.0"  RUBY=".\\miniruby.exe -I./lib -I. " MINIRUBY=".\\miniruby.exe -I./lib -I. " -l libenc})
          Utils.run(@compile_env, %Q{nmake #{@options[:nmake_args]} -f enc.mk V="0" UNICODE_HDR_DIR="./enc/unicode/9.0.0"  RUBY=".\\miniruby.exe -I./lib -I. " MINIRUBY=".\\miniruby.exe -I./lib -I. " -l libtrans})
          Utils.run(@compile_env, "nmake #{@options[:nmake_args]}")
          Utils.run(@compile_env, "nmake install")
          File.open(File.join(@options[:tmpdir], 'ruby', 'ext', 'Setup'), 'w') do |f|
            f.puts 'option nodynamic'
          end
        end
        # enclose_io_memfs.o - 2nd pass
        prepare_work_dir
        prepare_local if @entrance
        Utils.rm_f('verconf.h')
        Utils.rm_f('rbconfig.rb')
        Utils.rm_f('.rbconfig.time')
        Utils.rm_f('dir.obj')
        Utils.rm_f('file.obj')
        Utils.rm_f('io.obj')
        Utils.rm_f('main.obj')
        Utils.rm_f('win32/file.obj')
        Utils.rm_f('win32/win32.obj')
        Utils.rm_f('ruby.exe')
        Utils.rm_f('include/enclose_io.h')
        Utils.rm_f('enclose_io_memfs.c')
        make_enclose_io_memfs
        make_enclose_io_vars
        @compile_env['CFLAGS'] += ' -DENCLOSE_IO_RUBYC_2ND_PASS '
        @compile_env['ENCLOSE_IO_RUBYC_1ST_PASS'] = nil
        @compile_env['ENCLOSE_IO_RUBYC_2ND_PASS'] = '1'
        Utils.run(@compile_env, "nmake #{@options[:nmake_args]}")
        Utils.cp('ruby.exe', @options[:output])
      else
        unless File.exist?(@ruby_build)
          @compile_env['ENCLOSE_IO_RUBYC_1ST_PASS'] = '1'
          @compile_env['ENCLOSE_IO_RUBYC_2ND_PASS'] = nil
          # enclose_io_memfs.o - 1st pass
          Utils.run(@compile_env, "./configure \
                                  --prefix=#{Utils.escape @ruby_build} \
                                  --enable-bundled-libyaml \
                                  --without-gmp \
                                  --disable-dtrace \
                                  --enable-debug-env \
                                  --disable-install-rdoc")
          Utils.run(@compile_env, "make #{@options[:make_args]} -j1")
          Utils.run(@compile_env, "make install")
          File.open(File.join(@options[:tmpdir], 'ruby', 'ext', 'Setup'), 'w') do |f|
            f.puts 'option nodynamic'
          end
        end
        # enclose_io_memfs.o - 2nd pass
        prepare_work_dir
        prepare_local if @entrance
        Utils.rm_f('verconf.h')
        Utils.rm_f('rbconfig.rb')
        Utils.rm_f('.rbconfig.time')
        Utils.rm_f('dir.o')
        Utils.rm_f('file.o')
        Utils.rm_f('io.o')
        Utils.rm_f('main.o')
        Utils.rm_f('ruby')
        Utils.rm_f('include/enclose_io.h')
        Utils.rm_f('enclose_io_memfs.c')
        @compile_env['CFLAGS'] += ' -DENCLOSE_IO_RUBYC_2ND_PASS '
        @compile_env['ENCLOSE_IO_RUBYC_1ST_PASS'] = nil
        @compile_env['ENCLOSE_IO_RUBYC_2ND_PASS'] = '1'
        Utils.run(@compile_env, "./configure \
                                --prefix=#{Utils.escape @ruby_build} \
                                --enable-bundled-libyaml \
                                --without-gmp \
                                --disable-dtrace \
                                --enable-debug-env \
                                --disable-install-rdoc \
                                --with-static-linked-ext")
        make_enclose_io_memfs
        make_enclose_io_vars
        Utils.run(@compile_env, "make #{@options[:make_args]}")
        Utils.cp('ruby', @options[:output])
      end
    end
  end

  def prepare_work_dir
    # Prepare /__enclose_io_memfs__
    @work_dir = File.join(@options[:tmpdir], '__work_dir__')
    unless @options[:keep_tmpdir]
      Utils.rm_rf(@work_dir)
      Utils.mkdir_p(@work_dir)
    end
    
    @work_dir_inner = File.join(@work_dir, '__enclose_io_memfs__')
    
    unless @options[:keep_tmpdir]
      Utils.cp_r(@ruby_build, @work_dir_inner, preserve: true)

      Dir["#{@work_dir_inner}/**/*.{a,dylib,so,dll,lib,bundle}"].each do |thisdl|
        Utils.rm_f(thisdl)
      end
    end

    @gems_dir = File.join(@work_dir_inner, "lib/ruby/gems/#{self.class.ruby_api_version}")
    
    @path_env = "#{File.join(@options[:tmpdir], 'ruby', 'build', 'bin')}:#{ENV['PATH']}"
    @local_toolchain = {
      'PATH' => @path_env,
      'GEM_HOME' => nil,
      'GEM_PATH' => nil,
      'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1',
      'ENCLOSE_IO_RUBYC_1ST_PASS' => '1',
      'ENCLOSE_IO_RUBYC_2ND_PASS' => nil,
    }
  end

  def prepare_local
    # Prepare /__enclose_io_memfs__/local
    Utils.chdir(@root) do
      gemspecs = Dir['./*.gemspec']
      gemfiles = Dir['./Gemfile']
      the_bundler_gem = File.join(PRJ_ROOT, 'vendor', 'bundler-1.15.1.gem')
      if gemspecs.size > 0
        raise 'Multiple gemspecs detected' unless 1 == gemspecs.size
        @pre_prepare_dir = File.join(@options[:tmpdir], '__pre_prepare__')
        Utils.rm_rf(@pre_prepare_dir)
        Utils.cp_r(@root, @pre_prepare_dir)
        Utils.chdir(@pre_prepare_dir) do
          STDERR.puts "-> Detected a gemspec, trying to build the gem"
          Utils.rm_f('./*.gem')
          if gemfiles.size > 0
            Utils.run(@local_toolchain, 'sh', '-c', "gem install #{Utils.escape the_bundler_gem} --force --local --no-rdoc --no-ri")
            Utils.run(@local_toolchain, 'sh', '-c', "gem install #{Utils.escape the_bundler_gem} --force --local --no-rdoc --no-ri --install-dir #{Utils.escape @gems_dir}")
            Utils.run(@local_toolchain, 'sh', '-c', "bundle install")
            Utils.run(@local_toolchain, 'sh', '-c', "bundle exec gem build #{Utils.escape gemspecs.first}")
          else
            Utils.run(@local_toolchain, 'sh', '-c', "gem build #{Utils.escape gemspecs.first}")
          end
          gems = Dir['./*.gem']
          raise 'gem building failed' unless 1 == gems.size
          the_gem = gems.first
          Utils.run(@local_toolchain, 'sh', '-c', "gem install #{Utils.escape the_gem} --force --local --no-rdoc --no-ri --install-dir #{Utils.escape @gems_dir}")
          if File.exist?(File.join(@gems_dir, "bin/#{@entrance}"))
            @memfs_entrance = "#{MEMFS}/_gems_/bin/#{@entrance}"
          else
            Utils.chdir(File.join(@gems_dir, "bin")) do
              raise Error, "Cannot find entrance #{@entrance}, available entrances are #{ Dir['*'].join(', ') }."
            end
          end
        end
      elsif gemfiles.size > 0
        raise 'Multiple Gemfiles detected' unless 1 == gemfiles.size
        # gem install bundler
        Utils.run(@local_toolchain, 'sh', '-c', "gem install #{Utils.escape the_bundler_gem} --force --local --no-rdoc --no-ri")
        Utils.run(@local_toolchain, 'sh', '-c', "gem install #{Utils.escape the_bundler_gem} --force --local --no-rdoc --no-ri --install-dir #{Utils.escape @gems_dir}")
        # bundle install
        @work_dir_local = File.join(@work_dir_inner, 'local')
        @env_bundle_gemfile = '/__enclose_io_memfs__/local/Gemfile'
        unless @options[:keep_tmpdir]
          Utils.cp_r(@root, @work_dir_local)
        end
        Utils.chdir(@work_dir_local) do
          Utils.run(@local_toolchain, 'sh', '-c', 'bundle install --deployment')
          if File.exist?(@entrance)
            @memfs_entrance = mempath(@entrance)
          else
            if File.exist?("bin/#{@entrance}")
              @memfs_entrance = "#{MEMFS}/local/bin/#{@entrance}"
            else
              Utils.run(@local_toolchain, 'sh', '-c', 'bundle install --deployment --binstubs')
              if File.exist?("bin/#{@entrance}")
                @memfs_entrance = "#{MEMFS}/local/bin/#{@entrance}"
              else
                Utils.chdir('bin') do
                  raise Error, "Cannot find entrance #{@entrance}, available entrances are #{ Dir['*'].join(', ') }."
                end
              end
            end
          end
        end
      else
        @work_dir_local = File.join(@work_dir_inner, 'local')
        Utils.cp_r(@root, @work_dir_local)
        Utils.chdir(@work_dir_local) do
          x = Pathname.new @entrance
          y = Pathname.new @root
          if x.absolute?
            raise "Entrance #{@entrance} is not in the project root #{@root}" unless @entrance.include?(@root)
            @entrance = x.relative_path_from y
          end
          if File.exist?("#{@entrance}")
            @memfs_entrance = "#{MEMFS}/local/#{@entrance}"
          else
            Utils.chdir('bin') do
              raise Error, "Cannot find entrance #{@entrance}"
            end
          end
        end
      end
      
      if @work_dir_local
        Utils.chdir(@work_dir_local) do
          if Dir.exist?('.git')
            STDERR.puts `git status`
            Utils.rm_rf('.git')
          end
        end
      end
      
      Utils.rm_rf(File.join(@gems_dir, 'cache'))
    end
  end

  def make_enclose_io_memfs
    Utils.chdir(@vendor_ruby) do
      Utils.rm_f('enclose_io_memfs.squashfs')
      Utils.rm_f('enclose_io_memfs.c')
      Utils.run("mksquashfs -version")
      Utils.run("mksquashfs #{Utils.escape @work_dir} enclose_io_memfs.squashfs")
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
    Utils.chdir(@vendor_ruby) do
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
end
