# Copyright (c) 2017 Minqi Pan <pmq2001@gmail.com>
# 
# This file is part of Ruby Compiler, distributed under the MIT License
# For full terms see the included LICENSE file

require "compiler/constants"
require "compiler/error"
require "compiler/utils"
require "compiler/deps"
require 'shellwords'
require 'tmpdir'
require 'fileutils'
require 'json'
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
    if version_info =~ /RUBY_VERSION\s+"([^"]+)"$/
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
    @entrance = entrance
    @options = options

    if RbConfig::CONFIG['host_os'] =~ /darwin|mac os/i
      @extra_cflags = ' -mmacosx-version-min=10.7 '
      @extra_envvar = { 'CFLAGS' => @extra_cflags }
    else
      @extra_cflags = ''
      @extra_envvar = { }
    end

    check_base_ruby_version!

    init_options
    init_entrance
    init_tmpdir

    init_gmp
    init_zlib
    init_yaml
    init_openssl
    init_libsquash
    init_gdbm

    init_ruby
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
      @root = Dir.pwd
    end
  end

  def init_tmpdir
    @options[:tmpdir] ||= File.expand_path("rubyc", Dir.tmpdir)
    @options[:tmpdir] = File.expand_path(@options[:tmpdir])
    if @options[:tmpdir].include? @root
      raise Error, "Tempdir #{@options[:tmpdir]} cannot reside inside #{@root}."
    end

    Utils.prepare_tmpdir(@options[:tmpdir])
    @vendor_ruby = File.join(@options[:tmpdir], 'ruby')
    @vendor_bundler = File.join(@options[:tmpdir], 'bundler-1.13.7.gem')
  end

  def init_ruby
    @vendor_ruby_build_dir = File.join(@vendor_ruby, 'build')
    Utils.mkdir_p(@vendor_ruby_build_dir)
    raise "#{@vendor_ruby_build_dir} does not exist" unless Dir.exist?(@vendor_ruby_build_dir)
  end
  
  def check_base_ruby_version!
    expectation = "ruby #{self.class.ruby_version}"
    got = `ruby -v`
    unless got.include?(expectation)
      msg  = "Please make sure to have installed the correct version of ruby in your environment\n"
      msg += "Expecting #{expectation}; yet got #{got}"
      raise Error, msg
    end
  end
  
  def run!
    prepare! unless prepared?
    raise 'Unable to prepare for Ruby compilations' unless prepared?
    Utils.chdir(@vendor_ruby) do
      sep = Gem.win_platform? ? ';' : ':'
      @compile_env = {
                      'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1',
                      'PKG_CONFIG_PATH' => "#{File.join @vendor_openssl_build_dir, 'lib/pkgconfig'}#{sep}#{File.join @vendor_yaml_build_dir, 'lib/pkgconfig'}#{sep}#{File.join @vendor_zlib_build_dir, 'lib/pkgconfig'}",
                      'LDFLAGS' => "-L.",
                      'CFLAGS' => "-I#{Utils.escape @vendor_squash_include_dir} -Ienclose_io #{@extra_cflags}",
                    }

      # enclose_io/enclose_io_memfs.o - 1st pass
      Utils.rm_rf('enclose_io/')
      Utils.cp_r(@vendor_squash_sample_dir, 'enclose_io/')
      Utils.run("cc #{@compile_env['CFLAGS']} -c enclose_io/enclose_io_memfs.c -o enclose_io/enclose_io_memfs.o")
      raise 'enclose_io/enclose_io_memfs error' unless File.exist?('enclose_io/enclose_io_memfs.o')
      Utils.run("cc #{@compile_env['CFLAGS']} -c enclose_io/enclose_io_unix.c -o enclose_io/enclose_io_unix.o")
      raise 'enclose_io/enclose_io_unix error' unless File.exist?('enclose_io/enclose_io_unix.o')
      Utils.run("cc #{@compile_env['CFLAGS']} -c enclose_io/enclose_io_win32.c -o enclose_io/enclose_io_win32.o")
      raise 'enclose_io/enclose_io_win32 error' unless File.exist?('enclose_io/enclose_io_win32.o')
      if Gem.win_platform?
        Utils.run(@compile_env, "call win32\\configure.bat                                              \
                                --prefix=#{Utils.escape @vendor_ruby_build_dir}              \
                                --enable-debug-env \
                                --disable-install-doc                                             \
                                --with-static-linked-ext                                          \
                                --with-gmp-dir=#{Utils.escape @vendor_gmp_build_dir}             \
                                --with-zlib-dir=#{Utils.escape @vendor_zlib_build_dir}             \
                                --with-libyaml-dir=#{Utils.escape @vendor_yaml_build_dir}             \
                                --with-openssl-dir=#{Utils.escape @vendor_openssl_build_dir}")
        Utils.run(@compile_env, "nmake #{@options[:nmake_args]}")
        Utils.rm('ruby.exe')
        Utils.rm_rf('enclose_io/')
        Utils.rm('main.o')
        # enclose_io/enclose_io_memfs.o - 2nd pass
        bundle_deploy
        make_enclose_io_memfs
        make_enclose_io_vars
        Utils.run(@compile_env, "nmake #{@options[:nmake_args]}")
        Utils.cp('ruby.exe', @options[:output])
      else
        Utils.run(@compile_env, "./configure                                                           \
                               --prefix=#{Utils.escape @vendor_ruby_build_dir}              \
                               --with-out-ext=bigdecimal \
                               --enable-debug-env \
                               --with-sitearchdir=no \
                               --with-vendordir=no \
                               --disable-install-rdoc                                            \
                               --with-static-linked-ext                                          \
                               --with-gmp-dir=#{Utils.escape @vendor_gmp_build_dir}             \
                               --with-zlib-dir=#{Utils.escape @vendor_zlib_build_dir}             \
                               --with-libyaml-dir=#{Utils.escape @vendor_yaml_build_dir}             \
                               --with-openssl-dir=#{Utils.escape @vendor_openssl_build_dir} ")
        Utils.run(@compile_env, "make #{@options[:make_args]}")
        Utils.rm('ruby')
        Utils.rm_rf('enclose_io/')
        Utils.rm('main.o')
        # enclose_io/enclose_io_memfs.o - 2nd pass
        bundle_deploy
        make_enclose_io_memfs
        make_enclose_io_vars
        Utils.run(@compile_env, "make #{@options[:make_args]}")
        Utils.cp('ruby', @options[:output])
      end
    end
  end
  
  def prepared?
    ret = false
    Utils.chdir(@vendor_ruby) do
      if Gem.win_platform?
        # TODO
      else
        ret = %w{
          libcrypto.a
          libgmp.a
          libssl.a
          libyaml.a
          libz.a
          libsquash.a
          libgdbm.a
        }.map { |x| File.exist?(x) }.reduce(true) { |m,o| m && o }
      end
    end
    ret
  end

  def prepare!
    compile_gmp
    compile_zlib
    compile_yaml
    compile_openssl
    compile_libsquash
    compile_gdbm
  end

  def bundle_deploy
    @work_dir = File.join(@options[:tmpdir], '__work_dir__')
    Utils.rm_rf(@work_dir)
    Utils.mkdir_p(@work_dir)
    
    @work_dir_inner = File.join(@work_dir, '__enclose_io_memfs__')
    Utils.mkdir_p(@work_dir_inner)

    @work_dir_global = File.join(@work_dir_inner, '_global_')
    src_ruby_lib = File.join(@vendor_ruby, 'lib')
    Utils.mkdir_p(File.join(@work_dir_global, 'lib/ruby'))
    dst_ruby_lib = File.join(@work_dir_global, "lib/ruby/#{self.class.ruby_api_version}")
    raise 'logic error' unless Dir.exist?(src_ruby_lib)
    Utils.cp_r(src_ruby_lib, dst_ruby_lib)
    
    src_ruby_extlib = File.join(@vendor_ruby, 'ext')
    Utils.chdir(src_ruby_extlib) do
      Dir['*/lib'].each do |libpath|
        Utils.chdir(libpath) do
          Dir["**/*.rb"].each do |path|
            dst = File.expand_path(File.dirname(path), dst_ruby_lib)
            FileUtils.mkdir_p(dst)
            FileUtils.cp(path, dst)
          end
        end
      end
    end
    
    src_rbconfig = File.join(@vendor_ruby, 'rbconfig.rb')
    Utils.cp(src_rbconfig, dst_ruby_lib)

    @gems_dir = File.join(@work_dir_inner, '_gems_')
    Utils.rm_rf(@gems_dir)
    Utils.mkdir_p(@gems_dir)
    Dir["#{@vendor_ruby}/gems/*.gem"].each do |the_gem|
      Utils.run("gem install #{Utils.escape the_gem} --force --local --no-rdoc --no-ri --install-dir #{Utils.escape @gems_dir}")
    end
    dst = File.join(@gems_dir, 'specifications/default')
    FileUtils.mkdir_p(dst)
    Dir["#{@vendor_ruby}/ext/**/*.gemspec"].each do |the_gemspec|
      Utils.cp(the_gemspec, dst)
    end

    Utils.chdir(@root) do
      gemspecs = Dir['./*.gemspec']
      gemfiles = Dir['./Gemfile']
      if gemspecs.size > 0
        raise 'Multiple gemspecs detected' unless 1 == gemspecs.size
        @pre_prepare_dir = File.join(@options[:tmpdir], '__pre_prepare__')
        Utils.rm_rf(@pre_prepare_dir)
        Utils.cp_r(@root, @pre_prepare_dir)
        Utils.chdir(@pre_prepare_dir) do
          STDERR.puts "-> Detected a gemspec, trying to build the gem"
          Utils.rm_f('./*.gem')
          Utils.run("bundle")
          Utils.run("bundle exec gem build #{Utils.escape gemspecs.first}")
          gems = Dir['./*.gem']
          raise 'gem building failed' unless 1 == gems.size
          the_gem = gems.first
          Utils.run("gem install #{Utils.escape the_gem} --force --local --no-rdoc --no-ri --install-dir #{Utils.escape @gems_dir}")
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
        @work_dir_local = File.join(@work_dir_inner, '_local_')
        @chdir_at_startup = '/__enclose_io_memfs__/_local_'
        Utils.run("gem install #{Utils.escape @vendor_bundler} --force --local --no-rdoc --no-ri --install-dir #{Utils.escape @gems_dir}")
        Utils.cp_r(@root, @work_dir_local)
        Utils.chdir(@work_dir_local) do
          Utils.run('bundle install --deployment')
          if File.exist?("bin/#{@entrance}")
            @memfs_entrance = "#{MEMFS}/_local_/bin/#{@entrance}"
          else
            Utils.run('bundle install --deployment --binstubs')
            if File.exist?("bin/#{@entrance}")
              @memfs_entrance = "#{MEMFS}/_local_/bin/#{@entrance}"
            else
              Utils.chdir('bin') do
                raise Error, "Cannot find entrance #{@entrance}, available entrances are #{ Dir['*'].join(', ') }."
              end
            end
          end
        end
      else
        @work_dir_local = File.join(@work_dir_inner, '_local_')
        @chdir_at_startup = '/__enclose_io_memfs__/_local_'
        Utils.cp_r(@root, @work_dir_local)
        Utils.chdir(@work_dir_local) do
          x = Pathname.new @entrance
          y = Pathname.new @root
          if x.absolute?
            raise 'Entrance is not in the project root' unless @entrance.include?(@root)
            @entrance = x.relative_path_from y
          end
          if File.exist?("#{@entrance}")
            @memfs_entrance = "#{MEMFS}/_local_/#{@entrance}"
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
      Utils.cp_r(@vendor_squash_sample_dir, 'enclose_io/')
      Utils.rm_f('enclose_io/enclose_io_memfs.squashfs')
      Utils.rm_f('enclose_io/enclose_io_memfs.c')
      Utils.run("mksquashfs -version")
      Utils.run("mksquashfs #{Utils.escape @work_dir} enclose_io/enclose_io_memfs.squashfs")
      bytes = IO.binread('enclose_io/enclose_io_memfs.squashfs').bytes
      # TODO slow operation
      # remember to change vendor/libsquash/sample/enclose_io_memfs.c as well
      File.open("enclose_io/enclose_io_memfs.c", "w") do |f|
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
      Utils.run("cc #{@compile_env['CFLAGS']} -c enclose_io/enclose_io_unix.c -o enclose_io/enclose_io_unix.o")
      raise 'enclose_io/enclose_io_unix error' unless File.exist?('enclose_io/enclose_io_unix.o')
      Utils.run("cc #{@compile_env['CFLAGS']} -c enclose_io/enclose_io_win32.c -o enclose_io/enclose_io_win32.o")
      raise 'enclose_io/enclose_io_win32 error' unless File.exist?('enclose_io/enclose_io_win32.o')
      # TODO slow operation
      Utils.run("cc #{@compile_env['CFLAGS']} -c enclose_io/enclose_io_memfs.c -o enclose_io/enclose_io_memfs.o")
      raise 'cannot generate enclose_io/enclose_io_memfs.o' unless File.exist?('enclose_io/enclose_io_memfs.o')
    end
  end

  def make_enclose_io_vars
    Utils.chdir(@vendor_ruby) do
      File.open("enclose_io/enclose_io.h", "w") do |f|
        # remember to change vendor/libsquash/sample/enclose_io.h as well
        # might need to remove some object files at the 2nd pass  
        f.puts '#ifndef ENCLOSE_IO_H_999BC1DA'
        f.puts '#define ENCLOSE_IO_H_999BC1DA'
        f.puts ''
        f.puts '#include "enclose_io_prelude.h"'
        f.puts '#include "enclose_io_common.h"'
        f.puts '#include "enclose_io_win32.h"'
        f.puts '#include "enclose_io_unix.h"'
        f.puts ''
        f.puts "#define ENCLOSE_IO_CHDIR_AT_STARTUP #{@chdir_at_startup.inspect}" if @chdir_at_startup
        f.puts "#define ENCLOSE_IO_ENTRANCE #{@memfs_entrance.inspect}"
        f.puts '#endif'
        f.puts ''
      end
    end
  end
end
