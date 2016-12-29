# Copyright (c) 2016-2017 Minqi Pan
# 
# This file is part of Ruby Compiler, distributed under the MIT License
# For full terms see the included LICENSE file

require "ruby/compiler/constants"
require "ruby/compiler/error"
require "ruby/compiler/utils"
require "ruby/compiler/deps"
require "ruby/compiler/test"
require 'shellwords'
require 'tmpdir'
require 'fileutils'
require 'json'
require 'open3'
require 'bundler'

module Ruby
  class Compiler
    def self.ruby_api_version
      @ruby_api_version ||= peek_ruby_api_version
    end
    
    def self.ruby_version
      @ruby_version ||= peek_ruby_version
    end
    
    def self.peek_ruby_version
      version_info = File.read(File.join(VENDOR_DIR, 'ruby/version.h'))
      if version_info =~ /RUBY_VERSION\s+"([^"]+)"$/
        return $1.dup
      else
        raise 'Cannot peek RUBY_VERSION'
      end
    end
    
    def self.peek_ruby_api_version
      version_info = File.read(File.join(VENDOR_DIR, 'ruby/include/ruby/version.h'))
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

      check_base_ruby_version!

      init_options
      init_entrance
      init_tmpdir

      init_gmp
      init_zlib
      init_yaml
      init_openssl
      init_squash
      init_gdbm

      init_ruby
    end

    def init_options
      if Gem.win_platform?
        @options[:output] ||= 'a.exe'
      else
        @options[:output] ||= 'a.out'
      end
      @options[:output] = File.expand_path(@options[:output])
    end

    def init_entrance
      if @options[:project_root]
        @project_root = File.expand_path(@options[:project_root])
      else
        @project_root = Dir.pwd
      end
      unless File.exist?(File.expand_path('./Gemfile', @project_root))
        raise Error, "Cannot find a Gemfile at the project root #{@project_root}"
      end
    end

    def init_tmpdir
      @options[:tmpdir] ||= File.expand_path("rubyc", Dir.tmpdir)
      @options[:tmpdir] = File.expand_path(@options[:tmpdir])
      if @options[:tmpdir].include? @project_root
        raise Error, "Tempdir #{@options[:tmpdir]} cannot reside inside the project root #{@project_root}."
      end

      Utils.prepare_tmpdir(@options[:tmpdir])
      @vendor_ruby = File.join(@options[:tmpdir], 'ruby')
      @vendor_bundler = File.join(@options[:tmpdir], 'bundler-1.13.7.gem')
    end

    def init_ruby
      @vendor_ruby_build_dir = File.join(@vendor_ruby, 'build')
      STDERR.puts "-> FileUtils.mkdir_p #{@vendor_ruby_build_dir}"
      FileUtils.mkdir_p(@vendor_ruby_build_dir)
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
        compile_env = {
                        'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1',
                        'PKG_CONFIG_PATH' => "#{File.join @vendor_openssl_build_dir, 'lib/pkgconfig'}#{sep}#{File.join @vendor_yaml_build_dir, 'lib/pkgconfig'}#{sep}#{File.join @vendor_zlib_build_dir, 'lib/pkgconfig'}",
                        'LDFLAGS' => "-L.",
                        'CFLAGS' => "-I#{Shellwords.escape @vendor_squash_include_dir}",
                      }
        # enclose_io/memfs.o - 1st pass
        STDERR.puts "-> FileUtils.cp(#{File.join(VENDOR_DIR, 'ruby/enclose_io/memfs.c')}, #{'enclose_io/memfs.c'})"
        FileUtils.cp(File.join(VENDOR_DIR, 'ruby/enclose_io/memfs.c'), 'enclose_io/memfs.c')
        STDERR.puts "-> FileUtils.cp(#{File.join(VENDOR_DIR, 'ruby/include/enclose_io.h')}, #{'include/enclose_io.h'})"
        FileUtils.cp(File.join(VENDOR_DIR, 'ruby/include/enclose_io.h'), 'include/enclose_io.h')
        Utils.run("cc #{compile_env['CFLAGS']} -c enclose_io/memfs.c -o enclose_io/memfs.o")
        Utils.run("cc #{compile_env['CFLAGS']} -Iinclude -c enclose_io/intercept.c -o enclose_io/intercept.o")
        raise 'enclose_io/memfs error' unless File.exist?('enclose_io/memfs.o')
        raise 'enclose_io/intercept error' unless File.exist?('enclose_io/intercept.o')
        if Gem.win_platform?
          Utils.run(compile_env, "call win32\\configure.bat                                              \
                                  --prefix=#{Shellwords.escape @vendor_ruby_build_dir}              \
                                  --enable-debug-env \
                                  --disable-install-doc                                             \
                                  --with-static-linked-ext                                          \
                                  --with-gmp-dir=#{Shellwords.escape @vendor_gmp_build_dir}             \
                                  --with-zlib-dir=#{Shellwords.escape @vendor_zlib_build_dir}             \
                                  --with-libyaml-dir=#{Shellwords.escape @vendor_yaml_build_dir}             \
                                  --with-openssl-dir=#{Shellwords.escape @vendor_openssl_build_dir}")
          Utils.run(compile_env, "nmake #{@options[:nmake_args]}")
          STDERR.puts "-> FileUtils.rm('ruby.exe')"
          FileUtils.rm('ruby.exe')
          STDERR.puts "-> FileUtils.rm('enclose_io/memfs.o')"
          FileUtils.rm('enclose_io/memfs.o')
          STDERR.puts "-> FileUtils.rm('main.o')"
          FileUtils.rm('main.o')
          # enclose_io/memfs.o - 2nd pass
          bundle_deploy
          make_enclose_io_memfs
          make_enclose_io_vars
          Utils.run(compile_env, "nmake #{@options[:nmake_args]}")
          STDERR.puts "-> FileUtils.cp('ruby.exe', #{@options[:output]})"
          FileUtils.cp('ruby.exe', @options[:output])
        else
          Utils.run(compile_env, "./configure                                                           \
                                 --prefix=#{Shellwords.escape @vendor_ruby_build_dir}              \
                                 --with-out-ext=bigdecimal \
                                 --enable-debug-env \
                                 --with-sitearchdir=no \
                                 --with-vendordir=no \
                                 --disable-install-rdoc                                            \
                                 --with-static-linked-ext                                          \
                                 --with-gmp-dir=#{Shellwords.escape @vendor_gmp_build_dir}             \
                                 --with-zlib-dir=#{Shellwords.escape @vendor_zlib_build_dir}             \
                                 --with-libyaml-dir=#{Shellwords.escape @vendor_yaml_build_dir}             \
                                 --with-openssl-dir=#{Shellwords.escape @vendor_openssl_build_dir} ")
          Utils.run(compile_env, "make #{@options[:make_args]}")
          STDERR.puts "-> FileUtils.rm('ruby')"
          FileUtils.rm('ruby')
          STDERR.puts "-> FileUtils.rm('enclose_io/memfs.o')"
          FileUtils.rm('enclose_io/memfs.o')
          STDERR.puts "-> FileUtils.rm('main.o')"
          FileUtils.rm('main.o')
          # enclose_io/memfs.o - 2nd pass
          bundle_deploy
          make_enclose_io_memfs
          make_enclose_io_vars
          Utils.run(compile_env, "make #{@options[:make_args]}")
          STDERR.puts "-> FileUtils.cp('ruby', #{@options[:output]})"
          FileUtils.cp('ruby', @options[:output])
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
      compile_squash
      compile_gdbm
    end

    def bundle_deploy
      Bundler.with_clean_env do
        @work_dir = File.join(@options[:tmpdir], '__work_dir__')
        STDERR.puts "-> FileUtils.rm_rf(#{@work_dir})"
        FileUtils.rm_rf(@work_dir)
        STDERR.puts "-> FileUtils.mkdir_p #{@work_dir}"
        FileUtils.mkdir_p(@work_dir)
        
        @work_dir_inner = File.join(@work_dir, '__enclose_io_memfs__')
        STDERR.puts "-> FileUtils.mkdir_p #{@work_dir_inner}"
        FileUtils.mkdir_p(@work_dir_inner)

        @work_dir_global = File.join(@work_dir_inner, '_global_')
        src_ruby_lib = File.join(@vendor_ruby, 'lib')
        STDERR.puts "-> FileUtils.mkdir_p(File.join(#{@work_dir_global}, 'lib/ruby'))"
        FileUtils.mkdir_p(File.join(@work_dir_global, 'lib/ruby'))
        dst_ruby_lib = File.join(@work_dir_global, "lib/ruby/#{self.class.ruby_api_version}")
        raise 'logic error' unless Dir.exist?(src_ruby_lib)
        STDERR.puts "-> FileUtils.cp_r(#{src_ruby_lib}, #{dst_ruby_lib})"
        FileUtils.cp_r(src_ruby_lib, dst_ruby_lib)
        
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
        STDERR.puts "-> FileUtils.cp(#{src_rbconfig}, #{dst_ruby_lib})"
        FileUtils.cp(src_rbconfig, dst_ruby_lib)

        @gems_dir = File.join(@work_dir_inner, '_gems_')
        STDERR.puts "-> FileUtils.rm_rf(#{@gems_dir})"
        FileUtils.rm_rf(@gems_dir)
        STDERR.puts "-> FileUtils.mkdir_p #{@gems_dir}"
        FileUtils.mkdir_p(@gems_dir)
        Dir["#{@vendor_ruby}/gems/*.gem"].each do |the_gem|
          Utils.run("gem install #{Shellwords.escape the_gem} --force --local --no-rdoc --no-ri --install-dir #{Shellwords.escape @gems_dir}")
        end
        dst = File.join(@gems_dir, 'specifications/default')
        FileUtils.mkdir_p(dst)
        Dir["#{@vendor_ruby}/ext/**/*.gemspec"].each do |the_gemspec|
          STDERR.puts "-> FileUtils.cp(#{the_gemspec}, #{dst})"
          FileUtils.cp(the_gemspec, dst)
        end

        Utils.chdir(@project_root) do
          gemspecs = Dir['./*.gemspec']
          if gemspecs.size > 0
            raise 'Multiple gemspecs detected' unless 1 == gemspecs.size
            @pre_prepare_dir = File.join(@options[:tmpdir], '__pre_prepare__')
            STDERR.puts "-> FileUtils.rm_rf(#{@pre_prepare_dir})"
            FileUtils.rm_rf(@pre_prepare_dir)
            STDERR.puts "-> FileUtils.cp_r(#{@project_root}, #{@pre_prepare_dir})"
            FileUtils.cp_r(@project_root, @pre_prepare_dir)
            Utils.chdir(@pre_prepare_dir) do
              STDERR.puts "-> Detected a RubyGem project, trying to build the gem first"
              STDERR.puts "-> FileUtils.rm_f('./*.gem')"
              FileUtils.rm_f('./*.gem')
              Utils.run("bundle")
              Utils.run("bundle exec gem build #{Shellwords.escape gemspecs.first}")
              gems = Dir['./*.gem']
              raise 'gem building failed' unless 1 == gems.size
              the_gem = gems.first
              Utils.run("gem install #{Shellwords.escape the_gem} --force --local --no-rdoc --no-ri --install-dir #{Shellwords.escape @gems_dir}")
              if File.exist?(File.join(@gems_dir, "bin/#{@entrance}"))
                @memfs_entrance = "#{MEMFS}/_gems_/bin/#{@entrance}"
              else
                Utils.chdir(File.join(@gems_dir, "bin")) do
                  raise Error, "Cannot find entrance #{@entrance}, available entrances are #{ Dir['*'].join(', ') }."
                end
              end
            end
          else
            @work_dir_local = File.join(@work_dir_inner, '_local_')
            @chdir_at_startup = '/__enclose_io_memfs__/_local_'
            Utils.run("gem install #{Shellwords.escape @vendor_bundler} --force --local --no-rdoc --no-ri --install-dir #{Shellwords.escape @gems_dir}")
            STDERR.puts "-> FileUtils.cp_r(#{@project_root}, #{@work_dir_local})"
            FileUtils.cp_r(@project_root, @work_dir_local)
            Utils.chdir(@work_dir_local) do
              Utils.run('bundle install --deployment --binstubs')
              if File.exist?("bin/#{@entrance}")
                @memfs_entrance = "#{MEMFS}/_local_/bin/#{@entrance}"
              else
                Utils.chdir('bin') do
                  raise Error, "Cannot find entrance #{@entrance}, available entrances are #{ Dir['*'].join(', ') }."
                end
              end
            end
            Utils.chdir(@work_dir_local) do
              STDERR.puts "-> FileUtils.rm_rf('.git')"
              FileUtils.rm_rf('.git')
            end
          end
          
          STDERR.puts "-> FileUtils.rm_rf(#{File.join(@gems_dir, 'cache')})"
          FileUtils.rm_rf(File.join(@gems_dir, 'cache'))
        end
      end
    end

    def make_enclose_io_memfs
      Utils.chdir(@vendor_ruby) do
        FileUtils.rm_f('enclose_io/memfs.squashfs')
        FileUtils.rm_f('enclose_io/memfs.c')
        Utils.run("mksquashfs -version")
        Utils.run("mksquashfs #{Shellwords.escape @work_dir} enclose_io/memfs.squashfs")
        bytes = IO.binread('enclose_io/memfs.squashfs').bytes
        # TODO slow operation
        # remember to change vendor/ruby/enclose_io/memfs.c as well
        File.open("enclose_io/memfs.c", "w") do |f|
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
        # TODO slow operation
        Utils.run("cc -c enclose_io/memfs.c -o enclose_io/memfs.o")
        raise 'logic error' unless File.exist?('enclose_io/memfs.o')
      end
    end

    def make_enclose_io_vars
      Utils.chdir(@vendor_ruby) do
        entrances = @entrance.split
        File.open("include/enclose_io.h", "w") do |f|
          # remember to change vendor/ruby/include/enclose_io.h as well
          # might need to remove some object files at the 2nd pass
          f.puts '#ifndef ENCLOSE_IO_H_999BC1DA'
          f.puts '#define ENCLOSE_IO_H_999BC1DA'
          f.puts ''
          f.puts '#include "enclose_io_common.h"'
          f.puts '#include "enclose_io_intercept.h"'
          f.puts ''
          f.puts "#define ENCLOSE_IO_CHDIR_AT_STARTUP #{@chdir_at_startup.inspect}" if @chdir_at_startup
          f.puts %Q!
#define ENCLOSE_IO_ENTRANCE do { \\\n\
		new_argv = malloc( (argc + 1 + 1) * sizeof(char *)); \\\n\
		assert(new_argv); \\\n\
		new_argv[0] = argv[0]; \\\n\
		new_argv[1] = #{@memfs_entrance.inspect}; \\\n\
		for (size_t i = 1; i < argc; ++i) { \\\n\
			new_argv[2 + i - 1] = argv[i]; \\\n\
		} \\\n\
		new_argc = argc + 1; \\\n\
		new_argv[new_argc] = ""; \\\n\
	} while(0)
          !.strip
          f.puts '#endif'
          f.puts ''
        end
      end
    end
  end
end
