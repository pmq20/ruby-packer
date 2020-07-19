# frozen_string_literal: true

# Copyright (c) 2017 - 2020 Minqi Pan et al.
#
# This file is part of Ruby Packer, distributed under the MIT License
# For full terms see the included LICENSE file

require_relative './compiler/constants'
require_relative './compiler/error'
require_relative './compiler/utils'

require 'shellwords'
require 'tmpdir'
require 'fileutils'
require 'find'
require 'open3'
require 'pathname'
require 'uri'

# A Compiler object corresponds to a Ruby Packer runtime instance
class Compiler
  class << self
    def ruby_api_version
      @ruby_api_version ||= peek_ruby_api_version
    end

    def ruby_version
      @ruby_version ||= peek_ruby_version
    end

    def peek_ruby_version
      version_info = File.read(File.join(PRJ_ROOT, 'ruby/include/ruby/version.h'))
      versions = []
      raise 'Cannot peek RUBY_API_VERSION_MAJOR' unless version_info =~ /RUBY_API_VERSION_MAJOR\s+(\d+)/

      versions << Regexp.last_match(1).dup
      raise 'Cannot peek RUBY_API_VERSION_MINOR' unless version_info =~ /RUBY_API_VERSION_MINOR\s+(\d+)/

      versions << Regexp.last_match(1).dup

      version_info = File.read(File.join(PRJ_ROOT, 'ruby/version.h'))
      raise 'Cannot peek RUBY_VERSION_TEENY' unless version_info =~ /RUBY_VERSION_TEENY\s+(\d+)/

      versions << Regexp.last_match(1).dup
      versions.join('.')
    end

    def peek_ruby_api_version
      version_info = File.read(File.join(PRJ_ROOT, 'ruby/include/ruby/version.h'))
      versions = []
      raise 'Cannot peek RUBY_API_VERSION_MAJOR' unless version_info =~ /RUBY_API_VERSION_MAJOR\s+(\d+)/

      versions << Regexp.last_match(1).dup
      raise 'Cannot peek RUBY_API_VERSION_MINOR' unless version_info =~ /RUBY_API_VERSION_MINOR\s+(\d+)/

      versions << Regexp.last_match(1).dup
      raise 'Cannot peek RUBY_API_VERSION_TEENY' unless version_info =~ /RUBY_API_VERSION_TEENY\s+(\d+)/

      versions << Regexp.last_match(1).dup
      versions.join('.')
    end
  end

  attr_reader :entrance
  attr_reader :options

  DEFAULT_NAME =
    if Gem.win_platform?
      'a.exe'
    else
      'a.out'
    end

  def initialize(entrance, options = {})
    @options = options
    @utils   = Utils.new(options)

    init_entrance(entrance) if entrance
    init_options
    init_tmpdir

    log "Ruby Packer (rubyc) v#{::Compiler::VERSION}"
    if entrance
      log "- entrance: #{@entrance}"
    else
      log '- entrance: not provided, a single Ruby interpreter executable will be produced.'
      log '- HINT: call rubyc with --help to see more options and use case examples'
    end
    log "- options: #{@options}"
    log

    @ldflags = ''
    @cflags = if Gem.win_platform?
                if @options[:debug]
                  ' -MD /DEBUG:FULL /Od -Zi '
                else
                  ' -MD /Ox '
                end
              elsif @options[:debug]
                ' -DRUBY_DEBUG -fPIC -g -O0 -pipe '
              else
                ' -DRUBY_DEBUG -fPIC -O3 -fno-fast-math -ggdb3 -Os -fdata-sections -ffunction-sections -pipe '
              end

    @ruby_install1 = File.join(@options[:tmpdir], 'ruby_install1')
    @ruby_install1_bin = File.join(@ruby_install1, 'bin')
    @gem = File.join(@ruby_install1_bin, 'gem')
    @bundle = File.join(@ruby_install1_bin, 'bundle')
    @ruby_install2 = File.join(@options[:tmpdir], 'ruby_install2')

    # prefix for stuffed libraries
    @local_build = File.join(@options[:tmpdir], 'local')

    @build_pass1 = File.join(@options[:tmpdir], 'build_pass1')
    @build_pass2 = File.join(@options[:tmpdir], 'build_pass2')

    @ruby_configure = (Gem.win_platform? ? "#{@ruby_source_dir}\\win32\\configure.bat" : File.join(@ruby_source_dir, 'configure'))

    @work_dir = File.join(@options[:tmpdir], 'rubyc_work_dir')
    @work_dir_inner = File.join(@work_dir, '__enclose_io_memfs__')
    @work_dir_local = File.join(@work_dir_inner, 'local')

    clean_env
  end

  def init_entrance(entrance)
    @entrance = File.expand_path(entrance)
    unless File.exist?(@entrance)
      warn "You specified \"#{entrance}\" as the ENTRANCE. However, file #{@entrance} does not appear to exist."
      exit 1
    end

    if @options[:root]
      @root = File.expand_path(@options[:root])
      unless Dir.exist?(@root)
        warn "You specified \"#{@options[:root]}\" as the root of your application. However, directory #{@root} does not appear to exist."
        exit 2
      end
    else
      @root = @entrance.dup
      loop do
        @root = File.expand_path('..', @root)
        if File.expand_path('..', @root) == @root
          @root = Dir.pwd
          break
        end
        break if File.exist?(File.join(@root, 'Gemfile'))
        break if Dir.exist?(File.join(@root, '.git'))
      end
      log "-> Project root not supplied, #{@root} assumed."
    end

    @root_gemfile_exists = File.exist?(File.join(@root, 'Gemfile'))
    @memfs_entrance = mempath(@entrance)
  end

  def init_options
    @options[:make_args] ||= "-j#{@utils.default_make_j_arg}" unless Gem.win_platform?
    @options[:output] ||= DEFAULT_NAME
    @options[:output] = File.expand_path(@options[:output])
    @options[:tmpdir] ||= File.expand_path('rubyc', Dir.tmpdir)
    @options[:tmpdir] = File.expand_path(@options[:tmpdir])
    @options[:openssl_dir] ||= '/usr/local/etc/openssl/'
    @options[:ignore_file].concat(File.readlines('.rubycignore').map(&:strip)) if File.exist?('.rubycignore')

    if @options[:auto_update_url] || @options[:auto_update_base]
      raise Error, 'Please provide both --auto-update-url and --auto-update-base' unless !@options[:auto_update_url].empty? && !@options[:auto_update_base].empty?
    end

    @ruby_source_dir =
      File.join(@options[:tmpdir], "ruby-#{::Compiler::VERSION}")
  end

  def init_tmpdir
    return unless @root && @options[:tmpdir].include?(@root)

    raise Error, "Tempdir #{@options[:tmpdir]} cannot reside inside #{@root}."
  end

  def run!
    @utils.rm_rf(@options[:tmpdir]) unless @options[:keep_tmpdir]
    @utils.mkdir_p(@options[:tmpdir])
    @utils.mkdir_p(@build_pass1)
    @utils.mkdir_p(@build_pass2)

    copy_ruby_source unless Dir.exist?(@ruby_source_dir)
    stuff_zlib
    stuff_openssl
    stuff_gdbm
    stuff_yaml
    stuff_libffi
    stuff_ncurses
    stuff_readline
    prepare_additional_flags
    patch_common_mk
    patch_win32_makefile_sub if Gem.win_platform?
    build_pass1 unless Dir.exist?(@ruby_install1)
    local_toolchain_bundle unless Dir.exist?(@work_dir)
    local_toolchain_merge unless Dir.exist?(@work_dir)
    local_toolchain_clean unless Dir.exist?(@work_dir)
    ext_setup
    build_pass2
  end

  def build_pass1
    log '=> Building ruby for the first pass'
    if Gem.win_platform?
      build_pass1_windows
    else
      build_pass1_unix
    end
  end

  def build_pass2
    log '=> Building ruby for the second pass'
    if Gem.win_platform?
      build_pass2_windows
    else
      build_pass2_unix
    end
  end

  def build_pass1_unix
    @utils.chdir(@build_pass1) do
      @utils.run(compile_pass1_env,
                 @ruby_configure,
                 '-C',
                 '--prefix', @ruby_install1,
                 '--enable-bundled-libyaml',
                 '--without-gmp',
                 '--disable-dtrace',
                 '--enable-debug-env',
                 '--disable-install-rdoc')
      @utils.run(compile_pass1_env, "make #{@options[:make_args]}")
      @utils.run(compile_pass1_env, 'make update-gems')
      @utils.run(compile_pass1_env, 'make extract-gems')
      @utils.run(compile_pass1_env, 'make install')
    end
  end

  def build_pass1_windows
    @utils.chdir(@build_pass1) do
      @utils.run(compile_pass1_env,
                 'call', @ruby_configure,
                 '--target=x64-mswin64',
                 '--disable-install-doc',
                 "--with-openssl-dir=#{@local_build}",
                 "--prefix=#{@ruby_install1}")
      @utils.run(compile_pass1_env, "nmake #{@options[:nmake_args]}")
      @utils.run(compile_pass1_env, 'nmake install')
    end
  end

  def ext_setup
    File.open(File.join(@ruby_source_dir, 'ext', 'Setup'), 'w') do |f|
      f.puts 'option nodynamic'
    end
  end

  def local_toolchain_bundle
    @utils.mkdir_p(@work_dir)
    @utils.mkdir_p(@work_dir_inner)
    @utils.cp_r(@root, @work_dir_local) if @root

    return unless @root_gemfile_exists

    @utils.chdir(@work_dir_local) do
      log '=> gem env'
      @utils.run local_toolchain_env, @gem, 'env'
      @utils.run(local_toolchain_env, @bundle, 'install')
      # detect Rails
      if @utils.run_allow_failures(local_toolchain_env, @bundle, 'show', 'rails').exitstatus.zero?
        log '=> Detected a Rails project'
        @enclose_io_rails = true
        @utils.rm_rf('tmp')
        @utils.rm_rf('log')
        @utils.mkdir('tmp')
        @utils.mkdir('log')
      else
        log '=> Not a Rails project'
      end
    end
  end

  def local_toolchain_merge
    @utils.rm_rf(File.join(@ruby_install1, 'lib', 'ruby', 'gems', self.class.ruby_api_version, 'cache'))
    Dir[File.join(@ruby_install1, '*')].each do |path|
      @utils.cp_r(path, @work_dir_inner, preserve: true)
    end
  end

  def local_toolchain_clean
    Dir["#{@work_dir_inner}/**/*.{a,dylib,so,dll,lib,bundle}"].each do |thisdl|
      @utils.rm_f(thisdl)
    end

    return unless Dir.exist?(@work_dir_local)

    @utils.chdir(@work_dir_local) do
      if Dir.exist?('.git')
        log `git status`
        @utils.rm_rf('.git')
      end
      if File.exist?('a.exe')
        log `dir a.exe`
        @utils.rm_rf('a.exe')
      end
      if File.exist?('a.out')
        log `ls -l a.out`
        @utils.rm_rf('a.out')
      end
    end

    return unless @options[:ignore_file].is_a?(Array)

    @utils.chdir(@work_dir_local) do
      Dir["{#{@options[:ignore_file].join(',')}}"].each do |ignored_file|
        next unless File.exist?(ignored_file)

        @utils.rm_rf(ignored_file)
      end
    end
  end

  def build_pass2_unix
    @utils.chdir(@build_pass2) do
      baseruby = File.join(@ruby_install1_bin, 'ruby')

      @utils.run(compile_pass2_env,
                 @ruby_configure,
                 '-C',
                 '--prefix', @ruby_install2,
                 "--with-baseruby=#{baseruby}",
                 '--enable-bundled-libyaml',
                 '--without-gmp',
                 '--disable-dtrace',
                 '--enable-debug-env',
                 '--disable-install-rdoc',
                 '--with-static-linked-ext')

      make_enclose_io_memfs
      make_enclose_io_vars

      @utils.run(compile_pass2_env, "make #{@options[:make_args]}")
      @utils.cp('ruby', @options[:output])
    end
  end

  def build_pass2_windows
    @utils.chdir(@build_pass2) do
      @utils.run(compile_pass2_env,
                 'call', @ruby_configure,
                 "--prefix=#{@ruby_install2}",
                 '--enable-bundled-libyaml',\
                 '--enable-debug-env',
                 '--disable-install-doc',
                 '--with-static-linked-ext')

      make_enclose_io_memfs
      make_enclose_io_vars

      @utils.run(compile_pass2_env, "nmake #{@options[:nmake_args]}")
      @utils.run(compile_pass2_env, "nmake #{@options[:nmake_args]} ruby_static.exe")
      @utils.cp('ruby_static.exe', @options[:output])
    end
  end

  def make_enclose_io_memfs
    @utils.chdir(@ruby_source_dir) do
      log '=> making squashfs'

      @utils.rm_f('enclose_io_memfs.squashfs')
      @utils.rm_f('enclose_io_memfs.c')

      log "The following files are going to be packed:\n#{Dir[File.join(@work_dir, '**/*')].join("\n")}" if @options[:debug]

      @utils.run('mksquashfs', '-version')
      @utils.run('mksquashfs', @work_dir, 'enclose_io_memfs.squashfs')

      squashfs_to_c 'enclose_io_memfs.squashfs', 'enclose_io_memfs.c'
      log '=> squashfs complete'
    end
  end

  def make_enclose_io_vars
    @utils.chdir(@ruby_source_dir) do
      File.open('enclose_io.h', 'w') do |f|
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
        f.puts '#define ENCLOSE_IO_ENV_BUNDLE_GEMFILE "/__enclose_io_memfs__/local/Gemfile"' if @root_gemfile_exists
        f.puts "#define ENCLOSE_IO_ENTRANCE #{@memfs_entrance.inspect}" if @entrance
        f.puts '#define ENCLOSE_IO_RAILS 1' if @enclose_io_rails
        if @options[:auto_update_url] && @options[:auto_update_base]
          f.puts '#define ENCLOSE_IO_AUTO_UPDATE 1'
          f.puts "#define ENCLOSE_IO_AUTO_UPDATE_BASE #{@options[:auto_update_base].inspect}"
          urls = URI.split(@options[:auto_update_url])
          raise 'logic error' unless urls.length == 9

          port = urls[3]
          if port.nil?
            port = if urls[0] == 'https'
                     443
                   else
                     80
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

  ##
  # Remove ruby, bundler, and gem environment variables.  The ruby we build in
  # the first pass will install files in the correct places.

  def clean_env
    ENV.delete 'BUNDLE_BIN_PATH'  # disable loading outside bundler
    ENV.delete 'BUNDLE_GEMFILE'   # disable loading outside bundler
    ENV.delete 'GEM_HOME'         # use default gem install location
    ENV.delete 'GEM_PATH'         # use default installed gem locations
    ENV.delete 'RUBYGEMS_GEMDEPS' # disable loading outside gems
    ENV.delete 'RUBYLIB'          # disable outside changes to $LOAD_PATH
    ENV.delete 'RUBYOPT'          # disable outside changes to ruby options

    log '=> ENV'
    ENV.sort_by(&:first).each do |name, value|
      log "\t#{@utils.escape name} => #{@utils.escape value}"
    end
  end

  def copy_ruby_source
    source = File.join(PRJ_ROOT, 'ruby')
    @utils.cp_r(source, @ruby_source_dir, preserve: true)
    @utils.chdir(@ruby_source_dir) do
      Dir['**/configure.ac'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end
      Dir['**/*.m4'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end
      File.utime(Time.at(0), Time.at(0), 'parse.y')
      File.utime(Time.at(0), Time.at(0), 'ext/ripper/ripper.y')
    end
  end

  def log(message = nil)
    return if @options[:quiet]

    warn message
  end

  def stuff(library)
    source = File.join PRJ_ROOT, 'vendor', library
    target = File.join @options[:tmpdir], library

    return if Dir.exist? target

    @utils.cp_r source, target, preserve: true

    log "=> Stuffing #{library}..."

    @utils.capture_run_io "stuff_#{library}" do
      @utils.chdir(target) do
        yield
      end
    end

    log "=> Stuffed #{library}"
  end

  def stuff_zlib
    stuff 'zlib' do
      Dir['**/configure.ac'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end
      Dir['**/*.m4'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end

      if Gem.win_platform?
        @utils.run(compile_env, 'nmake /f win32\\Makefile.msc')
      else
        @utils.run(compile_env,
                   './configure',
                   '--static',
                   "--prefix=#{@local_build}")
        @utils.run(compile_env, "make #{@options[:make_args]}")
        @utils.run(compile_env, 'make install')
      end
    end
  end

  def stuff_openssl
    stuff 'openssl' do
      Dir['**/configure.ac'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end
      Dir['**/*.m4'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end
      if Gem.win_platform?
        @utils.run(compile_env,
                   'perl',
                   'Configure',
                   'VC-WIN64A',
                   'no-shared',
                   "--openssldir=#{@options[:openssl_dir]}",
                   "--prefix=#{@local_build}")
        @utils.run(compile_env, "nmake #{@options[:nmake_args]}")
        @utils.run(compile_env, 'nmake install_sw')
      else
        @utils.run(compile_env,
                   './config',
                   'no-shared',
                   "--openssldir=#{@options[:openssl_dir]}",
                   "--prefix=#{@local_build}")
        @utils.run(compile_env, "make #{@options[:make_args]}")
        @utils.run(compile_env, 'make install_sw')
      end
    end
  end

  def stuff_gdbm
    return if Gem.win_platform? # TODO

    stuff 'gdbm' do
      Dir['**/configure.ac'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end
      Dir['**/*.m4'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end

      @utils.run(compile_env,
                 './configure',
                 '--with-pic',
                 '--enable-libgdbm-compat',
                 '--disable-shared',
                 '--enable-static',
                 '--without-readline',
                 "--prefix=#{@local_build}")
      @utils.run(compile_env, "make #{@options[:make_args]}")
      @utils.run(compile_env, 'make install')
    end
  end

  def stuff_yaml
    return if Gem.win_platform? # TODO

    stuff 'yaml' do
      Dir['**/configure.ac'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end
      Dir['**/*.m4'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end

      @utils.run(compile_env,
                 './configure',
                 '--with-pic',
                 '--disable-shared',
                 '--enable-static',
                 "--prefix=#{@local_build}")
      @utils.run(compile_env, "make #{@options[:make_args]}")
      @utils.run(compile_env, 'make install')
    end
  end

  def stuff_libffi
    return if Gem.win_platform? # TODO

    stuff 'libffi' do
      Dir['**/configure.ac'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end
      Dir['**/*.m4'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end

      @utils.run(compile_env,
                 './configure',
                 '--with-pic',
                 '--disable-shared',
                 '--enable-static',
                 "--prefix=#{@local_build}")
      @utils.run(compile_env, "make #{@options[:make_args]}")
      @utils.run(compile_env, 'make install')
    end
  end

  def stuff_ncurses
    return if Gem.win_platform? # TODO

    stuff 'ncurses' do
      Dir['**/configure.ac'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end
      Dir['**/*.m4'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end
      @utils.run(compile_env,
                 './configure',
                 '--without-shared',
                 '--without-cxx-shared',
                 "--prefix=#{@local_build}")
      @utils.run(compile_env, "make #{@options[:make_args]}")
      @utils.run(compile_env, 'make install.libs')
    end
  end

  def stuff_readline
    return if Gem.win_platform? # TODO

    stuff 'readline' do
      Dir['**/configure.ac'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end
      Dir['**/*.m4'].each do |x|
        File.utime(Time.at(0), Time.at(0), x)
      end

      @utils.run(compile_env,
                 './configure',
                 '--disable-shared',
                 '--enable-static',
                 "--prefix=#{@local_build}")
      @utils.run(compile_env, "make #{@options[:make_args]}")
      @utils.run(compile_env, 'make install')
    end
  end

  def patch_win32_makefile_sub
    target = File.join(@ruby_source_dir, 'win32', 'Makefile.sub')
    target_content = File.read(target)

    found = 0

    File.open(target, 'w') do |f|
      target_content.each_line do |line|
        if found.zero? && (line =~ /^CFLAGS = (.*)$/)
          found = 1
          f.puts "CFLAGS = #{Regexp.last_match(1)} #{@cflags}"
        elsif found == 1 && (line =~ /^LDFLAGS = (.*)$/)
          found = 2
          f.puts "LDFLAGS = #{Regexp.last_match(1)} #{@ldflags}"
        else
          f.print line
        end
      end
    end

    raise "Failed to patch CFLAGS and LDFLAGS of #{target}" unless found == 2
  end

  def patch_common_mk
    target = File.join(@ruby_source_dir, 'common.mk')
    target_content = File.read(target)

    found = false

    File.open(target, 'w') do |f|
      target_content.each_line do |line|
        if !found && (line =~ /^INCFLAGS = (.*)$/)
          found = true
          f.puts "INCFLAGS = #{Regexp.last_match(1)} #{@cflags}"
        else
          f.print line
        end
      end
    end

    raise "Failed to patch INCFLAGS of #{target}" unless found
  end

  def mempath(path)
    path = File.expand_path(path)
    raise "path #{path} should start with #{@root}" unless @root == path[0...(@root.size)]

    "#{MEMFS}/local#{path[(@root.size)..]}"
  end

  def prepare_additional_flags
    if Gem.win_platform?
      @ldflags += " -libpath:#{@utils.escape File.join(@options[:tmpdir], 'zlib').gsub('/', '\\')} #{@utils.escape File.join(@options[:tmpdir], 'zlib', 'zlib.lib')} Advapi32.lib "
      @cflags += " -I#{@utils.escape File.join(@options[:tmpdir], 'zlib')} -I#{@utils.escape @ruby_source_dir} "
    else
      lib   = File.join @local_build, 'lib'
      lib64 = File.join @local_build, 'lib64'

      @ldflags += " -L#{@utils.escape lib} "   if Dir.exist? lib
      @ldflags += " -L#{@utils.escape lib64} " if Dir.exist? lib64

      libz = File.join lib, 'libz.a'
      lib64z = File.join lib64, 'libz.a'

      @ldflags += " #{@utils.escape libz} "   if File.exist? libz
      @ldflags += " #{@utils.escape lib64z} " if File.exist? lib64z

      @cflags += " -I#{@utils.escape @ruby_source_dir} "
      @cflags += " -I#{@utils.escape File.join(@local_build, 'include')} "
      @cflags += " -I#{@utils.escape File.join(lib, 'libffi-3.2.1', 'include')} "
    end
  end

  ##
  # When you change the output here remember to change libsquash's
  # sample/enclose_io_memfs.c as well

  def squashfs_to_c(squashfs_file, c_file)
    File.open squashfs_file, 'rb' do |squashfs|
      File.open(c_file, 'w') do |c|
        c.puts '#include <stdint.h>'
        c.puts '#include <stddef.h>'
        c.puts

        byte = squashfs.read(1).bytes.first
        c.puts "const uint8_t enclose_io_memfs[#{squashfs.size}] = { #{byte}"

        while (chunk = squashfs.read(101))
          c.puts ",#{chunk.bytes.join ','}"
        end

        c.puts '};'
        c.puts
      end
    end
  end

  def compile_env
    if Gem.win_platform?
      {
        'CI' => 'true',
        'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1',
        'MAKE' => 'nmake',
        'CL' => '/MP'
      }
    else
      {
        'CI' => 'true',
        'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1',
        'CFLAGS' => @cflags,
        'LDFLAGS' => @ldflags
      }
    end
  end

  def compile_pass1_env
    compile_env.merge({
                        'ENCLOSE_IO_RUBYC_1ST_PASS' => 'true',
                        'ENCLOSE_IO_RUBYC_2ND_PASS' => nil
                      })
  end

  def compile_pass2_env
    compile_env.merge({
                        'ENCLOSE_IO_RUBYC_1ST_PASS' => nil,
                        'ENCLOSE_IO_RUBYC_2ND_PASS' => 'true'
                      })
  end

  def local_toolchain_env
    {
      'CI' => 'true',
      'PATH' => "#{File.join(@ruby_install1, 'bin')}:#{ENV['PATH']}",
      'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1',
      'ENCLOSE_IO_RUBYC_1ST_PASS' => '1',
      'ENCLOSE_IO_RUBYC_2ND_PASS' => nil
    }
  end
end
