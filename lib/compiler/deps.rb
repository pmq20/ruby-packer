require 'shellwords'
require 'tmpdir'
require 'fileutils'
require 'json'
require 'open3'

class Compiler
  def init_gmp
    @vendor_gmp_dir = File.join(@options[:tmpdir], 'gmp')
    raise "#{@vendor_gmp_dir} does not exist" unless Dir.exist?(@vendor_gmp_dir)
    @vendor_gmp_build_dir = File.join(@vendor_gmp_dir, 'build')
    Utils.mkdir_p(@vendor_gmp_build_dir)
    raise "#{@vendor_gmp_build_dir} does not exist" unless Dir.exist?(@vendor_gmp_build_dir)
  end
  
  def compile_gmp
    Utils.chdir(@vendor_gmp_dir) do
      Utils.run(@extra_envvar, "./configure --prefix=#{Utils.escape @vendor_gmp_build_dir}")
      Utils.run(@extra_envvar, "make #{@options[:make_args]}")
      Utils.run(@extra_envvar, 'make install')
      Utils.remove_dynamic_libs(@vendor_gmp_build_dir)
      Utils.copy_static_libs(File.join(@vendor_gmp_build_dir, 'lib'), @vendor_ruby)
    end
  end
  
  def init_zlib
    @vendor_zlib_dir = File.join @options[:tmpdir], 'zlib'
    raise "#{@vendor_zlib_dir} does not exist" unless Dir.exist?(@vendor_zlib_dir)
    @vendor_zlib_build_dir = File.join(@vendor_zlib_dir, 'build')
    Utils.mkdir_p(@vendor_zlib_build_dir)
    raise "#{@vendor_zlib_build_dir} does not exist" unless Dir.exist?(@vendor_zlib_build_dir)
    @vendor_zlib_build_include_dir = File.join(@vendor_zlib_build_dir, 'include')
  end
  
  def compile_zlib
    Utils.chdir(@vendor_zlib_dir) do
      Utils.run(@extra_envvar, "./configure --static --prefix=#{Utils.escape @vendor_zlib_build_dir}")
      Utils.run(@extra_envvar, "make #{@options[:make_args]}")
      Utils.run(@extra_envvar, 'make install')
      Utils.remove_dynamic_libs(@vendor_zlib_build_dir)
      Utils.copy_static_libs(File.join(@vendor_zlib_build_dir, 'lib'), @vendor_ruby)
    end
  end
  
  def init_yaml
    @vendor_yaml_dir = File.join @options[:tmpdir], 'yaml'
    raise "#{@vendor_yaml_dir} does not exist" unless Dir.exist?(@vendor_yaml_dir)
    @vendor_yaml_build_dir = File.join(@vendor_yaml_dir, 'build')
    Utils.mkdir_p(@vendor_yaml_build_dir)
    raise "#{@vendor_yaml_build_dir} does not exist" unless Dir.exist?(@vendor_yaml_build_dir)
  end
  
  def compile_yaml
    Utils.chdir(@vendor_yaml_dir) do
      Utils.run(@extra_envvar, "./configure --enable-static --prefix=#{Utils.escape @vendor_yaml_build_dir}")
      Utils.run(@extra_envvar, "make #{@options[:make_args]}")
      Utils.run(@extra_envvar, 'make install')
      Utils.remove_dynamic_libs(@vendor_yaml_build_dir)
      Utils.copy_static_libs(File.join(@vendor_yaml_build_dir, 'lib'), @vendor_ruby)
    end
  end
  
  def init_openssl
    @vendor_openssl_dir = File.join @options[:tmpdir], 'openssl'
    raise "#{@vendor_openssl_dir} does not exist" unless Dir.exist?(@vendor_openssl_dir)
    @vendor_openssl_build_dir = File.join(@vendor_openssl_dir, 'build')
    Utils.mkdir_p(@vendor_openssl_build_dir)
    raise "#{@vendor_openssl_build_dir} does not exist" unless Dir.exist?(@vendor_openssl_build_dir)
  end
  
  def compile_openssl
    Utils.chdir(@vendor_openssl_dir) do
      if Gem.win_platform?
        Utils.run(@extra_envvar, "perl Configure --prefix=#{Utils.escape @vendor_openssl_build_dir} VC-WIN64A")
        Utils.run(@extra_envvar, "nmake #{@options[:nmake_args]}")
        Utils.run(@extra_envvar, "nmake install")
      else
        Utils.run(@extra_envvar, "./config --prefix=#{Utils.escape @vendor_openssl_build_dir}")
        x = File.read('Makefile')
        if @extra_cflags.length > 0
          File.open('Makefile', 'w') do |f|
            x.each_line.each do |line|
              if line =~ /^CFLAGS=(.*)$/
                f.puts "CFLAGS=#{@extra_cflags} #{$1}"
              else
                f.print line
              end
            end
          end
        end
        Utils.run(@extra_envvar, "make #{@options[:make_args]}")
        Utils.run(@extra_envvar, 'make install')
      end
      Utils.remove_dynamic_libs(@vendor_openssl_build_dir)
      Utils.copy_static_libs(File.join(@vendor_openssl_build_dir, 'lib'), @vendor_ruby)
    end
  end
  
  def init_libsquash
    @vendor_squash_dir = File.join @options[:tmpdir], 'libsquash'
    raise "#{@vendor_squash_dir} does not exist" unless Dir.exist?(@vendor_squash_dir)
    @vendor_squash_include_dir = File.join(@vendor_squash_dir, 'include')
    @vendor_squash_build_dir = File.join(@vendor_squash_dir, 'build')
    @vendor_squash_sample_dir = File.join(@vendor_squash_dir, 'sample')
    Utils.mkdir_p(@vendor_squash_build_dir)
    raise "#{@vendor_squash_build_dir} does not exist" unless Dir.exist?(@vendor_squash_build_dir)
  end
  
  def compile_libsquash
    Utils.chdir(@vendor_squash_build_dir) do
      Utils.run(@extra_envvar, "cmake -DZLIB_INCLUDE_DIR:PATH=#{Utils.escape @vendor_zlib_build_include_dir} ..")
      Utils.run(@extra_envvar, "cmake --build .")
      Utils.remove_dynamic_libs(@vendor_squash_build_dir)
      Utils.copy_static_libs(@vendor_squash_build_dir, @vendor_ruby)
    end
  end

  def init_gdbm
    @vendor_gdbm_dir = File.join @options[:tmpdir], 'gdbm'
    raise "#{@vendor_gdbm_dir} does not exist" unless Dir.exist?(@vendor_gdbm_dir)
    @vendor_gdbm_build_dir = File.join(@vendor_gdbm_dir, 'build')
    Utils.mkdir_p(@vendor_gdbm_build_dir)
    raise "#{@vendor_gdbm_build_dir} does not exist" unless Dir.exist?(@vendor_gdbm_build_dir)
  end
  
  def compile_gdbm
    Utils.chdir(@vendor_gdbm_dir) do
      Utils.run(@extra_envvar, "./configure --enable-static --prefix=#{Utils.escape @vendor_gdbm_build_dir}")
      Utils.run(@extra_envvar, "make #{@options[:make_args]}")
      Utils.run(@extra_envvar, 'make install')
      Utils.remove_dynamic_libs(@vendor_gdbm_build_dir)
      Utils.copy_static_libs(File.join(@vendor_gdbm_build_dir, 'lib'), @vendor_ruby)
    end
  end
end
