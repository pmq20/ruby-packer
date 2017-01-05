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
    STDERR.puts "-> FileUtils.mkdir_p #{@vendor_gmp_build_dir}"
    FileUtils.mkdir_p(@vendor_gmp_build_dir)
    raise "#{@vendor_gmp_build_dir} does not exist" unless Dir.exist?(@vendor_gmp_build_dir)
  end
  
  def compile_gmp
    Utils.chdir(@vendor_gmp_dir) do
      Utils.run("./configure --prefix=#{Shellwords.escape @vendor_gmp_build_dir}")
      Utils.run("make #{@options[:make_args]}")
      Utils.run('make install')
      Utils.remove_dynamic_libs(@vendor_gmp_build_dir)
      Utils.copy_static_libs(File.join(@vendor_gmp_build_dir, 'lib'), @vendor_ruby)
    end
  end
  
  def init_zlib
    @vendor_zlib_dir = File.join @options[:tmpdir], 'zlib'
    raise "#{@vendor_zlib_dir} does not exist" unless Dir.exist?(@vendor_zlib_dir)
    @vendor_zlib_build_dir = File.join(@vendor_zlib_dir, 'build')
    STDERR.puts "-> FileUtils.mkdir_p #{@vendor_zlib_build_dir}"
    FileUtils.mkdir_p(@vendor_zlib_build_dir)
    raise "#{@vendor_zlib_build_dir} does not exist" unless Dir.exist?(@vendor_zlib_build_dir)
    @vendor_zlib_build_include_dir = File.join(@vendor_zlib_build_dir, 'include')
  end
  
  def compile_zlib
    Utils.chdir(@vendor_zlib_dir) do
      Utils.run("./configure --static --prefix=#{Shellwords.escape @vendor_zlib_build_dir}")
      Utils.run("make #{@options[:make_args]}")
      Utils.run('make install')
      Utils.remove_dynamic_libs(@vendor_zlib_build_dir)
      Utils.copy_static_libs(File.join(@vendor_zlib_build_dir, 'lib'), @vendor_ruby)
    end
  end
  
  def init_yaml
    @vendor_yaml_dir = File.join @options[:tmpdir], 'yaml'
    raise "#{@vendor_yaml_dir} does not exist" unless Dir.exist?(@vendor_yaml_dir)
    @vendor_yaml_build_dir = File.join(@vendor_yaml_dir, 'build')
    STDERR.puts "-> FileUtils.mkdir_p #{@vendor_yaml_build_dir}"
    FileUtils.mkdir_p(@vendor_yaml_build_dir)
    raise "#{@vendor_yaml_build_dir} does not exist" unless Dir.exist?(@vendor_yaml_build_dir)
  end
  
  def compile_yaml
    Utils.chdir(@vendor_yaml_dir) do
      Utils.run("./configure --enable-static --prefix=#{Shellwords.escape @vendor_yaml_build_dir}")
      Utils.run("make #{@options[:make_args]}")
      Utils.run('make install')
      Utils.remove_dynamic_libs(@vendor_yaml_build_dir)
      Utils.copy_static_libs(File.join(@vendor_yaml_build_dir, 'lib'), @vendor_ruby)
    end
  end
  
  def init_openssl
    @vendor_openssl_dir = File.join @options[:tmpdir], 'openssl'
    raise "#{@vendor_openssl_dir} does not exist" unless Dir.exist?(@vendor_openssl_dir)
    @vendor_openssl_build_dir = File.join(@vendor_openssl_dir, 'build')
    STDERR.puts "-> FileUtils.mkdir_p #{@vendor_openssl_build_dir}"
    FileUtils.mkdir_p(@vendor_openssl_build_dir)
    raise "#{@vendor_openssl_build_dir} does not exist" unless Dir.exist?(@vendor_openssl_build_dir)
  end
  
  def compile_openssl
    Utils.chdir(@vendor_openssl_dir) do
      if Gem.win_platform?
        Utils.run("perl Configure --prefix=#{Shellwords.escape @vendor_openssl_build_dir} VC-WIN64A")
        Utils.run("nmake #{@options[:nmake_args]}")
        Utils.run("nmake install")
      else
        Utils.run("./config --prefix=#{Shellwords.escape @vendor_openssl_build_dir}")
        Utils.run("make #{@options[:make_args]}")
        Utils.run('make install')
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
    STDERR.puts "-> FileUtils.mkdir_p #{@vendor_squash_build_dir}"
    FileUtils.mkdir_p(@vendor_squash_build_dir)
    raise "#{@vendor_squash_build_dir} does not exist" unless Dir.exist?(@vendor_squash_build_dir)
  end
  
  def compile_libsquash
    Utils.chdir(@vendor_squash_build_dir) do
      Utils.run("cmake -DZLIB_INCLUDE_DIR:PATH=#{Shellwords.escape @vendor_zlib_build_include_dir} ..")
      Utils.run("cmake --build .")
      Utils.remove_dynamic_libs(@vendor_squash_build_dir)
      Utils.copy_static_libs(@vendor_squash_build_dir, @vendor_ruby)
    end
  end

  def init_gdbm
    @vendor_gdbm_dir = File.join @options[:tmpdir], 'gdbm'
    raise "#{@vendor_gdbm_dir} does not exist" unless Dir.exist?(@vendor_gdbm_dir)
    @vendor_gdbm_build_dir = File.join(@vendor_gdbm_dir, 'build')
    STDERR.puts "-> FileUtils.mkdir_p #{@vendor_gdbm_build_dir}"
    FileUtils.mkdir_p(@vendor_gdbm_build_dir)
    raise "#{@vendor_gdbm_build_dir} does not exist" unless Dir.exist?(@vendor_gdbm_build_dir)
  end
  
  def compile_gdbm
    Utils.chdir(@vendor_gdbm_dir) do
      Utils.run("./configure --enable-static --prefix=#{Shellwords.escape @vendor_gdbm_build_dir}")
      Utils.run("make #{@options[:make_args]}")
      Utils.run('make install')
      Utils.remove_dynamic_libs(@vendor_gdbm_build_dir)
      Utils.copy_static_libs(File.join(@vendor_gdbm_build_dir, 'lib'), @vendor_ruby)
    end
  end
end
