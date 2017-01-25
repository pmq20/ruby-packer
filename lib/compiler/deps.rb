require 'shellwords'
require 'tmpdir'
require 'fileutils'
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
      if Gem.win_platform?
        # TODO
      else
        Utils.run(@extra_envvar, "./configure --prefix=#{Utils.escape @vendor_gmp_build_dir}")
        Utils.run(@extra_envvar, "make #{@options[:make_args]}")
        Utils.run(@extra_envvar, 'make install')
      end
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
  end
  
  def compile_zlib
    Utils.chdir(@vendor_zlib_dir) do
      if Gem.win_platform?
        Utils.run(@extra_envvar, "nmake -f win32/Makefile.msc")
        Utils.remove_dynamic_libs(@vendor_zlib_dir)
        Utils.copy_static_libs(@vendor_zlib_dir, @vendor_ruby)
      else
        Utils.run(@extra_envvar, "./configure --static --prefix=#{Utils.escape @vendor_zlib_build_dir}")
        Utils.run(@extra_envvar, "make #{@options[:make_args]}")
        Utils.run(@extra_envvar, 'make install')
        Utils.remove_dynamic_libs(@vendor_zlib_build_dir)
        Utils.copy_static_libs(File.join(@vendor_zlib_build_dir, 'lib'), @vendor_ruby)
      end  
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
      Utils.run(@extra_envvar, "cmake -DCMAKE_BUILD_TYPE=#{@options[:debug] ? 'Debug' : 'Release'} -DZLIB_INCLUDE_DIR:PATH=#{Utils.escape @vendor_zlib_dir} ..")
      Utils.run(@extra_envvar, "cmake --build . --config #{@options[:debug] ? 'Debug' : 'Release'}")
      Utils.run(@extra_envvar, "make") if !Gem.win_platform? && !File.exist?('libsquash.a')
      Utils.remove_dynamic_libs(@vendor_squash_build_dir)
      Utils.copy_static_libs(@vendor_squash_build_dir, @vendor_ruby)
      Utils.copy_static_libs(File.join(@vendor_squash_build_dir, @options[:debug] ? 'Debug' : 'Release'), @vendor_ruby)
    end
  end
end
