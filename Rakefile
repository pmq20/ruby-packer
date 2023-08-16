# frozen_string_literal: true

LIBGDBM_VERSION = '1.23'
LIBFFI_VERSION = '3.4.4'
LIBYAML_VERSION = '0.2.5'
OPENSSL_VERSION = '3.0.10'
NCURSES_VERSION = '6.4'
ZLIB_VERSION = '1.2.13'
READLINE_VERSION = '8.2'
SQLITE3_VERSION = '3420000'
RP_RUBY_VERSION = ENV['RP_RUBY_VERSION'] || '3.2.2'
RUBY_MAJOR = RP_RUBY_VERSION.split('.')[0]
RUBY_MINOR = RP_RUBY_VERSION.split('.')[1]
RUBY_PATCH = RP_RUBY_VERSION.split('.')[2].split('-')[0]
RUBY_PREVIEW = RP_RUBY_VERSION.split('-')[1]
RUBY_MAJOR_MINOR = "#{RUBY_MAJOR}.#{RUBY_MINOR}".freeze
require 'rubygems'
require 'bundler/setup'
Bundler.require(:default)

require 'rake/testtask'
require 'rake/clean'

task default: %w[test]
rubyc_deps = FileList[File.expand_path('**/*', __dir__)] - [File.expand_path(((Gem.win_platform? ? 'rubyc.exe' : 'rubyc')), __dir__)]

desc "build #{Gem.win_platform? ? 'rubyc.exe' : 'rubyc'}"
file((Gem.win_platform? ? 'rubyc.exe' : 'rubyc') => rubyc_deps) do
  warn "Rebuilding #{Gem.win_platform? ? 'rubyc.exe' : 'rubyc'}..."

  # don't include rubyc in rubyc
  rm_f(Gem.win_platform? ? 'rubyc.exe' : 'rubyc')

  ruby_args = ['bin/rubyc', 'bin/rubyc', '-o', 'rubyc']
  if ENV['ENCLOSE_IO_RUBYC_ADDTIONAL_ARGS'].present?
    ENV['ENCLOSE_IO_RUBYC_ADDTIONAL_ARGS'].split(/\s+/).each do |arg|
      ruby_args << arg.strip
    end
  end
  warn "Will call ruby with #{ruby_args}"
  ruby(*ruby_args)
end

CLEAN << (Gem.win_platform? ? 'rubyc.exe' : 'rubyc')

namespace 'rubyc' do
  rubyc_original_ruby_env = {
    'ENCLOSE_IO_USE_ORIGINAL_RUBY' => 'true'
  }

  desc 'run pry from inside rubyc'
  task pry: (Gem.win_platform? ? 'rubyc.exe' : 'rubyc') do
    sh rubyc_original_ruby_env, (Gem.win_platform? ? '.\\rubyc.exe' : './rubyc'), '/__enclose_io_memfs__/bin/pry'
  end

  desc 'run irb from inside rubyc'
  task irb: (Gem.win_platform? ? 'rubyc.exe' : 'rubyc') do
    sh rubyc_original_ruby_env, (Gem.win_platform? ? '.\\rubyc.exe' : './rubyc'), '-rirb', '-e', 'IRB.start'
  end

  desc 'run ruby -e from inside rubyc'
  task :ruby, [:e] => (Gem.win_platform? ? 'rubyc.exe' : 'rubyc') do |_, args|
    puts args
    puts args[:e]
    sh rubyc_original_ruby_env, (Gem.win_platform? ? '.\\rubyc.exe' : './rubyc'), '-e', args[:e]
  end
end

desc 'Patch Ruby source code'
task patch_ruby_source: %i[download_ruby_source download_vendors] do
  Dir.chdir('ruby-enclose') do
    puts 'Copying Ruby source code'
    FileUtils.cp_r('.', '../ruby')
  end

  Dir.chdir("ruby-enclose-#{RP_RUBY_VERSION}-patch") do
    puts 'Copying Ruby patch'
    FileUtils.cp_r('.', '../ruby')
  end

  Dir.chdir('vendor-enclose') do
    puts 'Copying vendor patchees'
    FileUtils.cp_r('.', '../vendor')
  end
end

desc 'Download and extract the Ruby source code'
task :download_ruby_source do
  unless File.exist?("ruby-#{RP_RUBY_VERSION}.tar.gz")
    puts 'Downloading Ruby source'
    system("wget -O ruby-#{RP_RUBY_VERSION}.tar.gz.tmp http://cache.ruby-lang.org/pub/ruby/#{RUBY_MAJOR_MINOR}/ruby-#{RP_RUBY_VERSION}.tar.gz")
    FileUtils.mv("ruby-#{RP_RUBY_VERSION}.tar.gz.tmp", "ruby-#{RP_RUBY_VERSION}.tar.gz")
    puts
  end

  puts 'Extracting source code'
  FileUtils.rm_rf('ruby')
  FileUtils.rm_rf("ruby-#{RP_RUBY_VERSION}")
  FileUtils.mkdir_p("ruby-#{RP_RUBY_VERSION}")
  system("tar xzf ruby-#{RP_RUBY_VERSION}.tar.gz -C ruby-#{RP_RUBY_VERSION} --strip-components=1")
  FileUtils.rm_rf("ruby-#{RP_RUBY_VERSION}.tar.gz")
  puts "Entering ruby-#{RP_RUBY_VERSION}"
  Dir.chdir("ruby-#{RP_RUBY_VERSION}") do
    system('ls')
  end
  FileUtils.mv("ruby-#{RP_RUBY_VERSION}", 'ruby')
end

desc 'Download and extract the source code of the libraries used by the ruby-packer build process'
task :download_vendors do
  vendor_dir = 'vendor'

  FileUtils.mkdir_p(vendor_dir)

  download_and_extract(vendor_dir, 'libffi', "libffi-#{LIBFFI_VERSION}", "https://github.com/libffi/libffi/releases/download/v#{LIBFFI_VERSION}/libffi-#{LIBFFI_VERSION}.tar.gz")
  download_and_extract(vendor_dir, 'yaml', "yaml-#{LIBYAML_VERSION}", "https://pyyaml.org/download/libyaml/yaml-#{LIBYAML_VERSION}.tar.gz")
  download_and_extract(vendor_dir, 'openssl', "openssl-#{OPENSSL_VERSION}", "https://www.openssl.org/source/openssl-#{OPENSSL_VERSION}.tar.gz")
  download_and_extract(vendor_dir, 'ncurses', "ncurses-#{NCURSES_VERSION}", "https://ftp.gnu.org/pub/gnu/ncurses/ncurses-#{NCURSES_VERSION}.tar.gz")
  download_and_extract(vendor_dir, 'zlib', "zlib-#{ZLIB_VERSION}", "https://zlib.net/zlib-#{ZLIB_VERSION}.tar.gz")
  download_and_extract(vendor_dir, 'readline', "readline-#{READLINE_VERSION}", "https://ftp.gnu.org/gnu/readline/readline-#{READLINE_VERSION}.tar.gz")
  download_and_extract(vendor_dir, 'gdbm', "gdbm-#{LIBGDBM_VERSION}", "https://ftp.gnu.org/gnu/gdbm/gdbm-#{LIBGDBM_VERSION}.tar.gz")
  download_and_extract(vendor_dir, 'sqlite3', "sqlite-autoconf-#{SQLITE3_VERSION}", "https://www.sqlite.org/2023/sqlite-autoconf-#{SQLITE3_VERSION}.tar.gz")
end

def download_and_extract(base_name, lib, lib_plus_version, url)
  Dir.chdir(base_name) do
    FileUtils.rm_rf(lib)
    system("wget -O #{lib_plus_version}.tmp #{url}")

    if url.end_with?('.bz2')
      system("tar xjf #{lib_plus_version}.tmp")
    else
      system("tar xzf #{lib_plus_version}.tmp")
    end

    FileUtils.mv(lib_plus_version, lib)
    FileUtils.rm_rf("#{lib_plus_version}.tmp")
  end
end
