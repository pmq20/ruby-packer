# frozen_string_literal: true

require 'rubygems'
require 'bundler/setup'
Bundler.require(:default)

require 'rake/testtask'
require 'rake/clean'

task default: %w[test]
rubyc_deps = FileList[File.expand_path('**/*', __dir__)] - [File.expand_path(((Gem.win_platform? ? 'rubyc.exe' : 'rubyc')), __dir__)]

desc "build #{(Gem.win_platform? ? 'rubyc.exe' : 'rubyc')}"
file((Gem.win_platform? ? 'rubyc.exe' : 'rubyc') => rubyc_deps) do
  warn "Rebuilding #{(Gem.win_platform? ? 'rubyc.exe' : 'rubyc')}..."

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
    sh rubyc_original_ruby_env, (Gem.win_platform? ? '.\\rubyc.exe' : './rubyc'), '-e', args[:e]
  end
end
