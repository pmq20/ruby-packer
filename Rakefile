# frozen_string_literal: true

require 'rake/testtask'
require 'rake/clean'

task default: %w[test]
rubyc_deps = FileList[File.expand_path('**/*', __dir__)] - [File.expand_path('rubyc', __dir__)]

desc 'build rubyc'
file 'rubyc' => rubyc_deps do
  warn 'Rebuilding rubyc...'

  # don't include rubyc in rubyc
  rm_f 'rubyc'

  ruby_args = ['bin/rubyc', 'bin/rubyc', '-o', 'rubyc', ENV['ENCLOSE_IO_RUBYC_ADDTIONAL_ARGS']].compact
  warn "Will call ruby with #{ruby_args}"
  ruby(*ruby_args)
end

CLEAN << 'rubyc'

namespace 'rubyc' do
  rubyc_original_ruby_env = {
    'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1'
  }

  desc 'run irb from inside rubyc'
  task irb: 'rubyc' do
    sh rubyc_original_ruby_env, './rubyc', '/__enclose_io_memfs__/bin/irb'
  end

  desc 'run ruby -e from inside rubyc'
  task :ruby, [:e] => 'rubyc' do |_, args|
    sh rubyc_original_ruby_env, './rubyc', '-e', args[:e]
  end
end
