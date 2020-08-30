# frozen_string_literal: true

namespace 'test' do
  Rake::TestTask.new 'roundtrip' do |task|
    task.pattern = 'test/roundtrip/test_*.rb'
    task.warning = true
  end

  task 'roundtrip' => (Gem.win_platform? ? 'rubyc.exe' : 'rubyc')

  desc 'Run tests for unit'
  # This does not use Rake::TestTask because rake runs ruby via sh which
  # and can't determine the correct way to run ruby from inside rubyc
  task 'unit' do
    require_relative '../test/unit/test_compiler'
    Minitest.autorun
  end
end

desc 'run all tests'
task test: %w[test:unit test:roundtrip]
