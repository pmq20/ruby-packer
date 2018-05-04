require "rake/testtask"
require "rake/clean"

task default: %w[test]
task test:    %w[test:unit]

rubyc_deps = FileList[
  __FILE__,
  "bin/rubyc",
  "lib/**/*.rb",
  "ruby/**/*",
  "vendor/**/*",
]

file "rubyc" => rubyc_deps do
  # don't include rubyc in rubyc
  rm_f "rubyc"

  ruby "bin/rubyc", "bin/rubyc", "-o", "rubyc"
end

CLEAN << "rubyc"

namespace "rubyc" do
  rubyc_original_ruby_env = {
    "ENCLOSE_IO_ALWAYS_USE_ORIGINAL_RUBY" => "1",
    "ENCLOSE_IO_USE_ORIGINAL_RUBY"        => "1",
  }

  task irb: "rubyc" do
    sh rubyc_original_ruby_env, "./rubyc", "/__enclose_io_memfs__/bin/irb"
  end

  task :ruby, [:e] => "rubyc" do |_, args|
    sh rubyc_original_ruby_env, "./rubyc", "-e", args[:e]
  end
end

namespace "test" do
  Rake::TestTask.new "roundtrip" do |task|
    task.pattern = "test/roundtrip/test_*.rb"
    task.warning = true
  end

  task "roundtrip" => "rubyc"

  desc "Run tests for unit"
  # This does not use Rake::TestTask because rake runs ruby via sh which
  # and can't determine the correct way to run ruby from inside rubyc
  task "unit" do
    $LOAD_PATH.unshift 'lib'

    Rake::FileList["test/unit/test_*.rb"].each do |test|
      require_relative test
    end

    Minitest.autorun
  end
end

task test: %w[test:unit test:roundtrip]
