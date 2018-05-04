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

Rake::TestTask.new "test:unit" do |task|
  task.pattern = "test/unit/test_*.rb"
  task.warning = true
end

Rake::TestTask.new "test:round-trip" do |task|
  task.pattern = "test/round-trip/test_*.rb"
  task.warning = true
end

task "test:round-trip" => "rubyc"
