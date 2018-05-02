require "rake/testtask"

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
  ruby "bin/rubyc", "bin/rubyc", "-o", "rubyc"
end

namespace "rubyc" do
  task "irb" => "rubyc" do
    env = {
      "ENCLOSE_IO_ALWAYS_USE_ORIGINAL_RUBY" => "1",
      "ENCLOSE_IO_USE_ORIGINAL_RUBY"        => "1",
    }

    sh env, "./rubyc", "/__enclose_io_memfs__/bin/irb"
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
