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

file 'rubyc' => rubyc_deps do
  ruby "bin/rubyc", "bin/rubyc", "-o", "rubyc"
end

Rake::TestTask.new "test:unit" do |task|
  task.warning = true
end
