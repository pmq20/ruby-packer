require "rake/testtask"

task default: %w[test]
task test:    %w[test:unit]

Rake::TestTask.new "test:unit" do |task|
  task.warning = true
end
