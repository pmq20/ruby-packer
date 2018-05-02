require "rake/testtask"

task default: %w[test]
task test:    %w[test:unit]

file 'rubyc' do
  ruby "bin/rubyc", "bin/rubyc", "-o", "rubyc"
end

Rake::TestTask.new "test:unit" do |task|
  task.warning = true
end
