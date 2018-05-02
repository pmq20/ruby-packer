# -*- encoding: utf-8 -*-
# stub: rake 12.0.0 ruby lib

Gem::Specification.new do |s|
  s.name = "rake"
  s.version = "12.0.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 1.3.2") if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib"]
  s.authors = ["Hiroshi SHIBATA", "Eric Hodel", "Jim Weirich"]
  s.bindir = "exe"
  s.date = "2016-12-06"
  s.description = "Rake is a Make-like program implemented in Ruby. Tasks and dependencies are\nspecified in standard Ruby syntax.\nRake has the following features:\n  * Rakefiles (rake's version of Makefiles) are completely defined in standard Ruby syntax.\n    No XML files to edit. No quirky Makefile syntax to worry about (is that a tab or a space?)\n  * Users can specify tasks with prerequisites.\n  * Rake supports rule patterns to synthesize implicit tasks.\n  * Flexible FileLists that act like arrays but know about manipulating file names and paths.\n  * Supports parallel execution of tasks.\n"
  s.email = ["hsbt@ruby-lang.org", "drbrain@segment7.net", ""]
  s.executables = ["rake"]
  s.files = [".gitignore", ".rubocop.yml", ".travis.yml", "CONTRIBUTING.rdoc", "Gemfile", "History.rdoc", "MIT-LICENSE", "README.rdoc", "Rakefile", "appveyor.yml", "bin/console", "bin/setup", "doc/command_line_usage.rdoc", "doc/example/Rakefile1", "doc/example/Rakefile2", "doc/example/a.c", "doc/example/b.c", "doc/example/main.c", "doc/glossary.rdoc", "doc/jamis.rb", "doc/proto_rake.rdoc", "doc/rake.1", "doc/rakefile.rdoc", "doc/rational.rdoc", "exe/rake", "lib/rake.rb", "lib/rake/application.rb", "lib/rake/backtrace.rb", "lib/rake/clean.rb", "lib/rake/cloneable.rb", "lib/rake/cpu_counter.rb", "lib/rake/default_loader.rb", "lib/rake/dsl_definition.rb", "lib/rake/early_time.rb", "lib/rake/ext/core.rb", "lib/rake/ext/string.rb", "lib/rake/file_creation_task.rb", "lib/rake/file_list.rb", "lib/rake/file_task.rb", "lib/rake/file_utils.rb", "lib/rake/file_utils_ext.rb", "lib/rake/invocation_chain.rb", "lib/rake/invocation_exception_mixin.rb", "lib/rake/late_time.rb", "lib/rake/linked_list.rb", "lib/rake/loaders/makefile.rb", "lib/rake/multi_task.rb", "lib/rake/name_space.rb", "lib/rake/packagetask.rb", "lib/rake/phony.rb", "lib/rake/private_reader.rb", "lib/rake/promise.rb", "lib/rake/pseudo_status.rb", "lib/rake/rake_module.rb", "lib/rake/rake_test_loader.rb", "lib/rake/rule_recursion_overflow_error.rb", "lib/rake/scope.rb", "lib/rake/task.rb", "lib/rake/task_argument_error.rb", "lib/rake/task_arguments.rb", "lib/rake/task_manager.rb", "lib/rake/tasklib.rb", "lib/rake/testtask.rb", "lib/rake/thread_history_display.rb", "lib/rake/thread_pool.rb", "lib/rake/trace_output.rb", "lib/rake/version.rb", "lib/rake/win32.rb", "rake.gemspec"]
  s.homepage = "https://github.com/ruby/rake"
  s.licenses = ["MIT"]
  s.rdoc_options = ["--main", "README.rdoc"]
  s.required_ruby_version = Gem::Requirement.new(">= 1.9.3")
  s.rubygems_version = "2.4.5"
  s.summary = "Rake is a Make-like program implemented in Ruby"

  if s.respond_to? :specification_version then
    s.specification_version = 4

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_development_dependency(%q<bundler>, [">= 0"])
      s.add_development_dependency(%q<minitest>, [">= 0"])
      s.add_development_dependency(%q<rdoc>, [">= 0"])
    else
      s.add_dependency(%q<bundler>, [">= 0"])
      s.add_dependency(%q<minitest>, [">= 0"])
      s.add_dependency(%q<rdoc>, [">= 0"])
    end
  else
    s.add_dependency(%q<bundler>, [">= 0"])
    s.add_dependency(%q<minitest>, [">= 0"])
    s.add_dependency(%q<rdoc>, [">= 0"])
  end
end
