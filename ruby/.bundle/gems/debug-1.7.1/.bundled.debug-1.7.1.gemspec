# -*- encoding: utf-8 -*-
# stub: debug 1.7.1 ruby lib
# stub: ext/debug/extconf.rb

Gem::Specification.new do |s|
  s.name = "debug".freeze
  s.version = "1.7.1"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "homepage_uri" => "https://github.com/ruby/debug", "source_code_uri" => "https://github.com/ruby/debug" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Koichi Sasada".freeze]
  s.bindir = "exe".freeze
  s.date = "2022-12-22"
  s.description = "Debugging functionality for Ruby. This is completely rewritten debug.rb which was contained by the ancient Ruby versions.".freeze
  s.email = ["ko1@atdot.net".freeze]
  s.executables = ["rdbg".freeze]
  s.extensions = ["ext/debug/extconf.rb".freeze]
  s.files = ["CONTRIBUTING.md".freeze, "Gemfile".freeze, "LICENSE.txt".freeze, "README.md".freeze, "Rakefile".freeze, "TODO.md".freeze, "debug.gemspec".freeze, "exe/rdbg".freeze, "ext/debug/debug.c".freeze, "ext/debug/extconf.rb".freeze, "ext/debug/iseq_collector.c".freeze, "lib/debug.rb".freeze, "lib/debug/abbrev_command.rb".freeze, "lib/debug/breakpoint.rb".freeze, "lib/debug/client.rb".freeze, "lib/debug/color.rb".freeze, "lib/debug/config.rb".freeze, "lib/debug/console.rb".freeze, "lib/debug/frame_info.rb".freeze, "lib/debug/local.rb".freeze, "lib/debug/open.rb".freeze, "lib/debug/open_nonstop.rb".freeze, "lib/debug/prelude.rb".freeze, "lib/debug/server.rb".freeze, "lib/debug/server_cdp.rb".freeze, "lib/debug/server_dap.rb".freeze, "lib/debug/session.rb".freeze, "lib/debug/source_repository.rb".freeze, "lib/debug/start.rb".freeze, "lib/debug/thread_client.rb".freeze, "lib/debug/tracer.rb".freeze, "lib/debug/version.rb".freeze, "misc/README.md.erb".freeze]
  s.homepage = "https://github.com/ruby/debug".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.6.0".freeze)
  s.rubygems_version = "3.3.5".freeze
  s.summary = "Debugging functionality for Ruby".freeze
end
