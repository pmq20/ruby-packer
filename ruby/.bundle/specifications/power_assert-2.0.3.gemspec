# -*- encoding: utf-8 -*-
# stub: power_assert 2.0.3 ruby lib

Gem::Specification.new do |s|
  s.name = "power_assert".freeze
  s.version = "2.0.3"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Kazuki Tsujimoto".freeze]
  s.bindir = "exe".freeze
  s.date = "2022-12-22"
  s.description = "Power Assert shows each value of variables and method calls in the expression. It is useful for testing, providing which value wasn't correct when the condition is not satisfied.".freeze
  s.email = ["kazuki@callcc.net".freeze]
  s.extra_rdoc_files = ["README.md".freeze]
  s.files = [".github/dependabot.yml".freeze, ".github/workflows/ci.yml".freeze, ".gitignore".freeze, "BSDL".freeze, "COPYING".freeze, "Gemfile".freeze, "LEGAL".freeze, "README.md".freeze, "Rakefile".freeze, "lib/power_assert.rb".freeze, "lib/power_assert/colorize.rb".freeze, "lib/power_assert/configuration.rb".freeze, "lib/power_assert/context.rb".freeze, "lib/power_assert/enable_tracepoint_events.rb".freeze, "lib/power_assert/inspector.rb".freeze, "lib/power_assert/parser.rb".freeze, "lib/power_assert/version.rb".freeze, "power_assert.gemspec".freeze]
  s.homepage = "https://github.com/ruby/power_assert".freeze
  s.licenses = ["BSD-2-Clause".freeze, "Ruby".freeze]
  s.rdoc_options = ["--main".freeze, "README.md".freeze]
  s.rubygems_version = "3.3.5".freeze
  s.summary = "Power Assert for Ruby".freeze

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_development_dependency(%q<test-unit>.freeze, [">= 0"])
    s.add_development_dependency(%q<rake>.freeze, [">= 0"])
    s.add_development_dependency(%q<simplecov>.freeze, [">= 0"])
    s.add_development_dependency(%q<bundler>.freeze, [">= 0"])
    s.add_development_dependency(%q<irb>.freeze, [">= 1.3.1"])
    s.add_development_dependency(%q<byebug>.freeze, [">= 0"])
    s.add_development_dependency(%q<benchmark-ips>.freeze, [">= 0"])
  else
    s.add_dependency(%q<test-unit>.freeze, [">= 0"])
    s.add_dependency(%q<rake>.freeze, [">= 0"])
    s.add_dependency(%q<simplecov>.freeze, [">= 0"])
    s.add_dependency(%q<bundler>.freeze, [">= 0"])
    s.add_dependency(%q<irb>.freeze, [">= 1.3.1"])
    s.add_dependency(%q<byebug>.freeze, [">= 0"])
    s.add_dependency(%q<benchmark-ips>.freeze, [">= 0"])
  end
end
