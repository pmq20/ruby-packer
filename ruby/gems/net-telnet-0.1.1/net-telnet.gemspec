# -*- encoding: utf-8 -*-
# stub: net-telnet 0.1.1 ruby lib

Gem::Specification.new do |s|
  s.name = "net-telnet"
  s.version = "0.1.1"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib"]
  s.authors = ["SHIBATA Hiroshi"]
  s.bindir = "exe"
  s.date = "2015-04-21"
  s.description = "Provides telnet client functionality."
  s.email = ["hsbt@ruby-lang.org"]
  s.files = [".gitignore", ".travis.yml", "Gemfile", "LICENSE.txt", "README.md", "Rakefile", "bin/console", "bin/setup", "lib/net-telnet.rb", "lib/net/telnet.rb", "lib/net/telnet/version.rb", "net-telnet.gemspec"]
  s.homepage = "https://github.com/ruby/net-telnet"
  s.rubygems_version = "2.4.5"
  s.summary = "Provides telnet client functionality."

  if s.respond_to? :specification_version then
    s.specification_version = 4

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_development_dependency(%q<bundler>, ["~> 1.9"])
      s.add_development_dependency(%q<rake>, ["~> 10.0"])
    else
      s.add_dependency(%q<bundler>, ["~> 1.9"])
      s.add_dependency(%q<rake>, ["~> 10.0"])
    end
  else
    s.add_dependency(%q<bundler>, ["~> 1.9"])
    s.add_dependency(%q<rake>, ["~> 10.0"])
  end
end
