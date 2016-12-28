# -*- encoding: utf-8 -*-
# stub: xmlrpc 0.2.1 ruby lib

Gem::Specification.new do |s|
  s.name = "xmlrpc"
  s.version = "0.2.1"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib"]
  s.authors = ["SHIBATA Hiroshi"]
  s.bindir = "exe"
  s.date = "2016-12-22"
  s.description = "XMLRPC is a lightweight protocol that enables remote procedure calls over HTTP."
  s.email = ["hsbt@ruby-lang.org"]
  s.files = [".gitignore", ".travis.yml", "Gemfile", "LICENSE.txt", "README.md", "Rakefile", "bin/console", "bin/setup", "lib/xmlrpc.rb", "lib/xmlrpc/base64.rb", "lib/xmlrpc/client.rb", "lib/xmlrpc/config.rb", "lib/xmlrpc/create.rb", "lib/xmlrpc/datetime.rb", "lib/xmlrpc/marshal.rb", "lib/xmlrpc/parser.rb", "lib/xmlrpc/server.rb", "lib/xmlrpc/utils.rb", "xmlrpc.gemspec"]
  s.homepage = "https://github.com/ruby/xmlrpc"
  s.licenses = ["Ruby"]
  s.required_ruby_version = Gem::Requirement.new(">= 2.4.0dev")
  s.rubygems_version = "2.5.1"
  s.summary = "XMLRPC is a lightweight protocol that enables remote procedure calls over HTTP."

  if s.respond_to? :specification_version then
    s.specification_version = 4

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_development_dependency(%q<bundler>, [">= 0"])
      s.add_development_dependency(%q<rake>, [">= 0"])
      s.add_development_dependency(%q<test-unit>, [">= 0"])
    else
      s.add_dependency(%q<bundler>, [">= 0"])
      s.add_dependency(%q<rake>, [">= 0"])
      s.add_dependency(%q<test-unit>, [">= 0"])
    end
  else
    s.add_dependency(%q<bundler>, [">= 0"])
    s.add_dependency(%q<rake>, [">= 0"])
    s.add_dependency(%q<test-unit>, [">= 0"])
  end
end
