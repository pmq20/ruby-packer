# -*- encoding: utf-8 -*-
# stub: net-imap 0.3.4 ruby lib

Gem::Specification.new do |s|
  s.name = "net-imap".freeze
  s.version = "0.3.4"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "homepage_uri" => "https://github.com/ruby/net-imap", "source_code_uri" => "https://github.com/ruby/net-imap" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Shugo Maeda".freeze, "nicholas a. evans".freeze]
  s.bindir = "exe".freeze
  s.date = "2022-12-23"
  s.description = "Ruby client api for Internet Message Access Protocol".freeze
  s.email = ["shugo@ruby-lang.org".freeze, "nick@ekenosen.net".freeze]
  s.files = [".github/dependabot.yml".freeze, ".github/workflows/test.yml".freeze, ".gitignore".freeze, "Gemfile".freeze, "LICENSE.txt".freeze, "README.md".freeze, "Rakefile".freeze, "benchmarks/stringprep.yml".freeze, "benchmarks/table-regexps.yml".freeze, "docs/styles.css".freeze, "lib/net/imap.rb".freeze, "lib/net/imap/authenticators.rb".freeze, "lib/net/imap/authenticators/cram_md5.rb".freeze, "lib/net/imap/authenticators/digest_md5.rb".freeze, "lib/net/imap/authenticators/login.rb".freeze, "lib/net/imap/authenticators/plain.rb".freeze, "lib/net/imap/authenticators/xoauth2.rb".freeze, "lib/net/imap/command_data.rb".freeze, "lib/net/imap/data_encoding.rb".freeze, "lib/net/imap/errors.rb".freeze, "lib/net/imap/flags.rb".freeze, "lib/net/imap/response_data.rb".freeze, "lib/net/imap/response_parser.rb".freeze, "lib/net/imap/sasl.rb".freeze, "lib/net/imap/sasl/saslprep.rb".freeze, "lib/net/imap/sasl/saslprep_tables.rb".freeze, "lib/net/imap/sasl/stringprep.rb".freeze, "lib/net/imap/sasl/stringprep_tables.rb".freeze, "net-imap.gemspec".freeze, "rakelib/rdoc.rake".freeze, "rakelib/rfcs.rake".freeze, "rakelib/saslprep.rake".freeze, "rakelib/string_prep_tables_generator.rb".freeze]
  s.homepage = "https://github.com/ruby/net-imap".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.6.0".freeze)
  s.rubygems_version = "3.3.5".freeze
  s.summary = "Ruby client api for Internet Message Access Protocol".freeze

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_runtime_dependency(%q<net-protocol>.freeze, [">= 0"])
    s.add_runtime_dependency(%q<date>.freeze, [">= 0"])
    s.add_development_dependency(%q<digest>.freeze, [">= 0"])
    s.add_development_dependency(%q<strscan>.freeze, [">= 0"])
  else
    s.add_dependency(%q<net-protocol>.freeze, [">= 0"])
    s.add_dependency(%q<date>.freeze, [">= 0"])
    s.add_dependency(%q<digest>.freeze, [">= 0"])
    s.add_dependency(%q<strscan>.freeze, [">= 0"])
  end
end
