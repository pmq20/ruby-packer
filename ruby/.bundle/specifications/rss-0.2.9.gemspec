# -*- encoding: utf-8 -*-
# stub: rss 0.2.9 ruby lib

Gem::Specification.new do |s|
  s.name = "rss".freeze
  s.version = "0.2.9"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Kouhei Sutou".freeze]
  s.date = "2020-02-18"
  s.description = "Family of libraries that support various formats of XML \"feeds\".".freeze
  s.email = ["kou@cozmixng.org".freeze]
  s.files = ["Gemfile".freeze, "LICENSE.txt".freeze, "NEWS.md".freeze, "README.md".freeze, "Rakefile".freeze, "lib/rss.rb".freeze, "lib/rss/0.9.rb".freeze, "lib/rss/1.0.rb".freeze, "lib/rss/2.0.rb".freeze, "lib/rss/atom.rb".freeze, "lib/rss/content.rb".freeze, "lib/rss/content/1.0.rb".freeze, "lib/rss/content/2.0.rb".freeze, "lib/rss/converter.rb".freeze, "lib/rss/dublincore.rb".freeze, "lib/rss/dublincore/1.0.rb".freeze, "lib/rss/dublincore/2.0.rb".freeze, "lib/rss/dublincore/atom.rb".freeze, "lib/rss/image.rb".freeze, "lib/rss/itunes.rb".freeze, "lib/rss/maker.rb".freeze, "lib/rss/maker/0.9.rb".freeze, "lib/rss/maker/1.0.rb".freeze, "lib/rss/maker/2.0.rb".freeze, "lib/rss/maker/atom.rb".freeze, "lib/rss/maker/base.rb".freeze, "lib/rss/maker/content.rb".freeze, "lib/rss/maker/dublincore.rb".freeze, "lib/rss/maker/entry.rb".freeze, "lib/rss/maker/feed.rb".freeze, "lib/rss/maker/image.rb".freeze, "lib/rss/maker/itunes.rb".freeze, "lib/rss/maker/slash.rb".freeze, "lib/rss/maker/syndication.rb".freeze, "lib/rss/maker/taxonomy.rb".freeze, "lib/rss/maker/trackback.rb".freeze, "lib/rss/parser.rb".freeze, "lib/rss/rexmlparser.rb".freeze, "lib/rss/rss.rb".freeze, "lib/rss/slash.rb".freeze, "lib/rss/syndication.rb".freeze, "lib/rss/taxonomy.rb".freeze, "lib/rss/trackback.rb".freeze, "lib/rss/utils.rb".freeze, "lib/rss/version.rb".freeze, "lib/rss/xml-stylesheet.rb".freeze, "lib/rss/xml.rb".freeze, "lib/rss/xmlparser.rb".freeze, "lib/rss/xmlscanner.rb".freeze, "rss.gemspec".freeze, "test/dot.png".freeze, "test/rss-assertions.rb".freeze, "test/rss-testcase.rb".freeze, "test/run-test.rb".freeze, "test/test_1.0.rb".freeze, "test/test_2.0.rb".freeze, "test/test_accessor.rb".freeze, "test/test_atom.rb".freeze, "test/test_content.rb".freeze, "test/test_dublincore.rb".freeze, "test/test_image.rb".freeze, "test/test_inherit.rb".freeze, "test/test_itunes.rb".freeze, "test/test_maker_0.9.rb".freeze, "test/test_maker_1.0.rb".freeze, "test/test_maker_2.0.rb".freeze, "test/test_maker_atom_entry.rb".freeze, "test/test_maker_atom_feed.rb".freeze, "test/test_maker_content.rb".freeze, "test/test_maker_dc.rb".freeze, "test/test_maker_image.rb".freeze, "test/test_maker_itunes.rb".freeze, "test/test_maker_slash.rb".freeze, "test/test_maker_sy.rb".freeze, "test/test_maker_taxo.rb".freeze, "test/test_maker_trackback.rb".freeze, "test/test_maker_xml-stylesheet.rb".freeze, "test/test_parser.rb".freeze, "test/test_parser_1.0.rb".freeze, "test/test_parser_2.0.rb".freeze, "test/test_parser_atom_entry.rb".freeze, "test/test_parser_atom_feed.rb".freeze, "test/test_setup_maker_0.9.rb".freeze, "test/test_setup_maker_1.0.rb".freeze, "test/test_setup_maker_2.0.rb".freeze, "test/test_setup_maker_atom_entry.rb".freeze, "test/test_setup_maker_atom_feed.rb".freeze, "test/test_setup_maker_itunes.rb".freeze, "test/test_setup_maker_slash.rb".freeze, "test/test_slash.rb".freeze, "test/test_syndication.rb".freeze, "test/test_taxonomy.rb".freeze, "test/test_to_s.rb".freeze, "test/test_trackback.rb".freeze, "test/test_xml-stylesheet.rb".freeze]
  s.homepage = "https://github.com/ruby/rss".freeze
  s.licenses = ["BSD-2-Clause".freeze]
  s.rubygems_version = "3.3.5".freeze
  s.summary = "Family of libraries that support various formats of XML \"feeds\".".freeze
  s.test_files = ["test/test_maker_2.0.rb".freeze, "test/test_dublincore.rb".freeze, "test/test_setup_maker_slash.rb".freeze, "test/test_parser.rb".freeze, "test/test_maker_xml-stylesheet.rb".freeze, "test/test_maker_0.9.rb".freeze, "test/test_setup_maker_atom_feed.rb".freeze, "test/test_maker_image.rb".freeze, "test/test_parser_atom_entry.rb".freeze, "test/test_maker_taxo.rb".freeze, "test/run-test.rb".freeze, "test/test_itunes.rb".freeze, "test/test_maker_content.rb".freeze, "test/test_maker_dc.rb".freeze, "test/test_inherit.rb".freeze, "test/dot.png".freeze, "test/test_maker_atom_entry.rb".freeze, "test/test_setup_maker_2.0.rb".freeze, "test/test_to_s.rb".freeze, "test/test_setup_maker_itunes.rb".freeze, "test/rss-assertions.rb".freeze, "test/test_xml-stylesheet.rb".freeze, "test/test_1.0.rb".freeze, "test/test_accessor.rb".freeze, "test/test_taxonomy.rb".freeze, "test/test_maker_trackback.rb".freeze, "test/rss-testcase.rb".freeze, "test/test_slash.rb".freeze, "test/test_setup_maker_atom_entry.rb".freeze, "test/test_syndication.rb".freeze, "test/test_maker_1.0.rb".freeze, "test/test_parser_atom_feed.rb".freeze, "test/test_maker_itunes.rb".freeze, "test/test_image.rb".freeze, "test/test_setup_maker_1.0.rb".freeze, "test/test_atom.rb".freeze, "test/test_maker_slash.rb".freeze, "test/test_parser_1.0.rb".freeze, "test/test_setup_maker_0.9.rb".freeze, "test/test_2.0.rb".freeze, "test/test_trackback.rb".freeze, "test/test_content.rb".freeze, "test/test_maker_sy.rb".freeze, "test/test_parser_2.0.rb".freeze, "test/test_maker_atom_feed.rb".freeze]

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_runtime_dependency(%q<rexml>.freeze, [">= 0"])
    s.add_development_dependency(%q<bundler>.freeze, [">= 0"])
    s.add_development_dependency(%q<rake>.freeze, [">= 0"])
    s.add_development_dependency(%q<test-unit>.freeze, [">= 0"])
  else
    s.add_dependency(%q<rexml>.freeze, [">= 0"])
    s.add_dependency(%q<bundler>.freeze, [">= 0"])
    s.add_dependency(%q<rake>.freeze, [">= 0"])
    s.add_dependency(%q<test-unit>.freeze, [">= 0"])
  end
end
