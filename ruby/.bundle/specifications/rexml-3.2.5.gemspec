# -*- encoding: utf-8 -*-
# stub: rexml 3.2.5 ruby lib

Gem::Specification.new do |s|
  s.name = "rexml".freeze
  s.version = "3.2.5"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Kouhei Sutou".freeze]
  s.bindir = "exe".freeze
  s.date = "2021-04-05"
  s.description = "An XML toolkit for Ruby".freeze
  s.email = ["kou@cozmixng.org".freeze]
  s.extra_rdoc_files = ["LICENSE.txt".freeze, "NEWS.md".freeze, "README.md".freeze, "doc/rexml/context.rdoc".freeze, "doc/rexml/tasks/rdoc/child.rdoc".freeze, "doc/rexml/tasks/rdoc/document.rdoc".freeze, "doc/rexml/tasks/rdoc/element.rdoc".freeze, "doc/rexml/tasks/rdoc/node.rdoc".freeze, "doc/rexml/tasks/rdoc/parent.rdoc".freeze, "doc/rexml/tasks/tocs/child_toc.rdoc".freeze, "doc/rexml/tasks/tocs/document_toc.rdoc".freeze, "doc/rexml/tasks/tocs/element_toc.rdoc".freeze, "doc/rexml/tasks/tocs/master_toc.rdoc".freeze, "doc/rexml/tasks/tocs/node_toc.rdoc".freeze, "doc/rexml/tasks/tocs/parent_toc.rdoc".freeze]
  s.files = ["LICENSE.txt".freeze, "NEWS.md".freeze, "README.md".freeze, "doc/rexml/context.rdoc".freeze, "doc/rexml/tasks/rdoc/child.rdoc".freeze, "doc/rexml/tasks/rdoc/document.rdoc".freeze, "doc/rexml/tasks/rdoc/element.rdoc".freeze, "doc/rexml/tasks/rdoc/node.rdoc".freeze, "doc/rexml/tasks/rdoc/parent.rdoc".freeze, "doc/rexml/tasks/tocs/child_toc.rdoc".freeze, "doc/rexml/tasks/tocs/document_toc.rdoc".freeze, "doc/rexml/tasks/tocs/element_toc.rdoc".freeze, "doc/rexml/tasks/tocs/master_toc.rdoc".freeze, "doc/rexml/tasks/tocs/node_toc.rdoc".freeze, "doc/rexml/tasks/tocs/parent_toc.rdoc".freeze, "lib/rexml.rb".freeze, "lib/rexml/attlistdecl.rb".freeze, "lib/rexml/attribute.rb".freeze, "lib/rexml/cdata.rb".freeze, "lib/rexml/child.rb".freeze, "lib/rexml/comment.rb".freeze, "lib/rexml/doctype.rb".freeze, "lib/rexml/document.rb".freeze, "lib/rexml/dtd/attlistdecl.rb".freeze, "lib/rexml/dtd/dtd.rb".freeze, "lib/rexml/dtd/elementdecl.rb".freeze, "lib/rexml/dtd/entitydecl.rb".freeze, "lib/rexml/dtd/notationdecl.rb".freeze, "lib/rexml/element.rb".freeze, "lib/rexml/encoding.rb".freeze, "lib/rexml/entity.rb".freeze, "lib/rexml/formatters/default.rb".freeze, "lib/rexml/formatters/pretty.rb".freeze, "lib/rexml/formatters/transitive.rb".freeze, "lib/rexml/functions.rb".freeze, "lib/rexml/instruction.rb".freeze, "lib/rexml/light/node.rb".freeze, "lib/rexml/namespace.rb".freeze, "lib/rexml/node.rb".freeze, "lib/rexml/output.rb".freeze, "lib/rexml/parent.rb".freeze, "lib/rexml/parseexception.rb".freeze, "lib/rexml/parsers/baseparser.rb".freeze, "lib/rexml/parsers/lightparser.rb".freeze, "lib/rexml/parsers/pullparser.rb".freeze, "lib/rexml/parsers/sax2parser.rb".freeze, "lib/rexml/parsers/streamparser.rb".freeze, "lib/rexml/parsers/treeparser.rb".freeze, "lib/rexml/parsers/ultralightparser.rb".freeze, "lib/rexml/parsers/xpathparser.rb".freeze, "lib/rexml/quickpath.rb".freeze, "lib/rexml/rexml.rb".freeze, "lib/rexml/sax2listener.rb".freeze, "lib/rexml/security.rb".freeze, "lib/rexml/source.rb".freeze, "lib/rexml/streamlistener.rb".freeze, "lib/rexml/text.rb".freeze, "lib/rexml/undefinednamespaceexception.rb".freeze, "lib/rexml/validation/relaxng.rb".freeze, "lib/rexml/validation/validation.rb".freeze, "lib/rexml/validation/validationexception.rb".freeze, "lib/rexml/xmldecl.rb".freeze, "lib/rexml/xmltokens.rb".freeze, "lib/rexml/xpath.rb".freeze, "lib/rexml/xpath_parser.rb".freeze]
  s.homepage = "https://github.com/ruby/rexml".freeze
  s.licenses = ["BSD-2-Clause".freeze]
  s.rdoc_options = ["--main".freeze, "README.md".freeze]
  s.rubygems_version = "3.3.5".freeze
  s.summary = "An XML toolkit for Ruby".freeze

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_development_dependency(%q<bundler>.freeze, [">= 0"])
    s.add_development_dependency(%q<rake>.freeze, [">= 0"])
    s.add_development_dependency(%q<test-unit>.freeze, [">= 0"])
  else
    s.add_dependency(%q<bundler>.freeze, [">= 0"])
    s.add_dependency(%q<rake>.freeze, [">= 0"])
    s.add_dependency(%q<test-unit>.freeze, [">= 0"])
  end
end
