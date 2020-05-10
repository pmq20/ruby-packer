# -*- encoding: utf-8 -*-
# stub: test-unit 3.3.4 ruby lib

Gem::Specification.new do |s|
  s.name = "test-unit".freeze
  s.version = "3.3.4"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "source_code_uri" => "https://github.com/test-unit/test-unit" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Kouhei Sutou".freeze, "Haruka Yoshihara".freeze]
  s.date = "2019-09-29"
  s.description = "test-unit (Test::Unit) is unit testing framework for Ruby, based on xUnit\nprinciples. These were originally designed by Kent Beck, creator of extreme\nprogramming software development methodology, for Smalltalk's SUnit. It allows\nwriting tests, checking results and automated testing in Ruby.\n".freeze
  s.email = ["kou@cozmixng.org".freeze, "yoshihara@clear-code.com".freeze]
  s.files = ["COPYING".freeze, "GPL".freeze, "LGPL".freeze, "PSFL".freeze, "README.md".freeze, "Rakefile".freeze, "doc/text/getting-started.md".freeze, "doc/text/how-to.md".freeze, "doc/text/news.md".freeze, "lib/test-unit.rb".freeze, "lib/test/unit.rb".freeze, "lib/test/unit/assertion-failed-error.rb".freeze, "lib/test/unit/assertions.rb".freeze, "lib/test/unit/attribute-matcher.rb".freeze, "lib/test/unit/attribute.rb".freeze, "lib/test/unit/auto-runner-loader.rb".freeze, "lib/test/unit/autorunner.rb".freeze, "lib/test/unit/code-snippet-fetcher.rb".freeze, "lib/test/unit/collector.rb".freeze, "lib/test/unit/collector/descendant.rb".freeze, "lib/test/unit/collector/dir.rb".freeze, "lib/test/unit/collector/load.rb".freeze, "lib/test/unit/collector/objectspace.rb".freeze, "lib/test/unit/collector/xml.rb".freeze, "lib/test/unit/color-scheme.rb".freeze, "lib/test/unit/color.rb".freeze, "lib/test/unit/data-sets.rb".freeze, "lib/test/unit/data.rb".freeze, "lib/test/unit/diff.rb".freeze, "lib/test/unit/error.rb".freeze, "lib/test/unit/exception-handler.rb".freeze, "lib/test/unit/failure.rb".freeze, "lib/test/unit/fault-location-detector.rb".freeze, "lib/test/unit/fixture.rb".freeze, "lib/test/unit/notification.rb".freeze, "lib/test/unit/omission.rb".freeze, "lib/test/unit/pending.rb".freeze, "lib/test/unit/priority.rb".freeze, "lib/test/unit/runner/console.rb".freeze, "lib/test/unit/runner/emacs.rb".freeze, "lib/test/unit/runner/xml.rb".freeze, "lib/test/unit/test-suite-creator.rb".freeze, "lib/test/unit/testcase.rb".freeze, "lib/test/unit/testresult.rb".freeze, "lib/test/unit/testsuite.rb".freeze, "lib/test/unit/ui/console/outputlevel.rb".freeze, "lib/test/unit/ui/console/testrunner.rb".freeze, "lib/test/unit/ui/emacs/testrunner.rb".freeze, "lib/test/unit/ui/testrunner.rb".freeze, "lib/test/unit/ui/testrunnermediator.rb".freeze, "lib/test/unit/ui/testrunnerutilities.rb".freeze, "lib/test/unit/ui/xml/testrunner.rb".freeze, "lib/test/unit/util/backtracefilter.rb".freeze, "lib/test/unit/util/method-owner-finder.rb".freeze, "lib/test/unit/util/observable.rb".freeze, "lib/test/unit/util/output.rb".freeze, "lib/test/unit/util/procwrapper.rb".freeze, "lib/test/unit/version.rb".freeze, "sample/adder.rb".freeze, "sample/subtracter.rb".freeze, "sample/test_adder.rb".freeze, "sample/test_subtracter.rb".freeze, "sample/test_user.rb".freeze, "test/collector/test-descendant.rb".freeze, "test/collector/test-load.rb".freeze, "test/collector/test_dir.rb".freeze, "test/collector/test_objectspace.rb".freeze, "test/fixtures/header-label.csv".freeze, "test/fixtures/header-label.tsv".freeze, "test/fixtures/header.csv".freeze, "test/fixtures/header.tsv".freeze, "test/fixtures/no-header.csv".freeze, "test/fixtures/no-header.tsv".freeze, "test/fixtures/plus.csv".freeze, "test/run-test.rb".freeze, "test/test-assertions.rb".freeze, "test/test-attribute-matcher.rb".freeze, "test/test-attribute.rb".freeze, "test/test-code-snippet.rb".freeze, "test/test-color-scheme.rb".freeze, "test/test-color.rb".freeze, "test/test-data.rb".freeze, "test/test-diff.rb".freeze, "test/test-emacs-runner.rb".freeze, "test/test-error.rb".freeze, "test/test-failure.rb".freeze, "test/test-fault-location-detector.rb".freeze, "test/test-fixture.rb".freeze, "test/test-notification.rb".freeze, "test/test-omission.rb".freeze, "test/test-pending.rb".freeze, "test/test-priority.rb".freeze, "test/test-test-case.rb".freeze, "test/test-test-result.rb".freeze, "test/test-test-suite-creator.rb".freeze, "test/test-test-suite.rb".freeze, "test/testunit-test-util.rb".freeze, "test/ui/test_testrunmediator.rb".freeze, "test/util/test-method-owner-finder.rb".freeze, "test/util/test-output.rb".freeze, "test/util/test_backtracefilter.rb".freeze, "test/util/test_observable.rb".freeze, "test/util/test_procwrapper.rb".freeze]
  s.homepage = "http://test-unit.github.io/".freeze
  s.licenses = ["Ruby".freeze, "PSFL".freeze]
  s.rubygems_version = "3.1.2".freeze
  s.summary = "An xUnit family unit testing framework for Ruby.".freeze
  s.test_files = ["test/util/test_procwrapper.rb".freeze, "test/util/test_observable.rb".freeze, "test/util/test-output.rb".freeze, "test/util/test-method-owner-finder.rb".freeze, "test/util/test_backtracefilter.rb".freeze, "test/test-diff.rb".freeze, "test/test-assertions.rb".freeze, "test/testunit-test-util.rb".freeze, "test/run-test.rb".freeze, "test/test-emacs-runner.rb".freeze, "test/test-color.rb".freeze, "test/test-pending.rb".freeze, "test/test-omission.rb".freeze, "test/test-color-scheme.rb".freeze, "test/test-test-result.rb".freeze, "test/test-test-suite.rb".freeze, "test/test-code-snippet.rb".freeze, "test/test-test-case.rb".freeze, "test/test-failure.rb".freeze, "test/fixtures/no-header.csv".freeze, "test/fixtures/header-label.tsv".freeze, "test/fixtures/no-header.tsv".freeze, "test/fixtures/header.csv".freeze, "test/fixtures/header-label.csv".freeze, "test/fixtures/plus.csv".freeze, "test/fixtures/header.tsv".freeze, "test/ui/test_testrunmediator.rb".freeze, "test/test-fixture.rb".freeze, "test/test-priority.rb".freeze, "test/test-test-suite-creator.rb".freeze, "test/test-attribute.rb".freeze, "test/test-notification.rb".freeze, "test/test-attribute-matcher.rb".freeze, "test/test-fault-location-detector.rb".freeze, "test/collector/test-descendant.rb".freeze, "test/collector/test_objectspace.rb".freeze, "test/collector/test-load.rb".freeze, "test/collector/test_dir.rb".freeze, "test/test-error.rb".freeze, "test/test-data.rb".freeze]

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_runtime_dependency(%q<power_assert>.freeze, [">= 0"])
    s.add_development_dependency(%q<bundler>.freeze, [">= 0"])
    s.add_development_dependency(%q<rake>.freeze, [">= 0"])
    s.add_development_dependency(%q<yard>.freeze, [">= 0"])
    s.add_development_dependency(%q<kramdown>.freeze, [">= 0"])
    s.add_development_dependency(%q<packnga>.freeze, [">= 0"])
  else
    s.add_dependency(%q<power_assert>.freeze, [">= 0"])
    s.add_dependency(%q<bundler>.freeze, [">= 0"])
    s.add_dependency(%q<rake>.freeze, [">= 0"])
    s.add_dependency(%q<yard>.freeze, [">= 0"])
    s.add_dependency(%q<kramdown>.freeze, [">= 0"])
    s.add_dependency(%q<packnga>.freeze, [">= 0"])
  end
end
