# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'power_assert/version'

Gem::Specification.new do |s|
  s.name        = 'power_assert'
  s.version     = PowerAssert::VERSION
  s.authors     = ['Kazuki Tsujimoto']
  s.email       = ['kazuki@callcc.net']
  s.homepage    = 'https://github.com/ruby/power_assert'
  s.summary     = "Power Assert for Ruby"
  s.description = "Power Assert shows each value of variables and method calls in the expression. It is useful for testing, providing which value wasn't correct when the condition is not satisfied."

  s.files         = `git ls-files -z`.split("\x0").reject do |f|
    f.match(%r{\A(?:test|spec|features|benchmark|bin)/})
  end
  s.bindir        = 'exe'
  s.executables   = s.files.grep(%r{^exe/}) { |f| File.basename(f) }
  s.require_paths = ['lib']
  s.add_development_dependency 'test-unit'
  s.add_development_dependency 'rake'
  s.add_development_dependency 'simplecov'
  s.add_development_dependency 'bundler'
  s.add_development_dependency 'irb', '>= 1.3.1'
  s.add_development_dependency 'byebug'
  s.add_development_dependency 'benchmark-ips'
  s.extra_rdoc_files = ['README.md']
  s.rdoc_options     = ['--main', 'README.md']
  s.licenses         = ['BSD-2-Clause', "Ruby"]
end
