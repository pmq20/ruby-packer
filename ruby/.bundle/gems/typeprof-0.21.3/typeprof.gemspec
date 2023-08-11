require_relative 'lib/typeprof/version'

Gem::Specification.new do |spec|
  spec.name          = "typeprof" # temporal
  spec.version       = TypeProf::VERSION
  spec.authors       = ["Yusuke Endoh"]
  spec.email         = ["mame@ruby-lang.org"]

  spec.summary       = %q{TypeProf is a type analysis tool for Ruby code based on abstract interpretation}
  spec.description   = <<~EOD
    TypeProf performs a type analysis of non-annotated Ruby code.

    It abstractly executes input Ruby code in a level of types instead of values, gathers what types are passed to and returned by methods, and prints the analysis result in RBS format, a standard type description format for Ruby 3.0.

    This tool is planned to be bundled with Ruby 3.0.
  EOD
  spec.homepage      = "https://github.com/ruby/typeprof"
  spec.license       = "MIT"
  spec.required_ruby_version = Gem::Requirement.new(">= 2.7")

  spec.metadata["homepage_uri"] = spec.homepage
  spec.metadata["source_code_uri"] = "https://github.com/ruby/typeprof"

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  spec.files         = Dir.chdir(File.expand_path('..', __FILE__)) do
    `git ls-files -z`.split("\x0").reject { |f| f.match(%r{^(doc|test|spec|features|smoke|testbed|vscode)/}) }
  end
  spec.bindir        = "exe"
  spec.executables   = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]

  spec.add_runtime_dependency "rbs", ">= 1.8.1"
end
