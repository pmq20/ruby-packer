# Copyright (c) 2016-2017 Minqi Pan
# 
# This file is part of Ruby Compiler, distributed under the MIT License
# For full terms see the included LICENSE file

require "spec_helper"

tmpdir = File.expand_path("rubyc/compiler_spec", Dir.tmpdir)

describe ::Ruby::Compiler do
  it "has a version number" do
    expect(::Ruby::Compiler::VERSION).not_to be nil
  end

  it "passes all original and our tests" do
    x = ::Ruby::Compiler::Test.new(tmpdir)
    x.run!
  end
end
