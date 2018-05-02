require "minitest/autorun"
require "compiler"

class TestCompiler < Minitest::Test
  def test_class_ruby_api_version
    assert_equal "2.5.0", Compiler::ruby_api_version
  end
end
