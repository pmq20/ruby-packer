require "minitest/autorun"
require "compiler"

class TestCompiler < Minitest::Test
  def setup
    @source_root = File.expand_path '../..', __FILE__
  end

  def test_class_ruby_api_version
    assert_equal "2.5.0", Compiler::ruby_api_version
  end

  def test_initialize
    compiler = nil

    capture_io do
      compiler = Compiler.new "bogus"
    end

    assert_equal "bogus", compiler.entrance

    expected_output = File.expand_path(Compiler::DEFAULT_NAME, @source_root)
    options = compiler.options

    assert_equal "-j4",           options.delete(:make_args)
    assert_equal expected_output, options.delete(:output)
    assert_match "rubyc",         options.delete(:tmpdir)
  end
end
