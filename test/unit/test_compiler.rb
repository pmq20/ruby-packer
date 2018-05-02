require "minitest/autorun"
require "compiler"

class TestCompiler < Minitest::Test
  def setup
    @source_root = File.expand_path "../..", __FILE__

    capture_io do
      @compiler = Compiler.new "bogus"
    end
  end

  def test_class_ruby_api_version
    assert_equal "2.5.0", Compiler::ruby_api_version
  end

  def test_initialize
    assert_equal "bogus", @compiler.entrance

    expected_output = File.expand_path(Compiler::DEFAULT_NAME, @source_root)

    options = @compiler.options

    assert_equal "-j4",           options.delete(:make_args)
    assert_equal expected_output, options.delete(:output)
    assert_match "rubyc",         options.delete(:tmpdir)
  end

  def test_log
    out, err = capture_io do
      @compiler.log "hi"
    end

    assert_empty out
    assert_equal "hi\n", err
  end

  def test_log_quiet
    @compiler.options[:quiet] = true

    assert_silent do
      @compiler.log "hi"
    end
  end
end
