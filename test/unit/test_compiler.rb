require "minitest/autorun"
require "compiler"
require "tempfile"

class TestCompiler < Minitest::Test
  def setup
    @source_root = File.expand_path "../../..", __FILE__

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

  def test_squashfs_to_c
    c_output = nil

    Tempfile.open "squashfs" do |squashfs_io|
      0.upto 255 do |i|
        squashfs_io << i.chr
      end

      squashfs_io.flush

      Tempfile.open "c" do |c_io|
        @compiler.squashfs_to_c squashfs_io.path, c_io.path

        c_io.rewind

        c_output = c_io.read
      end
    end

    expected = <<-EXPECTED
#include <stdint.h>
#include <stddef.h>

const uint8_t enclose_io_memfs[256] = { 0
,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101
,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202
,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255
};

    EXPECTED

    assert_equal expected, c_output
  end
end
