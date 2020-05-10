require "minitest/autorun"
require "open3"

class TestRoundTrip < Minitest::Test
  def setup
    @rubyc = File.expand_path "../../../rubyc", __FILE__

    assert File.exist?(@rubyc),
           "could not find `rubyc` at #{@rubyc}, did you run `rake test:round-trip`?"

    @env = {
      "ENCLOSE_IO_ALWAYS_USE_ORIGINAL_RUBY" => "1",
      "ENCLOSE_IO_USE_ORIGINAL_RUBY"        => "1",
    }
  end

  ##
  # Run original ruby inside rubyc

  def ruby(*args)
    Open3.popen3(@env, @rubyc, *args) do |stdin, stdout, stderr, wait_thr|
      stdin.close

      status = wait_thr.value

      flunk <<-MESSAGE unless status.success?
failed to run ruby #{args.join " "}

-------- <stdout> ----------
#{stdout.read}
-------- <stdout> ----------

-------- <stderr> ----------
#{stderr.read}
-------- <stderr> ----------
      MESSAGE

      return [stdout.read, stderr.read]
    end
  end

  ##
  # Run rubyc

  def rubyc(*args)
    Open3.popen3(@rubyc, *args) do |stdin, stdout, stderr, wait_thr|
      stdin.close

      status = wait_thr.value

      flunk <<-MESSAGE unless status.success?
failed to run rubyc #{args.join " "}

-------- <stdout> ----------
#{stdout.read}
-------- <stdout> ----------

-------- <stderr> ----------
#{stderr.read}
-------- <stderr> ----------
      MESSAGE

      return [stdout.read, stderr.read]
    end
  end

  def test_help
    out, = rubyc "--help"

    assert_match "Compiling your Ruby application into a single executable.",
                 out,
                 "missing --help string"
  end

  def test_unit_tests
    ruby "-e", "ARGV.replace %w[test:unit]; load '/__enclose_io_memfs__/bin/rake'"
  end
end
