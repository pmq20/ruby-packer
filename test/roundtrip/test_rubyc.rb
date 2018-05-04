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

  def ruby(*args)
    Open3.popen3(@env, @rubyc, *args) do |stdin, stdout, stderr, wait_thr|
      stdin.close

      status = wait_thr.value

      flunk "failed to run ruby #{args.join " "}" unless status.success?

      return [stdout.read, stderr.read]
    end
  end

  def rubyc(*args)
    Open3.popen3(@rubyc, *args) do |stdin, stdout, stderr, wait_thr|
      stdin.close

      status = wait_thr.value

      flunk "failed to run rubyc #{args.join " "}" unless status.success?

      return [stdout.read, stderr.read]
    end
  end

  def test_help
    out, = rubyc "--help"

    assert_match "Compiling your Ruby application into a single executable.",
                 out,
                 "missing --help string"
  end

  def test_ruby_version
    out, = rubyc "--ruby-version"

    assert_match "2.5.1", out, "incorrect ruby version"
  end

  def test_unit_tests
    out, err = ruby "/__enclose_io_memfs__/bin/rake", "test:unit"

  rescue
    $stderr.puts "rake test stdout:"
    $stderr.puts out
    $stderr.puts "rake test stderr:"
    $stderr.puts err
  end
end
