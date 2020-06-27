# frozen_string_literal: true

require_relative '../test_helper'
require 'open3'

class TestRoundTrip < Minitest::Test
  def setup
    @rubyc = File.expand_path '../../rubyc', __dir__

    assert File.exist?(@rubyc),
           "could not find `rubyc` at #{@rubyc}, did you run `rake test:round-trip`?"

    @env = {
      'ENCLOSE_IO_USE_ORIGINAL_RUBY' => '1'
    }
  end

  ##
  # Run original ruby inside rubyc

  def ruby(*args)
    Bundler.with_unbundled_env do
      Open3.popen3(@env, @rubyc, *args) do |stdin, stdout, stderr, wait_thr|
        stdin.close

        Thread.new do
          until (raw_line = stdout.gets).nil?
            parsed_line = Hash[timestamp: Time.now, line: raw_line.to_s]
            STDOUT.puts "rubyc's ruby STDOUT: #{parsed_line}"
          end
        end

        Thread.new do
          until (raw_line = stderr.gets).nil?
            parsed_line = Hash[timestamp: Time.now, line: raw_line.to_s]
            warn "rubyc's ruby STDERR: #{parsed_line}"
          end
        end

        status = wait_thr.value

        flunk <<~MESSAGE unless status.success?
          failed to run ruby #{args.join ' '}
          
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
  end

  ##
  # Run rubyc

  def rubyc(*args)
    Bundler.with_unbundled_env do
      Open3.popen3(@rubyc, *args) do |stdin, stdout, stderr, wait_thr|
        stdin.close

        status = wait_thr.value

        flunk <<~MESSAGE unless status.success?
          failed to run rubyc #{args.join ' '}
          
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
  end

  def test_help
    out, = rubyc '--help'

    assert_match 'Compiling your Ruby application into a single executable.',
                 out,
                 'missing --help string'
  end

  def test_ruby_version
    out_itself, = rubyc '--version'
    out, = rubyc '--ruby-version'
    assert_match out_itself.strip[0..-2], out.strip, 'incorrect ruby version'
  end

  def test_unit_tests
    ruby '-e', "Dir.chdir('/__enclose_io_memfs__/local'); ARGV.replace %w[test:unit]; load '/__enclose_io_memfs__/bin/rake'"
  end

  def test_enclosed_ruby
    puts "TODO: RUN RUBY TEST FOR #{ENV['ENCLOSE_IO_RUBYC_TEST_ENCLOSED_RUBY'].inspect}"
  end
end
