# frozen_string_literal: true

require_relative '../test_helper'

require 'open3'

class TestRoundTrip < Minitest::Test
  def setup
    @rubyc = File.expand_path (Gem.win_platform? ? '../../rubyc.exe' : '../../rubyc'), __dir__

    assert File.exist?(@rubyc),
           "Cannot find rubyc at #{@rubyc}, did you run `rake test:roundtrip`?"

    @env = {
      'ENCLOSE_IO_USE_ORIGINAL_RUBY' => 'true'
    }
  end

  ##
  # Run original ruby inside rubyc

  def ruby(*args)
    Dir.mktmpdir do |dir|
      Dir.chdir(dir) do
        FileUtils.cp(@rubyc, (Gem.win_platform? ? "#{dir}/rubyc.exe" : "#{dir}/rubyc"))

        Bundler.with_unbundled_env do
          Open3.popen3(@env, (Gem.win_platform? ? "#{dir}/rubyc.exe" : "#{dir}/rubyc"), *args) do |stdin, stdout, stderr, wait_thr|
            stdin.close

            Thread.new do
              until (raw_line = stdout.gets).nil?
                parsed_line = Hash[timestamp: Time.now, line: raw_line.to_s]
                $stdout.puts "rubyc's ruby STDOUT: #{parsed_line}"
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
    end
  end

  ##
  # Run rubyc

  def rubyc(*args)
    Dir.mktmpdir do |dir|
      Dir.chdir(dir) do
        FileUtils.cp(@rubyc, (Gem.win_platform? ? "#{dir}/rubyc.exe" : "#{dir}/rubyc"))

        Bundler.with_unbundled_env do
          Open3.popen3((Gem.win_platform? ? "#{dir}/rubyc.exe" : "#{dir}/rubyc"), *args) do |stdin, stdout, stderr, wait_thr|
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
    assert_match out_itself.strip.split('.')[0..-2].join('.'), out.strip, 'incorrect ruby version'
  end

  def test_unit_tests
    ruby '-e', "Dir.chdir('/__enclose_io_memfs__/local'); ARGV.replace %w[test:unit]; load '/__enclose_io_memfs__/bin/rake'"
  end

  def test_enclosed_ruby
    # TODO: RUN RUBY TESTS
  end
end
