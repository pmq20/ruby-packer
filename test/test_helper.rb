# frozen_string_literal: true

require 'rubygems'
require 'bundler/setup'
require 'minitest/autorun'
require 'minitest/reporters'

$LOAD_PATH.unshift File.expand_path('../lib', __dir__)

# distinguish outputs by different reporters
if ENV['ENCLOSE_IO_USE_ORIGINAL_RUBY']
  Minitest::Reporters.use! Minitest::Reporters::DefaultReporter.new
else
  Minitest::Reporters.use! Minitest::Reporters::SpecReporter.new
end
