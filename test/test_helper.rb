# frozen_string_literal: true

require 'rubygems'
require 'bundler/setup'
Bundler.require(:default)

require 'minitest/autorun'

# distinguish outputs by different reporters
if ENV['ENCLOSE_IO_USE_ORIGINAL_RUBY']
  Minitest::Reporters.use! Minitest::Reporters::DefaultReporter.new
else
  Minitest::Reporters.use! Minitest::Reporters::SpecReporter.new
end
