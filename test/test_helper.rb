# frozen_string_literal: true

$LOAD_PATH.unshift File.expand_path('../lib', __dir__)

require 'minitest/autorun'
require 'minitest/reporters'

# distinguish outputs by different reporters
if ENV['ENCLOSE_IO_USE_ORIGINAL_RUBY']
  Minitest::Reporters.use! Minitest::Reporters::DefaultReporter.new
else
  Minitest::Reporters.use! Minitest::Reporters::SpecReporter.new
end
