# frozen_string_literal: true
# $Id: test_verbose.rb 57275 2017-01-07 02:14:07Z kazu $

require 'test/unit'
require 'fileutils'
require_relative 'visibility_tests'

class TestFileUtilsVerbose < Test::Unit::TestCase

  include FileUtils::Verbose
  include TestFileUtils::Visibility

  def setup
    super
    @fu_module = FileUtils::Verbose
  end

end
