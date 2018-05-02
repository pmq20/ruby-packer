# frozen_string_literal: true
# $Id: test_nowrite.rb 57275 2017-01-07 02:14:07Z kazu $

require 'fileutils'
require 'test/unit'
require_relative 'visibility_tests'

class TestFileUtilsNoWrite < Test::Unit::TestCase

  include FileUtils::NoWrite
  include TestFileUtils::Visibility

  def setup
    super
    @fu_module = FileUtils::NoWrite
  end

end
