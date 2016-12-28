# frozen_string_literal: false
# $Id: test_nowrite.rb 53141 2015-12-16 05:07:31Z naruse $

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
