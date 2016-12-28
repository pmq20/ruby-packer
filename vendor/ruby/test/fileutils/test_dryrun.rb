# frozen_string_literal: false
# $Id: test_dryrun.rb 53141 2015-12-16 05:07:31Z naruse $

require 'fileutils'
require 'test/unit'
require_relative 'visibility_tests'

class TestFileUtilsDryRun < Test::Unit::TestCase

  include FileUtils::DryRun
  include TestFileUtils::Visibility

  def setup
    super
    @fu_module = FileUtils::DryRun
  end

end
