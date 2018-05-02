# frozen_string_literal: true
# $Id: test_dryrun.rb 57275 2017-01-07 02:14:07Z kazu $

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
