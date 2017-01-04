# Copyright (c) 2016-2017 Minqi Pan <pmq2001@gmail.com>
# 
# This file is part of Ruby Compiler, distributed under the MIT License
# For full terms see the included LICENSE file

require 'shellwords'
require 'tmpdir'
require 'fileutils'
require 'json'
require 'open3'

module Ruby
  class Compiler
    class Test
      def initialize(tmpdir, options = {})
        Utils.prepare_tmpdir(tmpdir)
        @vendor_ruby = File.join(tmpdir, 'ruby')
        @options = options
      end
      
      def run!
        Utils.chdir(@vendor_ruby) do
          Utils.run("make test TESTOPTS=--color=never")
          Utils.run("make test-all TESTOPTS='-q -j3 --color=never --job-status=normal'")
          Utils.run("make test-rubyspec MSPECOPT=-fm")
        end
      end
    end
  end
end
