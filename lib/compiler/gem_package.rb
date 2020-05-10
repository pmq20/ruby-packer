# Copyright (c) 2017 - 2020 Ruby Packer authors (Minqi Pan et al.)
# 
# This file is part of Node.js Compiler, distributed under the MIT License
# For full terms see the included LICENSE file

require "compiler/error"
require 'shellwords'
require 'tmpdir'
require 'fileutils'
require 'json'
require 'open-uri'

class Compiler
  class GemPackage
    attr_reader :work_dir

    def initialize(entrance, options, utils)
      @options = options
      @module_name = options[:gem]
      @module_version = options[:gem_version]
      unless @module_name && @module_version
        raise Error, "Please provide both --gem and --gem-version"
      end
      unless entrance
        raise Error, "Please provide an entrance."
      end
      @work_dir = File.expand_path("#{@module_name}-#{@module_version}", options[:tmpdir])
      @utils = utils
    end

    def stuff_tmpdir
      @utils.rm_rf(@work_dir)
      @utils.mkdir_p(@work_dir)
      @utils.chdir(@work_dir) do
        url = "http://rubygems.org/downloads/#{@module_name}-#{@module_version}.gem"
        STDERR.puts "Downloading #{url}" unless @options[:quiet]
        download = open(url)
        IO.copy_stream(download, "#{@module_name}-#{@module_version}.gem")
      end
    end
  end
end
