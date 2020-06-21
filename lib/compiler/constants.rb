# frozen_string_literal: true

# Copyright (c) 2017 - 2020 Minqi Pan et al.
#
# This file is part of Ruby Packer, distributed under the MIT License
# For full terms see the included LICENSE file

class Compiler
  VERSION = '2.7.1a'
  PRJ_ROOT = File.expand_path('../..', __dir__)
  VENDOR_DIR = File.expand_path('vendor', PRJ_ROOT)
  MEMFS = '/__enclose_io_memfs__'
end
