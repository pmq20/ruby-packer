# frozen_string_literal: false
# $Id: extconf.rb 53143 2015-12-16 05:31:54Z naruse $

require 'mkmf'
have_func('rb_block_call', 'ruby/ruby.h')
create_makefile 'racc/cparse'
