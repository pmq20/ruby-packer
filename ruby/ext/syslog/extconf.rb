# frozen_string_literal: false
# $RoughId: extconf.rb,v 1.3 2001/11/24 17:49:26 knu Exp $
# $Id: extconf.rb 53143 2015-12-16 05:31:54Z naruse $

require 'mkmf'

have_header("syslog.h") &&
  have_func("openlog") &&
  have_func("setlogmask") &&
  create_makefile("syslog")

