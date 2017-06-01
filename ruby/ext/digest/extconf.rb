# frozen_string_literal: false
# $RoughId: extconf.rb,v 1.6 2001/07/13 15:38:27 knu Exp $
# $Id: extconf.rb 53143 2015-12-16 05:31:54Z naruse $

require "mkmf"

$INSTALLFILES = {
  "digest.h" => "$(HDRDIR)"
}

create_makefile("digest")
