# This file is part of grecs
# Copyright (C) 2007, 2009-2014 Sergey Poznyakoff
#
# Grecs is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# Grecs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Grecs.  If not, see <http://www.gnu.org/licenses/>.

BEGIN {
  if (since)
    split(since,since_a,"-")
}

function timeok(t,   a) {
	if (!since)
		return 1
	split(t,a,"-")
	if (a[1] < since_a[1])
		return 0
	if (a[1] > since_a[1])
		return 1
	if (a[2] < since_a[2])
		return 0
	if (a[2] > since_a[2])
		return 1
	return a[3] > since_a[3]
}
      
/^[0-9]+ .* +<[^>]+>/ {
	s = strftime("%F", $1)
	if (!timeok(s))
	  exit
	sub(/^[0-9]+ +/,"")
	if (s == datestr && author == $0)
		next
	datestr = s
	author = $0
	if (runlen) { runlen = 0; print "" }
	printf("%s  %s\n", datestr, author)
	next
}
/^Signed-off-by:/ { next }
/^<unknown>$/ { next }
NF==0 {
	runlen++
	next
}
{ if (runlen) { runlen = 0; print "" }
  print "\t" $0 }

END {
	if (append) {
		print ""
		while ((getline < append) > 0) {
			if (match($0, /^Local *Variables:/))
				break
			print
		}
	}
	print "\f"
	# Make sure Emacs won't recognize this line:
	print "Local", "Variables:"
	print "mode: change-log"
	print "version-control: never"
	print "buffer-read-only: t"
	print "End:"
}
