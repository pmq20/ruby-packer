# $Id: strip-comment.rb 25189 2009-10-02 12:04:37Z akr $

require 'ripper/filter'

class CommentStripper < Ripper::Filter
  def CommentStripper.strip(src)
    new(src).parse(nil)
  end

  def on_default(event, token, data)
    print token
  end

  def on_comment(token, data)
    puts
  end
end

CommentStripper.strip(ARGF)
