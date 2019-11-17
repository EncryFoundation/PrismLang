package org.encryfoundation.prismlang.parser

import fastparse._, NoWhitespace._

object Basic {

  def WSChars[_: P]: P[Unit] = P( CharsWhileIn("\u0020\u0009") )
  def Newline[_: P]: P[Unit] = P( StringIn("\r\n", "\n") )
  def Semi[_: P]: P[Unit] = P( ";" | Newline.rep(1) )
  def Linebreak[_: P]: P[Unit] = P( Newline.rep(1) )
}
