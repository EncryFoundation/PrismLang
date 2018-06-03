package org.encryfoundation.prismlang.parser

import fastparse.all._

object Basic {

  val WSChars = P( CharsWhileIn("\u0020\u0009") )
  val Newline = P( StringIn("\r\n", "\n") )
  val Semi = P( ";" | Newline.rep(1) )
}
