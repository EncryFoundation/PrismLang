package org.encryfoundation.prismlang.parser

import fastparse.all
import fastparse.all._

object Basic {

  val WSChars: all.Parser[Unit] = P( CharsWhileIn("\u0020\u0009") )
  val Newline: all.Parser[Unit] = P( StringIn("\r\n", "\n") )
  val Semi: all.Parser[Unit] = P( ";" | Newline.rep(1) )
  val Linebreak: all.Parser[Unit] = P( Newline.rep(1) )
}
