package org.encryfoundation.prismlang.parser

import fastparse.all._
import fastparse.{all, core}
import org.encryfoundation.prismlang.core.Ast

object WsApi extends fastparse.WhitespaceApi.Wrapper(Lexer.WS)

object Lexer {

  // TODO: Remove redundant
  val keywords = Set(
    "and",       "not",       "true",      "return",
    "elif",      "abort",     "or",
    "assert",    "else",      "if",
    "case",      "unlock",    "print",
    "match",     "exec",      "in",
    "let",       "is",        "false",
    "def",       "lamb",      "pass",
  )

  /**
    * Parses all whitespace, excluding newlines. This is only
    * really useful in e.g. {} blocks, where we want to avoid
    * capturing newlines so semicolon-inference would work
    */
  val WS: Parser[Unit] = P( NoCut(NoTrace((Basic.WSChars | Comment).rep)) )

  /**
    * Parses whitespace, including newlines.
    * This is the default for most things
    */
  val WL0: Parser[Unit] = P( NoTrace((Basic.WSChars | Comment | Basic.Newline).rep) )(sourcecode.Name("WL"))
  val WL: Parser[Unit] = P( NoCut(WL0) )

  val Semi: Parser[Unit] = P( WS ~ Basic.Semi )
  val Semis: Parser[Unit] = P( Semi.rep(1) ~ WS )
  val Newline: Parser[Unit] = P( WL ~ Basic.Newline )

  val NotNewline: P0 = P( &( WS ~ !Basic.Newline ) )
  val OneNLMax: P0 = {
    val ConsumeComments = P( (Basic.WSChars.? ~ Comment ~ Basic.WSChars.? ~ Basic.Newline).rep )
    P( NoCut( WS ~ Basic.Newline.? ~ ConsumeComments ~ NotNewline) )
  }

  // Comments cannot have cuts in them, because they appear before every
  // terminal node. That means that a comment before any terminal will
  // prevent any backtracking from working, which is not what we want!
  val CommentChunk: Parser[Unit] = P( CharsWhile(c => c != '/' && c != '*') | MultilineComment | !"*/" ~ AnyChar )
  lazy val MultilineComment: P0 = P( "/*" ~/ CommentChunk.rep ~ "*/" )
  val SameLineCharChunks: Parser[Unit] = P( CharsWhile(c => c != '\n' && c != '\r')  | !Basic.Newline ~ AnyChar )
  val LineComment: Parser[Unit] = P( "//" ~ SameLineCharChunks.rep ~ &(Basic.Newline | End) )
  lazy val Comment: P0 = P( MultilineComment | LineComment )

  def negatable[T](p: P[T])(implicit ev: Numeric[T]): core.Parser[T, Char, String] = (("+" | "-").?.! ~ p).map {
    case ("-", i) => ev.negate(i)
    case (_, i) => i
  }

  val integer: core.Parser[Long, Char, String] = negatable[Long](P( CharIn('0' to '9').rep(min = 1).!.map(_.toLong) ))

  lazy val stringliteral: P[String] = P( stringprefix.? ~ (longstring | shortstring) )
  lazy val stringprefix: P0 = P(
    "r" | "u" | "ur" | "R" | "U" | "UR" | "Ur" | "uR" | "b" | "B" | "br" | "Br" | "bR" | "BR"
  )

  val escapeseq: P0 = P( "\\" ~ AnyChar )

  val shortstring: P[String] = P( shortstring0("'") | shortstring0("\"") )
  def shortstring0(delimiter: String): all.Parser[String] = P( delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)
  def shortstringitem(quote: String): P0 = P( shortstringchar(quote) | escapeseq )
  def shortstringchar(quote: String): P0 = P( CharsWhile(!s"\\\n${quote(0)}".contains(_)) )

  lazy val longstring: P[String] = P( longstring0("'''") | longstring0("\"\"\"") )
  def longstring0(delimiter: String): all.Parser[String] = P( delimiter ~ longstringitem(delimiter).rep.! ~ delimiter)
  def longstringitem(quote: String): P0 = P( longstringchar(quote) | escapeseq | !quote ~ quote.take(1)  )
  def longstringchar(quote: String): P0 = P( CharsWhile(!s"\\${quote(0)}".contains(_)) )

  val identifier: P[Ast.Ident] = P( (letter|"_") ~ (letter | digit | "_").rep ).!.filter(!keywords.contains(_))
    .map(Ast.Ident)

  lazy val letter: all.Parser[Unit] =        P( lowercase | uppercase )
  lazy val lowercase: all.Parser[Unit] =     P( CharIn('a' to 'z') )
  lazy val uppercase: all.Parser[Unit] =     P( CharIn('A' to 'Z') )
  lazy val digit: all.Parser[Unit] =         P( CharIn('0' to '9') )

  def kwd(s: String): core.Parser[Unit, Char, String] = s ~ !(letter | digit | "_")
}
