package org.encryfoundation.prismlang.parser

import fastparse._
import NoWhitespace._
import org.encryfoundation.prismlang.core.Ast

object Lexer {

  val keywords: Set[String] = Set(
    "and",
    "not",
    "true",
    "return",
    "elif",
    "or",
    "else",
    "if",
    "contract",
    "match",
    "in",
    "let",
    "is",
    "false",
    "def",
    "lamb",
    "struct"
  )


  /** Parses all whitespace, excluding newlines. This is only
    * really useful in e.g. {} blocks, where we want to avoid
    * capturing newlines so semicolon-inference would work */

  def WS[_: P]: P[Unit] = P( NoCut(NoTrace((Basic.WSChars | Comment).rep)))

  /** Parses whitespace, including newlines.
    * This is the default for most things */

  def Semi[_: P]: P[Unit] = P( WS ~ Basic.Semi )
  def Semis[_: P]: P[Unit] = P( Semi.rep(1) ~ WS)

  def LineB[_: P]: P[Unit] = P( WS ~ Basic.Linebreak )
  def LineBreak[_: P]: P[Unit] = P( LineB.rep(1) ~ WS)

  def NotNewline[_: P]: P0 = P( &( WS ~ !Basic.Newline ) )
  def OneNLMax[_: P]: P0 = {
    def ConsumeComments = P( (Basic.WSChars.? ~ Comment ~ Basic.WSChars.? ~ Basic.Newline).rep )
    P( NoCut( WS ~ Basic.Newline.? ~ ConsumeComments ~ NotNewline) )
  }

  // Comments cannot have cuts in them, because they appear before every
  // terminal node. That means that a comment before any terminal will
  // prevent any backtracking from working, which is not what we want!
  def CommentChunk[_: P]: P[Unit] = P( CharsWhile(c => c != '/' && c != '*') | MultilineComment | !"*/" ~ AnyChar )
  def MultilineComment[_: P]: P0 = P( "/*" ~/ CommentChunk.rep ~ "*/" )
  def SameLineCharChunks[_: P]: P[Unit] = P( CharsWhile(c => c != '\n' && c != '\r')  | !Basic.Newline ~ AnyChar )
  def LineComment[_: P]: P[Unit] = P( "//" ~ SameLineCharChunks.rep ~ &(Basic.Newline | End) )
  def Comment[_: P]: P0 = P( MultilineComment | LineComment )

  def integer[_: P]: P[Long] =
    P(CharIn("+\\-").?.! ~ CharIn("0-9").rep(1).!).map {
      case ("-", i) => ("-" + i).toLong
      case (_, i) => i.toLong
    }

  def stringliteral[_: P]: P[String] = P( stringprefix.? ~ (longstring | shortstring) )
  def stringprefix[_: P]: P0 = P(
    "r" | "u" | "ur" | "R" | "U" | "UR" | "Ur" | "uR" | "b" | "B" | "br" | "Br" | "bR" | "BR"
  )

  def escapeseq[_: P]: P0 = P( "\\" ~ AnyChar )

  def shortstring[_: P]: P[String] = P( shortstring0("'") | shortstring0("\"") )
  def shortstring0[_: P](delimiter: String): P[String] = P( delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)
  def shortstringitem[_: P](quote: String): P0 = P( shortstringchar(quote) | escapeseq )
  def shortstringchar[_: P](quote: String): P0 = P( CharsWhile(!s"\\\n${quote(0)}".contains(_)) )

  def longstring[_: P]: P[String] = P( longstring0("'''") | longstring0("\"\"\"") )
  def longstring0[_: P](delimiter: String): P[String] = P( delimiter ~ longstringitem(delimiter).rep.! ~ delimiter)
  def longstringitem[_: P](quote: String): P0 = P( longstringchar(quote) | escapeseq | !quote ~ quote.take(1)  )
  def longstringchar[_: P](quote: String): P0 = P( CharsWhile(!s"\\${quote(0)}".contains(_)) )

  def identifier[_: P]: P[Ast.Ident] = P( (letter|"_") ~ (letter | digit | "_").rep ).!.filter(!keywords.contains(_))
    .map(Ast.Ident)

  def letter[_: P]: P[Unit] =        P( lowercase | uppercase )
  def lowercase[_: P]: P[Unit] =     P( CharIn("a-z") )
  def uppercase[_: P]: P[Unit] =     P( CharIn("A-Z") )
  def digit[_: P]: P[Unit] =         P( CharIn("0-9") )
  def hexdigit[_: P]: P0 =           P( digit | CharIn("a-f", "A-F") )

  def kwd[_: P](s: String): P[Unit] = s ~ !(letter | digit | "_")
}
