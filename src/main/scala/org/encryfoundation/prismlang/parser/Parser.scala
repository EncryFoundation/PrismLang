package org.encryfoundation.prismlang.parser

import fastparse._
import org.encryfoundation.prismlang.core.Ast
import scala.util.{Failure, Success, Try}

object Parser {

  implicit def whitespace(cfg: P[_]): P[Unit] = Lexer.WS(cfg)

  def module[_: P]: P[Ast.Module] = P(Expressions.module ~ End)

  def parseModule(source: String): Try[Ast.Module] = parse(source, module(_)) match {
    case r: Parsed.Success[Ast.Module] => Success(r.value)
    case e: Parsed.Failure =>
      val position: String = e.extra.input.prettyIndex(e.index)
      val errorMsg: String = Parsed.Failure.formatTrailing(e.extra.input, e.index)
      val result: String = errorMsg.replaceAll("\\\\n", "")
      Failure(new Exception(s"Parsing failed on line $position. Invalid input is: $result"))
  }
}
