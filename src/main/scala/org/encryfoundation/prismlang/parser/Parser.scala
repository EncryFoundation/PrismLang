package org.encryfoundation.prismlang.parser

import fastparse._
import org.encryfoundation.prismlang.core.Ast
import scala.util.{Failure, Success, Try}

object Parser {

  implicit def whitespace(cfg: P[_]): P[Unit] = Lexer.WS(cfg)

  def expr[_: P]: P[Ast.Expr] = P(Expressions.expr ~ End)
  def module[_: P]: P[Ast.Module] = P(Expressions.module ~ End)

  def parseExpr(source: String): Try[Ast.Expr] = parse(source, expr(_)) match {
    case r: Parsed.Success[Ast.Expr] => Success(r.value)
    case e: Parsed.Failure => Failure(new Exception(s"Parsing failed: ${e.msg}"))
  }

  def parseModule(source: String): Try[Ast.Module] = parse(source, module(_)) match {
    case r: Parsed.Success[Ast.Module] => Success(r.value)
    case e: Parsed.Failure => Failure(new Exception(s"Parsing failed: ${e.msg}"))
  }
}
