package org.encryfoundation.prismlang.parser

import fastparse.all._
import org.encryfoundation.prismlang.core.Ast

import scala.util.{Failure, Success, Try}

object Parser {

  def parse(source: String): Try[Ast.Expr] = ( Expressions.expr ~ End ).parse(source) match {
    case r: Parsed.Success[Ast.Expr] => Success(r.value)
    case e: Parsed.Failure => Failure(new Exception(e.msg))
  }
}
