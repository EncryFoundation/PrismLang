package org.encryfoundation.prismlang.parser

import fastparse.all._
import org.encryfoundation.prismlang.core.Ast

import scala.util.{Failure, Success, Try}

trait Parser {

  def parse(source: String): Try[Seq[Ast.Expr]] = ( Expressions.fileInput ~ End ).parse(source) match {
    case r: Parsed.Success[Seq[Ast.Expr]] => Success(r.value)
    case e: Parsed.Failure => Failure(new Exception(e.msg))
  }

  def parseModule(source: String): Try[Ast.Module] = ( Expressions.module ~ End ).parse(source) match {
    case r: Parsed.Success[Ast.Module] => Success(r.value)
    case e: Parsed.Failure => Failure(new Exception(e.msg))
  }
}
