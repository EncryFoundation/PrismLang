package org.encryfoundation.prismlang.integration

import org.encryfoundation.prismlang.compiler.TestCompiler
import org.encryfoundation.prismlang.core.Ast
import org.encryfoundation.prismlang.evaluator.ExprEvaluator
import org.encryfoundation.prismlang.parser.Parser
import scala.util.{Failure, Random, Success, Try}

trait Utils extends TestCompiler with Parser with ExprEvaluator {

  def getArrayString(sample: List[Any]): String = sample.mkString("Array(", ", ", ")")

  def compiled(prismScript: String): Try[Ast.Expr] = parse(prismScript) match {
    case Success(parsed) => compileExpr(parsed.head)
    case Failure(e) => Failure[Ast.Expr](e)
  }

  def generateRandomString(length: Int): String = (Random.alphanumeric take length).mkString
}
