package org.encryfoundation.prismlang.integration

import org.encryfoundation.prismlang.compiler.TestCompiler
import org.encryfoundation.prismlang.core.Ast
import org.encryfoundation.prismlang.parser.Parser
import scala.util.{Failure, Success, Try}

trait Utils extends TestCompiler with Parser {

  def getArrayString(sample: List[Any]): String = sample.mkString("Array(", ", ", ")")

  def compiled(prismScript: String): Try[Ast.Expr] = parse(prismScript) match {
      case Success(parsed) => compileExpr(parsed.head)
      case Failure(e) => Failure[Ast.Expr](e)
    }

}
