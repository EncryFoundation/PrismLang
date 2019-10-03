package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.compiler.TestCompiler
import org.encryfoundation.prismlang.core.Ast

import scala.util.{Failure, Success}

trait ExprChecker extends TestCompiler with ExprEvaluator {

  def checkExpr(expr: Ast.Expr, expectedExceptions: List[String]) {
    val astExpr = compileExpr(expr)

    val throwable = astExpr match {
      case Success(expr) =>
        val evaluation = eval(expr)
        assert(evaluation.isFailure, s"Expression '${expr.toString}' shouldn't compile")
        evaluation match {
          case Failure(e) => e
        }
      case Failure(e) => e
    }

    assert(expectedExceptions.contains(throwable.getClass.getSimpleName),
      s"${throwable.getClass.getSimpleName} should be  ${expectedExceptions.mkString}")
  }

}
