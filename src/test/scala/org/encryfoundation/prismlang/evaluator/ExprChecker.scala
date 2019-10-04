package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.compiler.TestCompiler
import org.encryfoundation.prismlang.core.Ast.Expr

import scala.util.{Failure, Success}

trait ExprChecker extends TestCompiler with ExprEvaluator {

  def checkExprForExceptions(expr: Expr, expectedExceptions: List[String]) {
    val throwable = compileExpr(expr) match {
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
