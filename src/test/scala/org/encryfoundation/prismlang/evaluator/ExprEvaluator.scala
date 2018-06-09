package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.core.Ast.Expr

import scala.util.Try

trait ExprEvaluator {

  def eval(expr: Expr): Try[expr.tpe.Underlying] =
    Try(Evaluator.default.eval[expr.tpe.Underlying](expr))
}
