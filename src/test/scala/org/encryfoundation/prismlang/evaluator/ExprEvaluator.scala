package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.core.Ast.Expr.Contract
import org.encryfoundation.prismlang.core.TypeSystem

import scala.util.Try

trait ExprEvaluator {

  def eval(expr: Expr): Try[expr.tpe.Underlying] =
    Try(Evaluator(ScopedRuntimeEnvironment.empty(1), TypeSystem.default).eval[expr.tpe.Underlying](expr))
}
