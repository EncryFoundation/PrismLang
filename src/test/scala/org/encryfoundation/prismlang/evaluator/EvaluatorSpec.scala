package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.compiler.ExprCompiler
import org.encryfoundation.prismlang.core.Ast.Expr.{Bin, IntConst}
import org.encryfoundation.prismlang.core.Ast.{CompOp, Expr, Operator}
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class EvaluatorSpec extends PropSpec with Matchers with ExprCompiler with ExprEvaluator {

  property("BinOp") {

    val expr: Expr = Bin(
      IntConst(100),
      Operator.Div,
      IntConst(4)
    )

    val resultTry: Try[Any] = eval(compile(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldEqual 25
  }

  property("Compare") {

    val expr: Expr = Expr.Compare(
      IntConst(3),
      List(CompOp.Gt),
      List(IntConst(0))
    )

    val resultTry: Try[Any] = eval(compile(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe true
  }
}
