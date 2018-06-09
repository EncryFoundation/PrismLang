package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.compiler.ExprCompiler
import org.encryfoundation.prismlang.core.Ast.Expr.{Compare, _}
import org.encryfoundation.prismlang.core.Ast._
import org.encryfoundation.prismlang.core.Types
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class EvaluatorSpec extends PropSpec with Matchers with ExprCompiler with ExprEvaluator {

  implicit def int2intConst = IntConst(_)
  implicit def str2ident = Ident(_)

  property("BinOp") {

    val expr: Expr = Bin( 100L, Operator.Div, 4L)

    val resultTry: Try[Any] = compile(expr).flatMap(eval)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldEqual 25
  }

  property("Compare") {

    val expr: Expr = Expr.Compare(3L, List(CompOp.Gt), List(0L))

    val resultTry: Try[Any] = compile(expr).flatMap(eval)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe true
  }

  property("Map") {

    val expr: Expr = Call(
      Attribute(
        Collection(
          List[IntConst](1L, 2L, 3L),
          Types.Nit
        ), "map",
        Types.Nit
      ),
      List(
        Lambda(
          List( Ident("a") -> TypeIdent("Int",List())),
          Bin( Name("a", Types.Nit), Operator.Mult, 2L),
          Types.Nit
        )
      ),
      Types.Nit
    )

    val resultTry: Try[Any] = compile(expr).flatMap(eval)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe List(2, 4, 6)
  }

  property("IfElse") {

    val expr: Expr = If(
      Compare( 5L, List(CompOp.Gt), List(10L)),
      Block(List(True)),
      Block(List(False))
    )

    val resultTry: Try[Any] = compile(expr).flatMap(eval)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe false
  }
}
