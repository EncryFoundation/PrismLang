package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.compiler.ExprCompiler
import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast._
import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.Types.Nit
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

  property("Map") {

    val expr: Expr = Call(
      Attribute(
        Collection(
          List(
            IntConst(1),
            IntConst(2),
            IntConst(3)
          ),
          Types.Nit
        ),
        Ident("map"),
        Types.Nit
      ),
      List(
        Lambda(
          List(
            (Ident("a"), TypeIdent("Int",List()))
          ),
          Bin(
            Name(Ident("a"), Types.Nit),
            Operator.Mult,
            IntConst(2)
          ),
          Types.Nit
        )
      ),
      Types.Nit
    )

    val resultTry: Try[Any] = eval(compile(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe List(2, 4, 6)
  }
}
