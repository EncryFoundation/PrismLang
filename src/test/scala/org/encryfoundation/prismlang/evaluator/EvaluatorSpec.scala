package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.compiler.TestCompiler
import org.encryfoundation.prismlang.core.Ast.Expr.{Compare, _}
import org.encryfoundation.prismlang.core.Ast._
import org.encryfoundation.prismlang.core.Types
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class EvaluatorSpec extends PropSpec with Matchers with TestCompiler with ExprEvaluator {

  property("BinOp") {

    val expr: Expr = Bin(
      IntConst(100),
      Operator.Div,
      IntConst(4)
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldEqual 25
  }

  property("Compare") {

    val expr: Expr = Expr.Compare(
      IntConst(3),
      List(CompOp.Gt),
      List(IntConst(0))
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe true
  }

  property("Int to Byte") {

    val expr: Expr = Attribute(
      IntConst(7),
      Ident("toByte"),
      Types.Nit
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldEqual 7.toByte
  }

  property("Compare Byte/Int") {

    val expr: Expr = Expr.Compare(
      Attribute(
        IntConst(7),
        Ident("toByte"),
        Types.Nit
      ),
      List(CompOp.Gt),
      List(IntConst(5))
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe true
  }

  property("Compare.In") {

    val expr: Expr = Compare(
      IntConst(2),
      List(CompOp.In),
      List(
        Collection(
          List(
            IntConst(1),
            IntConst(2),
            IntConst(3),
            IntConst(4)
          ),
          Types.Nit
        )
      )
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe true
  }

  property("Compare.NotIn") {

    val expr: Expr = Compare(
      IntConst(2),
      List(CompOp.NotIn),
      List(
        Collection(
          List(
            IntConst(1),
            IntConst(2),
            IntConst(3),
            IntConst(4)
          ),
          Types.Nit
        )
      )
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe false
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

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe List(2, 4, 6)
  }

  property("Filter") {

    val expr: Expr = Call(
      Attribute(
        Collection(
          List(IntConst(1), IntConst(2), IntConst(3), IntConst(4), IntConst(5)),
          Types.Nit
        ),
        Ident("filter"),
        Types.Nit
      ),
      List(
        Lambda(
          List(
            (Ident("i"), TypeIdent("Int", List()))
          ),
          Compare(
            Name(Ident("i"), Types.Nit),
            List(CompOp.Gt),
            List(IntConst(3))
          ),
          Types.Nit
        )
      ),
      Types.Nit
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe List(4, 5)
  }

  property("Filter (empty result)") {

    val expr: Expr = Call(
      Attribute(
        Collection(
          List(IntConst(1), IntConst(2)),
          Types.Nit
        ),
        Ident("filter"),
        Types.Nit
      ),
      List(
        Lambda(
          List(
            (Ident("i"), TypeIdent("Int", List()))
          ),
          Compare(
            Name(Ident("i"), Types.Nit),
            List(CompOp.Gt),
            List(IntConst(3))
          ),
          Types.Nit
        )
      ),
      Types.Nit
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe List()
  }

  property("IfElse") {

    val expr: Expr = If(
      Compare(
        IntConst(5),
        List(CompOp.Gt),
        List(IntConst(10))
      ),
      Block(List(True)),
      Block(List(False))
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe false
  }

  property("allOf()") {

    val expr: Expr = Call(
      Name(Ident("allOf"), Types.Nit),
      List(
        Collection(
          List(
            Compare(
              IntConst(2),
              List(CompOp.Gt),
              List(IntConst(1))
            ),
            Compare(
              IntConst(1),
              List(CompOp.Eq),
              List(IntConst(1))
            ),
            Compare(
              IntConst(3),
              List(CompOp.NotEq),
              List(IntConst(4))
            ),
            Compare(
              IntConst(10),
              List(CompOp.Lt),
              List(IntConst(100))
            ),
            True
          ), Types.Nit
        )
      ), Types.Nit
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe true
  }


  property("anyOf()") {

    val expr: Expr = Call(
      Name(Ident("anyOf"), Types.Nit),
      List(
        Collection(
          List(
            Compare(
              IntConst(2),
              List(CompOp.Gt),
              List(IntConst(1))
            ),
            Compare(
              IntConst(1),
              List(CompOp.NotEq),
              List(IntConst(1))
            ),
            Compare(
              IntConst(3),
              List(CompOp.NotEq),
              List(IntConst(4))
            ),
            Compare(
              IntConst(10),
              List(CompOp.Gt),
              List(IntConst(100))
            ),
            True
          ), Types.Nit
        )
      ), Types.Nit
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe true
  }
}
