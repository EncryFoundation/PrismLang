package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.compiler.TestCompiler
import org.encryfoundation.prismlang.core.Ast.Expr.{Compare, _}
import org.encryfoundation.prismlang.core.Ast._
import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.Types.{PFunc, PInt}
import org.scalatest.{Matchers, PropSpec}

import scala.util.{Failure, Try}

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

  property("UnaryOp Not") {

    val expr: Expr = Unary(
      UnaryOp.Not,
      True
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldEqual false
  }

  property("UnaryOp Invert") {

    val expr: Expr = Unary(
      UnaryOp.Invert,
      IntConst(10)
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldEqual -10
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

  property("Map on Array of Arrays") {

    val expr: Expr = Call(
      Attribute(
        Collection(
          List(
            Collection(List(IntConst(1), IntConst(2), IntConst(3))),
            Collection(List(IntConst(2), IntConst(3), IntConst(4))),
            Collection(List(IntConst(3), IntConst(4), IntConst(5)))
          ),
          Types.Nit
        ),
        Ident("map"),
        Types.Nit
      ),
      List(
        Lambda(
          List(
            (Ident("list"), TypeIdent("Array", List(TypeIdent("Int"))))
          ),
          Sum(Name(Ident("list"), Types.Nit)),
          Types.Nit
        )
      ),
      Types.Nit
    )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe List(6, 9, 12)
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

  property("Collection") {

    val expr: Expr =
      Collection(
        List(
          IntConst(1),
          IntConst(2)
        ))

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe List(1, 2)
  }

  property("Tuple") {

    val expr: Expr =
      Tuple(
        List(
          Str("qwe"),
          IntConst(1)
        ))

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe List("qwe", 1)
  }

  property("Nested") {
    val expr: Expr =
      Block(
        List(
          Def(
            Ident("nested0"),
            List(
              (Ident("arg"), TypeIdent("Int", List()))
            ),
            Block(
              List(
                Def(
                  Ident("nested1"),
                  List(
                    (Ident("arg1"), TypeIdent("Int", List()))
                  ),
                  Block(
                    List(
                      Name(Ident("arg1"), PInt)
                    )
                  )
                  ,
                  TypeIdent("Int",List())
                ),
                Call(
                  Name(Ident("nested1")),
                  List(
                    Name(Ident("arg"), PInt)
                  ),
                  PInt
                )
              )
            ),
            TypeIdent("Int", List())
          ),
          Call(
            Name(Ident("nested0")),
            List(
              IntConst(2)
            ),
            PInt
          )
        )
      )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe 2
  }

  property("Same name different params number (second func)") {

    val expr: Expr =
      Block(
        List(
          Def(
            Ident("function"),
            List(
              (Ident("arg"), TypeIdent("Int", List()))
            ),
            Block(
              List(
                IntConst(2)
              )
            ),
            TypeIdent("Int", List())
          ),
          Def(
            Ident("function"),
            List(
              (Ident("arg"), TypeIdent("Int", List())),
              (Ident("arg1"), TypeIdent("Int", List()))
            ),
            Block(
              List(
                IntConst(3)
              )
            ),
            TypeIdent("Int", List())
          ),
          Def(
            Ident("function"),
            List(
              (Ident("arg"), TypeIdent("Int", List())),
              (Ident("arg1"), TypeIdent("Int", List())),
              (Ident("arg2"), TypeIdent("Int", List()))
            ),
            Block(
              List(
                IntConst(4)
              )
            ),
            TypeIdent("Int", List())
          ),
          Call(
            Name(Ident("function")),
            List(
              IntConst(1),
              IntConst(1)
            ),
            PInt
          )
        )
      )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe 3
  }

  property("Same name different params number (first func)") {
    val expr: Expr =
      Block(
        List(
          Def(
            Ident("function"),
            List(
              (Ident("arg"), TypeIdent("Int", List()))
            ),
            Block(
              List(
                IntConst(2)
              )
            ),
            TypeIdent("Int", List())
          ),
          Def(
            Ident("function"),
            List(
              (Ident("arg"), TypeIdent("Int", List())),
              (Ident("arg1"), TypeIdent("Int", List()))
            ),
            Block(
              List(
                IntConst(3)
              )
            ),
            TypeIdent("Int", List())
          ),
          Def(
            Ident("function"),
            List(
              (Ident("arg"), TypeIdent("Int", List())),
              (Ident("arg1"), TypeIdent("Int", List())),
              (Ident("arg2"), TypeIdent("Int", List()))
            ),
            Block(
              List(
                IntConst(4)
              )
            ),
            TypeIdent("Int", List())
          ),
          Call(
            Name(Ident("function")),
            List(
              IntConst(1)
            ),
            PInt
          )
        )
      )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe 2
  }

  property("Same name different params number (third func)") {

    val expr: Expr =
      Block(
        List(
          Def(
            Ident("function"),
            List(
              (Ident("arg"), TypeIdent("Int", List()))
            ),
            Block(
              List(
                IntConst(2)
              )
            ),
            TypeIdent("Int", List())
          ),
          Def(
            Ident("function"),
            List(
              (Ident("arg"), TypeIdent("Int", List())),
              (Ident("arg1"), TypeIdent("Int", List()))
            ),
            Block(
              List(
                IntConst(3)
              )
            ),
            TypeIdent("Int", List())
          ),
          Def(
            Ident("function"),
            List(
              (Ident("arg"), TypeIdent("Int", List())),
              (Ident("arg1"), TypeIdent("Int", List())),
              (Ident("arg2"), TypeIdent("Int", List()))
            ),
            Block(
              List(
                IntConst(4)
              )
            ),
            TypeIdent("Int", List())
          ),
          Call(
            Name(Ident("function")),
            List(
              IntConst(1),
              IntConst(1),
              IntConst(1)
            ),
            PInt
          )
        )
      )

    val resultTry: Try[Any] = eval(compileExpr(expr).get)

    resultTry.isSuccess shouldBe true

    resultTry.get shouldBe 4
  }
}
