package org.encryfoundation.prismlang.parser

import org.encryfoundation.prismlang.core.Types
import org.scalatest.{Matchers, PropSpec}

import scala.collection.mutable.ArrayBuffer

class ParserTest extends PropSpec with Matchers with Parser {

  import org.encryfoundation.prismlang.core.Ast.Expr._
  import org.encryfoundation.prismlang.core.Ast._

  property("Lambda parsing") {

    val source = "lamb (a: Int, b: Int) = a + b"

    val expected: Expr = Lambda(
      List((Ident("a"), TypeIdent("Int", List())), (Ident("b"), TypeIdent("Int", List()))),
      Bin(Name(Ident("a")), Operator.Add, Name(Ident("b")), Types.Nit)
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Lambda parsing (with block)") {

    val source = "lamb (a: Int, b: Int) = { a + b }"

    val expected: Expr = Lambda(
      List((Ident("a"), TypeIdent("Int", List())), (Ident("b"), TypeIdent("Int", List()))),
      Block(List(Bin(Name(Ident("a")), Operator.Add, Name(Ident("b")), Types.Nit)))
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  // TODO
  property("Lambda parsing (with if)") {

    val source = "lamb (a: Int, b: Int) = if (a > b) b else a"

    val expected: Seq[Expr] = ArrayBuffer(Lambda(
      List((Ident("a"), TypeIdent("Int", List())), (Ident("b"), TypeIdent("Int", List()))),
      Block(List(Bin(Name(Ident("a")), Operator.Add, Name(Ident("b")), Types.Nit)))
    ))

    val parsedTry = parse(source)

    println(parsedTry.get)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Let (explicit type annotation)") {

    val source = "let age: Int = 28"

    val expected: Seq[Expr] = ArrayBuffer(Let(Ident("age"), IntConst(28), Some(TypeIdent("Int", List()))))

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Let") {

    val source = "let age = 28"

    val expected: Seq[Expr] = ArrayBuffer(Let(Ident("age"), IntConst(28), None))

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Let (block assignment)") {

    val source = "let age = { 100 / 4 }"

    val expected: Seq[Expr] = ArrayBuffer(
      Let(
        Ident("age"),
        Block(
          List(
            Bin(
              IntConst(100),
              Operator.Div,
              IntConst(4),
              Types.Nit
            )
          )
        ),
        None
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Let (complex block assignment)") {

    val source =
      """
        |let age: Int = {
        |  let sum = 30 + 40
        |  sum - 10
        |}
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      Let(
        Ident("age"),
        Block(
          List(
            Let(
              Ident("sum"),
              Bin(IntConst(30), Operator.Add, IntConst(40), Types.Nit),
              None
            ),
            Bin(
              Name(Ident("sum")),
              Operator.Sub,
              IntConst(10),
              Types.Nit
            )
          )
        ),
        Some(TypeIdent("Int", List()))
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Def") {

    val source =
      """
        |def sum(a: Int, b: Int): Int = {
        |  a + b
        |}
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      Def(
        Ident("sum"),
        List(
          (Ident("a"), TypeIdent("Int",List())),
          (Ident("b"), TypeIdent("Int",List()))
        ),
        Block(
          List(
            Bin(
              Name(Ident("a")),
              Operator.Add,
              Name(Ident("b")),
              Types.Nit)
          )
        ),
        TypeIdent("Int",List())
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }
}
