package org.encryfoundation.prismlang.parser

import org.encryfoundation.prismlang.core.Types
import org.scalatest.{Matchers, PropSpec}

class ParserTest extends PropSpec with Matchers {

  import org.encryfoundation.prismlang.core.Ast.Expr._
  import org.encryfoundation.prismlang.core.Ast._

  property("Lambda parsing") {

    val source = "lamb (a: Int, b: Int) = { a + b }"

    val expected: Expr = Lambda(
      List((Ident("a"), TypeIdent("Int", List())), (Ident("b"), TypeIdent("Int", List()))),
      Bin(Name(Ident("a")), Operator.Add, Name(Ident("b")), Types.Nit)
    )

    val parsedTry = Parser.parse(source)

    parsedTry.get

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }
}
