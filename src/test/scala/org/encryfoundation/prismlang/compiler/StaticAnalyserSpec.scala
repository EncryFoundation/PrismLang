package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.core.TypeSystem
import org.encryfoundation.prismlang.parser.Parser
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class StaticAnalyserSpec extends PropSpec with Matchers with Parser {

  import org.encryfoundation.prismlang.core.Ast.Expr._
  import org.encryfoundation.prismlang.core.Ast._

  property("Let") {

    val analyser: StaticAnalyser = StaticAnalyser(TypeSystem.default)

    val expr: Expr = Let(Ident("age"), IntConst(28), Some(TypeIdent("Int", List())))

    val analyseTry = Try(analyser.scan(expr))

    analyseTry.isSuccess shouldBe true
  }

  property("BinOp") {

    val analyser: StaticAnalyser = StaticAnalyser(TypeSystem.default)

    val expr: Expr = Let(
      Ident("age"),
      Block(
        List(
          Let(
            Ident("sum"),
            IntConst(30),
            None
          ),
          Bin(
            Name(Ident("sum")),
            Operator.Sub,
            IntConst(10)
          )
        )
      ),
      Some(TypeIdent("Int", List()))
    )

    val analyseTry = Try(analyser.scan(expr))

    analyseTry.isSuccess shouldBe true
  }
}
