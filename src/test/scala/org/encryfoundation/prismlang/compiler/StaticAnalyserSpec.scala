package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.core.{TypeSystem, Types}
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

  property("Conditional assignment") {

    val analyser: StaticAnalyser = StaticAnalyser(TypeSystem.default)

    val expr: Expr = Let(
      Ident("age"),
      If(
        Compare(
          IntConst(3),
          List(CompOp.Gt),
          List(IntConst(0))
        ),
        Block(List(True)),
        Block(List(False))
      ),
      Some(TypeIdent("Bool", List()))
    )

    val analyseTry = Try(analyser.scan(expr))

    analyseTry.isSuccess shouldBe true
  }

  property("Collection constant creation") {

    val analyser: StaticAnalyser = StaticAnalyser(TypeSystem.default)

    val expr: Expr = Let(
        Ident("coll"),
        Collection(
          List(
            IntConst(1),
            IntConst(2),
            IntConst(4),
            IntConst(8),
            IntConst(16),
            IntConst(32),
            IntConst(64),
            IntConst(128),
            IntConst(256),
            IntConst(512),
            IntConst(1024)
          ),
          Types.Nit
        ),
        Some(TypeIdent("Array", List("Int")))
      )

    val analyseTry = Try(analyser.scan(expr))

    analyseTry.isSuccess shouldBe true
  }
}
