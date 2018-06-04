package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.core.TypeSystem
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class StaticAnalyserSpec extends PropSpec with Matchers {

  import org.encryfoundation.prismlang.core.Ast.Expr._
  import org.encryfoundation.prismlang.core.Ast._

  property("Let") {

    val analyser: StaticAnalyser = StaticAnalyser(TypeSystem.default)

    val expr: Expr = Let(Ident("age"), IntConst(28), Some(TypeIdent("Int", List())))

    val analyseTry = Try(analyser.scan(expr))

    analyseTry.isSuccess shouldBe true
  }
}
