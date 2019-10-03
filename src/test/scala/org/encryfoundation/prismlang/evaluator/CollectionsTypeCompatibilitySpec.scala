package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast._
import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec}

class CollectionsTypeCompatibilitySpec extends PropSpec
  with Matchers
  with TableDrivenPropertyChecks
  with ExprChecker {

  val values = Table("values", Str("10"), IntConst(10), ByteConst(10), Base16Str("10"), Base58Str("10"), True)

  property("collection should be consistent") {
    checkConsistency(values => Collection(values))
  }

  def checkConsistency[T](expr: (List[Expr]) => Expr): Unit = {
    forAll(values) { value1 =>
      forAll(values) { value2 =>
        whenever(value1 != value2) {
          checkExpr(expr(List(value1, value2)), List("SemanticAnalysisException"))
        }
      }
    }
  }

}
