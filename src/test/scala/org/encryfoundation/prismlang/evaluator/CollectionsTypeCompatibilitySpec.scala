package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.ValueGenerator
import org.encryfoundation.prismlang.core.Ast.Expr.{ByteConst, IntConst, Str, _}
import org.encryfoundation.prismlang.core.Ast._
import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec, TryValues}

class CollectionsTypeCompatibilitySpec extends PropSpec
  with Matchers
  with TableDrivenPropertyChecks
  with ExprChecker
  with TryValues {

  val valueTypes = Table("valueTypes", "Str", "IntConst", "ByteConst", "Base16Str", "Base58Str", "Bool")

  def checkConsistencyBound(expr: List[Expr] => Expr): Unit = {
    forAll(valueTypes) { valueType1 =>
      forAll(valueTypes) { valueType2 =>
        whenever(valueType1 != valueType2) {
          val (value1, _) = ValueGenerator.genValue(valueType1)
          val (value2, _) = ValueGenerator.genValue(valueType2)
          checkExpr(expr(List(value1, value2)), List("SemanticAnalysisException"))
        }
      }
    }
  }

  def checkCollection(expr: List[Expr] => Expr): Unit = {
    (1 to 300).foreach { n =>
      forAll(valueTypes) { valueType =>
        val (exVal, v) = ValueGenerator.genValue(valueType)
        val exprValues: List[Expr] = List.fill(n)(exVal)
        val expectedValues: List[Any] = List.fill(n)(v)
        val compile = compileExpr(expr(exprValues))
        val result = eval(compile.get).get
        result shouldBe expectedValues
      }
    }
  }

  property("collection should be contains elements") {
    checkCollection(values => Collection(values))
  }

  property("collection should be consistent") {
    checkConsistencyBound(values => Collection(values))
  }


}
