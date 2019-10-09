package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.ValueGenerator
import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast._
import org.encryfoundation.prismlang.core.Constants
import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec, TryValues}

class CollectionsSpec extends PropSpec
  with Matchers
  with TableDrivenPropertyChecks
  with ExprChecker
  with TryValues {

  val valueTypes = List("Str", "IntConst", "ByteConst", "Base16Str", "Base58Str", "Bool")
  val valueTypesTable = Table("valueTypes", valueTypes: _*)

  def checkContains(valueTypes: List[String], wrapper: List[Expr] => Expr, count: Int): Unit = {
    val (exprVals, expectedVals) =
      ValueGenerator.genValueTypeList(valueTypes, count)
        .map(ValueGenerator.genRandomValue)
        .unzip

    val compile = compileExpr(wrapper(exprVals))
    val result = eval(compile.get).get
    result shouldBe expectedVals
  }

  def checkConsistencyExceptions(valueTypesTable: TableFor1[String], wrapper: List[Expr] => Expr, expectedExceptions: List[String]): Unit = {
    forAll(valueTypesTable) { valueType1 =>
      forAll(valueTypesTable) { valueType2 =>
          val (value1, _) = ValueGenerator.genRandomValue(valueType1)
          val (value2, _) = ValueGenerator.genRandomValue(valueType2)
          if(value1.tpe != value2.tpe && !exclusion(value1, value2)) {
            checkExprForExceptions(wrapper(List(value1, value2)), expectedExceptions)
        }
      }
    }
  }

  property("collection should be contains elements") {
    (1 to Constants.CollMaxLength).foreach { n =>
      forAll(valueTypesTable) { valueType =>
        checkContains(List(valueType), values => Collection(values), n)
      }
    }
  }

  property("collection should be contains at least 1 element") {
    checkExprForExceptions(Collection(List()), List("SemanticAnalysisException"))
  }

  property("collection should be consistent") {
    checkConsistencyExceptions(valueTypesTable, values => Collection(values), List("SemanticAnalysisException"))
  }

  property("tuple should be contains at least 1 element") {
    checkExprForExceptions(Tuple(List()), List("SemanticAnalysisException"))
  }

  property("tuple should be contain elements of different types") {
    (1 to Constants.TupleMaxDim).foreach { n =>
      checkContains(valueTypes, values => Tuple(values), n)
    }
  }

}
