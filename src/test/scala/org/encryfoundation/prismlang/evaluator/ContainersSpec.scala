package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.ValueGenerator
import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast._
import org.encryfoundation.prismlang.core.Constants
import org.encryfoundation.prismlang.integration.Utils
import org.scalatest.PropSpec

class ContainersSpec extends PropSpec with Utils {

  val valueTypes = List("Str", "IntConst", "ByteConst", "Base16Str", "Base58Str", "Bool")

  property("collection should be contains elements") {
    (1 to Constants.CollMaxLength).foreach { n =>
      valueTypes.foreach { valueType =>
        checkContains(List(valueType), values => Collection(values), n)
      }
    }
  }

  property("collection should be contains at least 1 element") {
    checkExpr(Collection(List()))
  }

//  property("collection should be consistent") {
//    checkConsistency(valueTypes, values => Collection(values))
//  }

  property("tuple should be contains at least 1 element") {
    checkExpr(Tuple(List()))
  }

  property("tuple should be contain elements of different types") {
    (1 to Constants.TupleMaxDim).foreach { n =>
      checkContains(valueTypes, values => Tuple(values), n)
    }
  }

  def checkContains(valueTypes: List[String], wrapper: List[Expr] => Expr, count: Int): Unit = {
    val (exprVals, expectedVals) =
      ValueGenerator.genValueTypeList(valueTypes, count)
        .map(ValueGenerator.genRandomValue)
        .unzip

    checkExpr(wrapper(exprVals), compilationSuccess = true, evaluationSuccess = true, Some(expectedVals))
  }

  def checkConsistency(valueTypes: List[String], wrapper: List[Expr] => Expr): Unit = {
    valueTypes.foreach { valueType1 =>
      valueTypes.foreach { valueType2 =>
        val (value1, _) = ValueGenerator.genRandomValue(valueType1)
        val (value2, _) = ValueGenerator.genRandomValue(valueType2)
        if(value1.tpe != value2.tpe && !compatibleTypesExclusion(value1, value2)) {
          checkExpr(wrapper(List(value1, value2)))
        }
      }
    }
  }

}
