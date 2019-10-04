package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast.{CompOp, _}
import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec}

class OperatorsTypeCompatibilitySpec extends PropSpec
  with Matchers
  with TableDrivenPropertyChecks
  with ExprChecker {

  val values1 = Table("values1", Str("qwe"), Str("100"), Base58Str("abc"), Base16Str("123"),
    True, False, Collection(List(Str("asd"))), Collection(List(IntConst(0))), Tuple(List(IntConst(10))))

  val values2 = Table("values2", IntConst(0), IntConst(123), ByteConst(123), True, False,
    Collection(List(Str("123"))), Tuple(List(IntConst(0))))

  def compareExpr(oper: CompOp, values: List[Expr]) = Compare(values(0), List(oper), List(values(1)))
  def binExpr(oper: Operator, values: List[Expr]) = Bin(values(0), oper, values(1))

  def exclusion(value1: Expr, value2: Expr): Boolean = value1 != value2 && value1.tpe != value2.tpe

  def checkOperators[T](table: TableFor1[T], expr: (T, List[Expr]) => Expr, expectedExceptions: List[String]) {
    forAll(table) { operator =>
      forAll(values1) { value1 =>
        forAll(values2) { value2 =>
          whenever(exclusion(value1, value2)) {
            checkExprForExceptions(expr(operator, List(value1, value2)), expectedExceptions)
          }
        }
      }
    }
  }

  property("binary operator with different types shouldn't compile") {
    checkOperators(Table("operators", Operator.Add, Operator.Sub, Operator.Mult, Operator.Div, Operator.Mod, Operator.Pow),
      binExpr, List("SemanticAnalysisException"))
  }

  property("compare operator with different types shouldn't compile") {
    checkOperators(Table("compOps", CompOp.GtE, CompOp.Gt, CompOp.Lt, CompOp.LtE), compareExpr, List("SemanticAnalysisException"))
  }

  property("Eq NotEq with different types shouldn't compile") {
    checkOperators(Table("compOps", CompOp.Eq, CompOp.NotEq), compareExpr, List("Exception", "ClassCastException"))
  }

  property("In NotIn with different types shouldn't compile") {
    checkOperators(Table("compOps", CompOp.In, CompOp.NotIn), compareExpr, List("SemanticAnalysisException", "ClassCastException"))
  }

}
