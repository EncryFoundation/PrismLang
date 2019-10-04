package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast.{CompOp, _}
import org.encryfoundation.prismlang.core.Types.{PBoolean, PByte, PInt}
import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec}

class OperatorsTypeCompatibilitySpec extends PropSpec
  with Matchers
  with TableDrivenPropertyChecks
  with ExprChecker {

  val values1: TableFor1[Expr] = Table("values1", Str("qwe"), Str("100"), Base58Str("abc"), Base16Str("123"),
    True, False, Collection(List(Str("asd"))), Collection(List(IntConst(0))), Tuple(List(IntConst(10))))

  val values2: TableFor1[Expr] = Table("values2", IntConst(0), IntConst(123), ByteConst(123), True, False,
    Collection(List(Str("123"))), Tuple(List(IntConst(0))))

  def compareExpr(oper: CompOp, values: List[Expr]) = Compare(values(0), List(oper), List(values(1)))
  def binExpr(oper: Operator, values: List[Expr]) = Bin(values(0), oper, values(1))
  def boolExpr(oper: BooleanOp, values: List[Expr]) = Bool(oper, List(values(0), values(1)))
  def unaryExpr(oper: UnaryOp, values: List[Expr]) = Unary(oper, values(0))

  def exclusion(value1: Expr, value2: Expr): Boolean = value1 != value2 && value1.tpe != value2.tpe

  def checkOperators[T](table: TableFor1[T], values1: TableFor1[Expr], values2: TableFor1[Expr], expr: (T, List[Expr]) => Expr, expectedExceptions: List[String]) {
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

  def checkUnaryOperators[T](operators: List[T], values: List[Expr], expr: (T, List[Expr]) => Expr, expectedExceptions: List[String]) {
    operators.foreach { operator =>
      values.foreach { value =>
        checkExprForExceptions(expr(operator, List(value)), expectedExceptions)
      }
    }
  }

  property("binary operator with different types shouldn't compile") {
    checkOperators(Table("operators", Operator.Add, Operator.Sub, Operator.Mult, Operator.Div, Operator.Mod, Operator.Pow),
      values1, values2, binExpr, List("SemanticAnalysisException"))
  }

  property("compare operator with different types shouldn't compile") {
    checkOperators(Table("compOps", CompOp.GtE, CompOp.Gt, CompOp.Lt, CompOp.LtE),
      values1, values2, compareExpr, List("SemanticAnalysisException"))
  }

  property("Eq NotEq with different types shouldn't compile") {
    checkOperators(Table("compOps", CompOp.Eq, CompOp.NotEq),
      values1, values2, compareExpr, List("Exception", "ClassCastException"))
  }

  property("In NotIn with different types shouldn't compile") {
    checkOperators(Table("compOps", CompOp.In, CompOp.NotIn),
      values1, values2, compareExpr, List("SemanticAnalysisException", "ClassCastException"))
  }

  property("And Or with different types shouldn't compile") {
    checkOperators(Table("boolOps", BooleanOp.And, BooleanOp.Or),
      values1, values2, boolExpr, List("SemanticAnalysisException"))
  }

  property("Unary Not except Boolean type shouldn't compile") {
    checkUnaryOperators(List(UnaryOp.Not), (values1.toList ++ values2.toList).filterNot(_.tpe == PBoolean),
      unaryExpr, List("SemanticAnalysisException"))
  }

  property("Unary Invert except Int type shouldn't compile") {
    checkUnaryOperators(List(UnaryOp.Invert), (values1.toList ++ values2.toList).filterNot(e => List(PInt, PByte).contains(e.tpe)),
      unaryExpr, List("SemanticAnalysisException"))
  }

}
