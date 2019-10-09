package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.ValueGenerator
import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast.{CompOp, _}
import org.encryfoundation.prismlang.core.Types.{PBoolean, PByte, PInt}
import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec}

class OperatorsTypeCompatibilitySpec extends PropSpec
  with Matchers
  with TableDrivenPropertyChecks
  with ExprChecker {

  val valueTypes = List("Str", "IntConst", "ByteConst", "Base16Str", "Base58Str", "Bool")

  def genValues: List[Expr] = ValueGenerator.genValues(valueTypes, valueTypes.size)._1

  val values1: List[Expr] = genValues
  val values2: List[Expr] = genValues

  def compareExpr(oper: CompOp, values: List[Expr]) = Compare(values(0), List(oper), List(values(1)))
  def binExpr(oper: Operator, values: List[Expr]) = Bin(values(0), oper, values(1))
  def boolExpr(oper: BooleanOp, values: List[Expr]) = Bool(oper, List(values(0), values(1)))
  def unaryExpr(oper: UnaryOp, values: List[Expr]) = Unary(oper, values(0))

  def checkBinOperators[T](operators: List[T], values1: List[Expr], values2: List[Expr],
                           expr: (T, List[Expr]) => Expr, expectedExceptions: List[String]) {
    operators.foreach { operator =>
      values1.foreach { value1 =>
        values2.foreach { value2 =>
          if(value1.tpe != value2.tpe && !exclusion(value1, value2)) {
            checkExprForExceptions(expr(operator, List(value1, value2)), expectedExceptions)
          }
        }
      }
    }
  }

  def checkUnaryOperators[T](operators: List[T], values: List[Expr],
                             expr: (T, List[Expr]) => Expr, expectedExceptions: List[String]) {
    operators.foreach { operator =>
      values.foreach { value =>
        checkExprForExceptions(expr(operator, List(value)), expectedExceptions)
      }
    }
  }

  property("binary operators shouldn't compile with different types") {
    checkBinOperators(List(Operator.Add, Operator.Sub, Operator.Mult, Operator.Div, Operator.Mod, Operator.Pow),
      values1, values2, binExpr, List("SemanticAnalysisException"))
  }

  property("compare operators shouldn't compile with different types") {
    checkBinOperators(List(CompOp.GtE, CompOp.Gt, CompOp.Lt, CompOp.LtE),
      values1, values2, compareExpr, List("SemanticAnalysisException"))
  }

  property("Eq NotEq shouldn't compile with different types") {
    checkBinOperators(List(CompOp.Eq, CompOp.NotEq),
      values1, values2, compareExpr, List("Exception", "ClassCastException"))
  }

  property("In NotIn shouldn't compile with different types") {
    checkBinOperators(List(CompOp.In, CompOp.NotIn), values1, values2, compareExpr, List("SemanticAnalysisException", "ClassCastException", "Exception"))
  }

  property("And Or shouldn't compile with different types") {
    checkBinOperators(List(BooleanOp.And, BooleanOp.Or), values1, values2, boolExpr, List("SemanticAnalysisException"))
  }

  property("Unary.Not should be compile with Boolean type only") {
    val valExprs = genValues
      .filterNot(_.tpe == PBoolean)
    checkUnaryOperators(List(UnaryOp.Not), valExprs, unaryExpr, List("SemanticAnalysisException"))
  }

  property("Unary.Invert should be compile with Int Byte types only") {
    val valExprs = genValues
      .filterNot(expr =>  List(PInt, PByte).contains(expr.tpe))
    checkUnaryOperators(List(UnaryOp.Invert), valExprs, unaryExpr, List("SemanticAnalysisException"))
  }

}
