package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.ValueGenerator
import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast.{CompOp, _}
import org.encryfoundation.prismlang.core.Types.{PBoolean, PByte, PCollection, PInt}
import org.encryfoundation.prismlang.integration.Utils
import org.scalatest.PropSpec

class OperatorsTypeCompatibilitySpec extends PropSpec with Utils {

  val valueTypes = List("Str", "IntConst", "ByteConst", "Base16Str", "Base58Str", "Bool")
  def genValues: List[Expr] = ValueGenerator.genValues(valueTypes, valueTypes.size)._1

  val values1: List[Expr] = genValues
  val values2: List[Expr] = genValues

  property("binary operators shouldn't compile with different types") {
    checkBinaryOperators(List(Operator.Add, Operator.Sub, Operator.Mult, Operator.Div, Operator.Mod, Operator.Pow),
      values1, values2, binExpr)
  }

  property("compare operators shouldn't compile with different types") {
    checkBinaryOperators(List(CompOp.GtE, CompOp.Gt, CompOp.Lt, CompOp.LtE), values1, values2, compareExpr)
  }

//  property("Eq NotEq shouldn't compile with different types") {
//    checkBinaryOperators(List(CompOp.Eq, CompOp.NotEq), values1, values2, compareExpr)
//  }
//
//  property("In NotIn shouldn't compile with different types") {
//    checkBinaryOperators(List(CompOp.In, CompOp.NotIn), values1, values2, inExpr)
//  }

  property("And Or shouldn't compile with different types") {
    checkBinaryOperators(List(BooleanOp.And, BooleanOp.Or), values1, values2, boolExpr)
  }

  property("Unary.Not should be compile with Boolean type only") {
    val valExprs = values1
      .filterNot(_.tpe == PBoolean)
    checkUnaryOperators(List(UnaryOp.Not), valExprs, unaryExpr)
  }

  property("Unary.Invert should be compile with Int Byte types only") {
    val valExprs = values1
      .filterNot(expr =>  List(PInt, PByte).contains(expr.tpe))
    checkUnaryOperators(List(UnaryOp.Invert), valExprs, unaryExpr)
  }

  def compareExpr(oper: CompOp, values: List[Expr]) = Compare(values(0), List(oper), List(values(1)))
  def inExpr(oper: CompOp, values: List[Expr]) =  Compare(values(0), List(oper), List(Collection(List(values(1)))))
  def binExpr(oper: Operator, values: List[Expr]) = Bin(values(0), oper, values(1))
  def boolExpr(oper: BooleanOp, values: List[Expr]) = Bool(oper, List(values(0), values(1)))
  def unaryExpr(oper: UnaryOp, values: List[Expr]) = Unary(oper, values(0))

  def checkBinaryOperators[T](operators: List[T], values1: List[Expr], values2: List[Expr], expr: (T, List[Expr]) => Expr) {
    operators.foreach { operator =>
      values1.foreach { value1 =>
        values2.foreach { value2 =>
          if(value1.tpe != value2.tpe && !compatibleTypesExclusion(value1, value2)) {
            checkExpr(expr(operator, List(value1, value2)))
          }
        }
      }
    }
  }

  def checkUnaryOperators[T](operators: List[T], values: List[Expr], expr: (T, List[Expr]) => Expr) {
    operators.foreach { operator =>
      values.foreach { value =>
        checkExpr(expr(operator, List(value)))
      }
    }
  }

}
