package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.compiler.TestCompiler
import org.encryfoundation.prismlang.core.Ast
import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast.{CompOp, _}
import org.encryfoundation.prismlang.core.Types.{PCollection, PString}
import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec}

import scala.util.{Failure, Success}

class EvaluatorOperationsSpec extends PropSpec
  with Matchers
  with TableDrivenPropertyChecks
  with TestCompiler
  with ExprEvaluator {

  val values1 = Table("values1", Str("qwe"), Str("100"), Base58Str("abc"), Base16Str("123"),
    True, False, Collection(List(Str("asd"))), Collection(List(IntConst(0))), Tuple(List(IntConst(10))))

  val values2 = Table("values2", IntConst(0), IntConst(123), ByteConst(123), True, False,
    Collection(List(Str("123"))), Tuple(List(IntConst(0))))

  property("binary operator with different types shouldn't compile") {

    val operators = Table("operators", Operator.Add, Operator.Sub, Operator.Mult, Operator.Div, Operator.Mod, Operator.Pow)

    forAll(operators) { operator =>
      forAll(values1) { value1 =>
        forAll(values2) { value2 =>
          whenever(exclusion(value1, value2)) {
            checkExpr(
              Bin(
                value1,
                operator,
                value2
              ),
              List("SemanticAnalysisException")
            )
          }
        }
      }
    }
  }

  property("compare operator with different types shouldn't compile") {
    checkCompOps(Table("compOps", CompOp.GtE, CompOp.Gt, CompOp.Lt, CompOp.LtE), List("SemanticAnalysisException"))
  }

  property("Eq NotEq with different types shouldn't compile") {
    checkCompOps(Table("compOps", CompOp.Eq, CompOp.NotEq), List("Exception", "ClassCastException"))
  }

  property("In NotIn with different types shouldn't compile") {
    checkCompOps(Table("compOps", CompOp.In, CompOp.NotIn), List("SemanticAnalysisException", "ClassCastException"))
  }

  def checkCompOps(compOps: TableFor1[CompOp], expectedExceptions: List[String]) {
    forAll(compOps) { compOp =>
      forAll(values1) { value1 =>
        forAll(values2) { value2 =>
          whenever(exclusion(value1, value2)) {
            checkExpr(
              Expr.Compare(
                value1,
                List(compOp),
                List(value2)
              ),
              expectedExceptions
            )
          }
        }
      }
    }
  }


  def exclusion(value1: Expr, value2: Expr): Boolean = value1 != value2 && value1.tpe != value2.tpe

  def checkExpr(expr: Ast.Expr, expectedExceptions: List[String]) {
    val astExpr = compileExpr(expr)

    val throwable = astExpr match {
      case Success(expr) =>
        val evaluation = eval(expr)
        assert(evaluation.isFailure, s"Expression '${expr.toString}' shouldn't compile")
        evaluation match {
          case Failure(e) => e
        }
      case Failure(e) => e
    }

    assert(expectedExceptions.contains(throwable.getClass.getSimpleName),
      s"${throwable.getClass.getSimpleName} should be  ${expectedExceptions.mkString}")
  }

}
