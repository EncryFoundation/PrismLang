package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.compiler.TestCompiler
import org.encryfoundation.prismlang.core.Ast.Expr.{Compare, _}
import org.encryfoundation.prismlang.core.Ast.{CompOp, _}
import org.encryfoundation.prismlang.core.{Ast, Types}
import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec}

import scala.util.{Failure, Success, Try}
import org.scalatest._
import org.scalatest.prop._

class EvaluatorOperationsSpec extends PropSpec
  with Matchers
  with TableDrivenPropertyChecks
  //  with ScalaCheckDrivenPropertyChecks
  //  with GeneratorDrivenPropertyChecks
  with TestCompiler
  with ExprEvaluator {

  val operators = Table("operators", Operator.Add, Operator.Sub, Operator.Mult, Operator.Div, Operator.Mod, Operator.Pow)
  val compOps = Table("compOps", CompOp.GtE, CompOp.Gt, CompOp.Lt, CompOp.LtE, CompOp.Eq, CompOp.NotEq)
  val values = Table("values", Str("qwe"), Str("100"), IntConst(100), True, False)
    //Collection(List(IntConst(0))), Tuple(List()))

  property("binary operator with different types shouldn't compile") {

    forAll(operators) { operator =>
      forAll(values) { value1 =>
        forAll(values) { value2 =>
          whenever(value1.tpe != value2.tpe) {
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

  property("compare with different types shouldn't compile") {
    forAll(compOps) { compOp =>
      forAll(values) { value1 =>
        forAll(values) { value2 =>
          whenever(value1.tpe != value2.tpe) {
            checkExpr(
              Expr.Compare(
                value1,
                List(compOp),
                List(value2)
              ),
              List("SemanticAnalysisException", "Exception")
            )
          }
        }
      }
    }
  }

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

    expectedExceptions.contains(throwable.getClass.getSimpleName) shouldBe true
  }


}
