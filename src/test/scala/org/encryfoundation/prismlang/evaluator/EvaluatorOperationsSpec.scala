package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.compiler.TestCompiler
import org.encryfoundation.prismlang.core.Ast.Expr.{Compare, _}
import org.encryfoundation.prismlang.core.Ast.{CompOp, _}
import org.encryfoundation.prismlang.core.{Ast, Types}
import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec}

import scala.util.{Failure, Try}
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
  val comps = Table("comps", CompOp.GtE, CompOp.Gt, CompOp.Lt, CompOp.LtE)

  val values = Table("values", Str("qwe"), Str("100"), IntConst(100), Collection(List(IntConst(0))), True, False)

  property("binary operator with different types shouldn't compiled") {

    forAll(operators) { operator =>
      forAll(values) { value1 =>
        forAll(values) { value2 =>
          whenever(value1.tpe != value2.tpe) {
            check(
              Bin(
                value1,
                operator,
                value2
              ),
              "SemanticAnalysisException"
            )
          }
        }
      }
    }

    forAll(comps) { comp =>
      forAll(values) { value1 =>
        forAll(values) { value2 =>
          whenever(value1.tpe != value2.tpe) {
            check(
              Expr.Compare(
                value1,
                List(comp),
                List(value2)
              ),
              "SemanticAnalysisException"
            )
          }
        }
      }
    }

  }

  def check(expr: Ast.Expr, expectedExceptionName: String) {
    val astExpr = compileExpr(expr)

    assert(astExpr.isFailure, s"Expression '${expr.toString}' shouldn't compile")
    val throwable: Throwable = astExpr match {
      case Failure(e) => e
    }

    throwable.getClass.getSimpleName shouldBe expectedExceptionName
  }

}
