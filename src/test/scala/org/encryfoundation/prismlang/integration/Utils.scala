package org.encryfoundation.prismlang.integration

import org.encryfoundation.prismlang.compiler.{CostEstimator, TestCompiler}
import org.encryfoundation.prismlang.core.Ast
import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.core.Types.{PByte, PInt}
import org.encryfoundation.prismlang.evaluator.ExprEvaluator
import org.encryfoundation.prismlang.parser.Parser
import org.scalatest.Matchers
import scala.util.{Failure, Random, Success, Try}

trait Utils extends TestCompiler with Parser with ExprEvaluator with Matchers {

  def getArrayString(sample: List[Any]): String = sample.mkString("Array(", ", ", ")")

  def compiled(prismScript: String): Try[Ast.Expr] = parse(prismScript) match {
    case Success(parsed) => compileExpr(parsed.head)
    case Failure(e) => Failure[Ast.Expr](e)
  }

  def estimateCost(ast: Ast.Expr): Try[Int] = Try(CostEstimator.default.costOf(ast))

  def testCompiledExpressionWithOptionalEvaluation(prismSource: String, compilationSuccess: Boolean,
                                                   evaluationSuccess: Option[Boolean] = None,
                                                   expectedValue: Option[Any] = None): Unit = {
    val tryCompiledSource: Try[Ast.Expr] = compiled(prismSource)
    tryCompiledSource.isSuccess shouldBe compilationSuccess
    if (compilationSuccess) {
      val evaluation: Boolean = evaluationSuccess.getOrElse(
        throw new Exception("Evaluation success param Required to evaluate expression"))
      evaluate(tryCompiledSource, evaluation, expectedValue)
    }
  }

  def evaluate(tryExpression: Try[Ast.Expr], evaluationSuccess: Boolean, expectedValue: Option[Any] = None): Unit = {
    val expression: Ast.Expr = tryExpression match {
      case Success(compiled) => compiled
      case _ => throw new Exception("Cannot evaluate expression")
    }
    val tryEvaluatedExpression: Try[Any] = eval(expression)
    tryEvaluatedExpression.isSuccess shouldBe evaluationSuccess
    if (evaluationSuccess) {
      val value: Any = expectedValue.getOrElse(throw new Exception("No value to compare found"))
      tryEvaluatedExpression.getOrElse(s"Expected $value, but failure occurred") shouldEqual value
    }
  }

  def checkExpr(expr: Expr, compilationSuccess: Boolean = false, evaluationSuccess: Boolean = false, expectedValue: Option[Any] = None) {
    val (compiled, evaluated, result) = compileExpr(expr) match {
      case Success(expr) =>
        if (evaluationSuccess) {
          val result = eval(expr)
          (true, result.isSuccess, result.toOption)
        } else
          (true, false, None)
      case Failure(_) => (false, false, None)
    }

    if (compilationSuccess != compiled) {
      val should = if (compilationSuccess) "should" else "shouldn't"
      throw new Exception(s"Expression '${expr.toString}' $should compile")
    }

    if (evaluationSuccess != evaluated) {
      val should = if (compilationSuccess) "should" else "shouldn't"
      throw new Exception(s"Expression '${expr.toString}' $should evaluate")
    }

    if (evaluationSuccess) {
      val value: Any = expectedValue.getOrElse(throw new Exception("No value to compare found"))
      result.getOrElse(s"Expected $value, but failure occurred") shouldEqual value
    }
  }

  def compatibleTypesExclusion(value1: Expr, value2: Expr): Boolean = {
    val compatibleTypes = List(PByte, PInt)
    compatibleTypes.contains(value1.tpe) && compatibleTypes.contains(value2.tpe)
  }

  def generateRandomString(length: Int): String = Random.alphanumeric.dropWhile(_.isDigit).take(length).mkString

}
