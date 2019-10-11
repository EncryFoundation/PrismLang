package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast.Operator
import org.encryfoundation.prismlang.core.Types._
import org.encryfoundation.prismlang.core.{Ast, Types}

trait TypeMatching {

  def rightType(required: Types.PType, actual: Types.PType): Boolean = {
    required match {
      case coll: PCollection => coll.valT != PAny
      case _ => required == actual || actual.isSubtypeOf(required) || actual.canBeDerivedTo(required)
    }
  }

  def rightTypeIn(left: Types.PType, right: Types.PType): Boolean = {
    right match {
      case PCollection(collType) if rightType(collType, left) => true
      case _ => false
    }
  }

  def matchType(required: Types.PType, actual: Types.PType, msgOpt: Option[String] = None): Unit =
    if (!rightType(required, actual)) throw SemanticAnalysisException(msgOpt.getOrElse(s"Type mismatch: $required != $actual"))

  def unsupportedOperation(leftType: PType, rightType: PType) =
    throw SemanticAnalysisException(s"Unsupported operation for types $leftType and $rightType")

  def isValidBinaryOperation(leftOperand: Ast.Expr, rightOperand: Ast.Expr, operator: Ast.Operator): Unit = {
    operator match {
      case Operator.Add => leftOperand.tpe match {
        case PByte | PInt => matchType(PInt, rightOperand.tpe)
          checkIntBoundaries(leftOperand, rightOperand, operator)
        case PString => matchType(PString, rightOperand.tpe)
        case _ => unsupportedOperation(leftOperand.tpe, rightOperand.tpe)
      }
      case Operator.Div | Operator.Mod => leftOperand.tpe match {
        case PByte | PInt =>
          matchType(PInt, rightOperand.tpe)
          checkZeroDivision(rightOperand)
        case _ => unsupportedOperation(leftOperand.tpe, rightOperand.tpe)
      }
      case _ => leftOperand.tpe match {
        case PByte | PInt => matchType(PInt, rightOperand.tpe)
          if (!checkIntBoundaries(leftOperand, rightOperand, operator)) throw SemanticAnalysisException("Result exceeds PInt boundary")
        case _ => unsupportedOperation(leftOperand.tpe, rightOperand.tpe)
      }
    }
  }

  def checkZeroDivision(rightOperand: Ast.Expr): Unit = {
    rightOperand match {
      case exp: IntConst => exp.value match {
        case 0 => throw SemanticAnalysisException("Zero division found")
        case _ =>
      }
      case exp: ByteConst => exp.value match {
        case 0 => throw SemanticAnalysisException("Zero division found")
        case _ =>
      }
      case _ =>
    }
  }

  def checkIntBoundaries(lefOperand: Ast.Expr, rightOperand: Ast.Expr, operator: Ast.Operator): Boolean = {
    lefOperand match {
      case lExpr: IntConst => rightOperand match {
        case rExpr: IntConst => operator match {
          case Operator.Add => (BigInt(lExpr.value) + BigInt(rExpr.value)).isValidLong
          case Operator.Sub => (BigInt(lExpr.value) - BigInt(rExpr.value)).isValidLong
          case Operator.Pow => (BigInt(lExpr.value).modPow(BigInt(rExpr.value), 1)).isValidLong
          case Operator.Mult => (BigInt(lExpr.value) * BigInt(rExpr.value)).isValidLong
        }
        case rExpr: ByteConst => checkIntBoundaries(lefOperand, IntConst(rExpr.value), operator)
      }
      case lExpr: ByteConst => checkIntBoundaries(IntConst(lExpr.value), rightOperand, operator)
      case _ => true
    }
  }
}
