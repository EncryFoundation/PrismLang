package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast.Operator
import org.encryfoundation.prismlang.core.Types.{PByte, _}
import org.encryfoundation.prismlang.core.{Ast, Types}

trait TypeMatching {

  def rightType(required: Types.PType, actual: Types.PType): Boolean = {
    required match {
      case coll: PCollection => coll.valT != PAny
      case _ => required == actual || actual.isSubtypeOf(required) || actual.canBeDerivedTo(required) || required.isSubtypeOf(actual) || required.canBeDerivedTo(actual)
    }
  }

  def rightTypeEqNotEq(required: Types.PType, actual: Types.PType): Boolean = {
    required match {
      case coll: PCollection if actual.isCollection =>
        val actualCollType = actual.asInstanceOf[PCollection].valT
        coll.valT == actualCollType || coll.valT.isSubtypeOf(actualCollType) || coll.valT.canBeDerivedTo(actualCollType)
      case _  if !actual.isCollection && !required.isCollection => required == actual || actual.isSubtypeOf(required) || actual.canBeDerivedTo(required)
      case _ => false
    }
  }

  def rightTypeIn(left: Types.PType, right: Types.PType): Boolean = {
    right match {
      case PCollection(collType) if collType.isCollection && rightTypeEqNotEq(collType, left) => true
      case PCollection(collType) if !collType.isCollection && rightType(collType, left) => true
      case _ => false
    }
  }

  def matchType(required: Types.PType, actual: Types.PType, msgOpt: Option[String] = None): Unit =
    if (!rightType(required, actual)) throw SemanticAnalysisException(msgOpt.getOrElse(s"Type mismatch: $required != $actual"))

  def matchTypeCollElems(required: Types.PType, actual: Types.PType, msgOpt: Option[String] = None): Unit =
    if (!rightTypeEqNotEq(required, actual)) throw SemanticAnalysisException(msgOpt.getOrElse(s"Type mismatch in collection: $required != $actual"))

  def unsupportedOperation(leftType: PType, rightType: PType) =
    throw SemanticAnalysisException(s"Unsupported operation for types $leftType and $rightType")

  def isValidBinaryOperation(leftOperand: Ast.Expr, rightOperand: Ast.Expr, operator: Ast.Operator): Unit = {
    operator match {
      case Operator.Add =>
        leftOperand.tpe match {
        case PByte if rightOperand.tpe == PByte => matchType(PInt, rightOperand.tpe)
          if (!checkByteBoundaries(leftOperand, rightOperand, operator)) throw SemanticAnalysisException("Result exceeds PByte boundary")
        case PInt => matchType(PInt, rightOperand.tpe)
          if (!checkIntBoundaries(leftOperand, rightOperand, operator)) throw SemanticAnalysisException("Result exceeds PInt boundary")
        case PString => matchType(PString, rightOperand.tpe)
        case _ => unsupportedOperation(leftOperand.tpe, rightOperand.tpe)
      }
      case Operator.Div | Operator.Mod => leftOperand.tpe match {
        case PByte | PInt => matchType(PInt, rightOperand.tpe)
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

  def checkIntBoundaries(leftOperand: Ast.Expr, rightOperand: Ast.Expr, operator: Ast.Operator): Boolean = {
    leftOperand match {
      case lExpr: IntConst => rightOperand match {
        case rExpr: IntConst => operator match {
          case Operator.Add => (BigInt(lExpr.value) + BigInt(rExpr.value)).isValidLong
          case Operator.Sub => (BigInt(lExpr.value) - BigInt(rExpr.value)).isValidLong
          case Operator.Pow => (BigInt(lExpr.value).modPow(BigInt(rExpr.value), 1)).isValidLong
          case Operator.Mult => (BigInt(lExpr.value) * BigInt(rExpr.value)).isValidLong
        }
        case rExpr: ByteConst => checkIntBoundaries(leftOperand, IntConst(rExpr.value), operator)
      }
      case lExpr: ByteConst => checkIntBoundaries(IntConst(lExpr.value), rightOperand, operator)
      case _ => true
    }
  }

  def checkByteBoundaries(leftOperand: Ast.Expr, rightOperand: Ast.Expr, operator: Ast.Operator): Boolean = {
    leftOperand match {
      case lExpr: ByteConst => rightOperand match {
        case rExpr: ByteConst => operator match {
          case Operator.Add => (BigInt(lExpr.value) + BigInt(rExpr.value)).isValidByte
          case Operator.Sub => (BigInt(lExpr.value) - BigInt(rExpr.value)).isValidByte
          case Operator.Pow => (BigInt(lExpr.value).modPow(BigInt(rExpr.value), 1)).isValidByte
          case Operator.Mult => (BigInt(lExpr.value) * BigInt(rExpr.value)).isValidByte
        }
        case rExpr: IntConst => checkIntBoundaries(IntConst(lExpr.value), rExpr, operator)
      }
      case lExpr: IntConst => checkIntBoundaries(IntConst(lExpr.value), rightOperand, operator)
      case _ => true
    }
  }
}
