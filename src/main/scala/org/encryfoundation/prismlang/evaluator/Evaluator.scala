package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.core.Ast.Expr.Contract
import org.encryfoundation.prismlang.core.Ast._
import org.encryfoundation.prismlang.core.wrapped.{PFunction, PObject, PValue, PWrappedMember}
import org.encryfoundation.prismlang.core.{Constants, TypeSystem, Types}
import org.encryfoundation.prismlang.evaluator.Evaluator.OutOfFuelException

import scala.util.{Success, Try}

case class Evaluator(initialEnv: ScopedRuntimeEnvironment, types: TypeSystem, debug: Boolean = false) {

  var environments: List[ScopedRuntimeEnvironment] = List(initialEnv)
  var fuel: Int = Constants.InitialFuelLimit

  def evalContract(contract: Contract): Boolean = Try(eval[Boolean](contract)) match {
    case Success(result) => result
    case _ => false
  }

  def eval[T](expr: Expr): T = if (fuel > 0) (expr match {
    /** Evaluate `value`, wrap it with `PValue` and add to the scope */
    case Expr.Let(name, value, _) =>
      val valT: Types.PType = value.tpe
      addToEnv(name.name, PValue(valT)(eval[valT.Underlying](value)))
    /** Resolve args, construct `PFunction` and add it to the scope. */
    case Expr.Def(name, args, body, _) =>
      addToEnv(name.name, PFunction(resolveArgs(args), body.tpe, body))
    /** Evaluate `test`, then evaluate either `body` or `orelse` depending on test result. */
    case Expr.If(test, body, orelse, tpe) =>
      val testR: Boolean = eval[Boolean](test)
      environments = currentEnvironment.emptyChild :: environments
      val result: tpe.Underlying = if (testR) eval[tpe.Underlying](body) else eval[tpe.Underlying](orelse)
      environments = environments.tail
      result
    /** Check whether evaluated value of `target` can be cast to required type, if it
      * can create nested scope, put `local` there and evaluate `body`, else do the same
      * for `orelse` branch. */
    case Expr.IfLet(local, typeIdent, target, body, orelse, tpe) =>
      val requiredT: Types.PType = resolveType(typeIdent)
      val targetR: target.tpe.Underlying = eval[target.tpe.Underlying](target)
      environments = currentEnvironment.emptyChild :: environments
      val result: tpe.Underlying = targetR match {
        case obj: PObject if obj.isInstanceOf(requiredT) =>
          addToEnv(local.name, PValue(requiredT)(obj))
          eval[tpe.Underlying](body)
        case _ => eval[tpe.Underlying](orelse)
      }
      environments = environments.tail
      result
    /** Create new nested scope, then evaluate each expression inside the body, return
      * result of the last one. */
    case Expr.Block(body, _) =>
      environments = currentEnvironment.emptyChild :: environments
      val result: Any = body.map(expr => eval[expr.tpe.Underlying](expr)).last
      environments = environments.tail
      result
    /** Evaluate operands, then perform `op`. */
    case bin @ Expr.Bin(left, op, right) =>
      val leftR: left.tpe.Underlying = eval[left.tpe.Underlying](left)
      val rightR: right.tpe.Underlying = eval[right.tpe.Underlying](right)
      op match {
        case Operator.Add =>
          Arith.add[bin.tpe.Underlying](leftR, rightR)
        case Operator.Sub =>
          Arith.sub[bin.tpe.Underlying](leftR, rightR)
        case Operator.Mult =>
          Arith.mul[bin.tpe.Underlying](leftR, rightR)
        case Operator.Div =>
          Arith.div[bin.tpe.Underlying](leftR, rightR)
        case Operator.Mod =>
          Arith.mod[bin.tpe.Underlying](leftR, rightR)
      }
    /** Evaluate operands, then compare them according to `op`. */
    case Expr.Bool(op, operands) => op match {
      case BooleanOp.And => operands.forall(operand => eval[Boolean](operand))
      case BooleanOp.Or => operands.foldLeft(false) { case (bool, operand) =>
        bool || eval[Boolean](operand)
      }
    }
    /** Evaluate left operand then compare it with evaluated
      * operands using `ops`. */
    case Expr.Compare(left, ops, comps) =>
      val leftV: left.tpe.Underlying = eval[left.tpe.Underlying](left)
      ops.zip(comps).forall {
        case (CompOp.Eq, comp) => Compare.eq(leftV, eval[comp.tpe.Underlying](comp))
        case (CompOp.Gt, comp) => Compare.gt(leftV, eval[comp.tpe.Underlying](comp))
        case (CompOp.GtE, comp) => Compare.gte(leftV, eval[comp.tpe.Underlying](comp))
        case (CompOp.Lt, comp) => Compare.lt(leftV, eval[comp.tpe.Underlying](comp))
        case (CompOp.LtE, comp) => Compare.lte(leftV, eval[comp.tpe.Underlying](comp))
      }
    case Expr.IntConst(value) => value
    case Expr.Str(value) => value
    case Expr.True => true
    case Expr.False => false
  }).asInstanceOf[T] else throw OutOfFuelException

  def currentEnvironment: ScopedRuntimeEnvironment = environments.head

  /** Add member to current environment. */
  def addToEnv(name: String, member: PWrappedMember): Unit = {
    val updatedEnv: ScopedRuntimeEnvironment = environments.head.updated(name, member)
    environments = environments.updated(0, updatedEnv)
  }

  def resolveArgs(args: List[(Ident, TypeIdent)]): List[(String, Types.PType)] =
    args.map { case (id, typeId) => id.name -> resolveType(typeId) }

  /** Resolves the type from its string representation
    * (including type parameters). */
  def resolveType(ident: TypeIdent): Types.PType = {
    val typeParams: List[Types.PType] = ident.typeParams.map(p => types.typeByIdent(p)
      .getOrElse(error(s"Type '$p' is undefined.")))
    types.typeByIdent(ident.name).map {
      case Types.PCollection(_) => Types.PCollection(typeParams.head)
      case Types.PTuple(_, qty) => Types.PTuple(typeParams.head, qty)
      case Types.POption(_) => Types.POption(typeParams.head)
      case otherT: Types.PType => otherT
    }.getOrElse(error(s"Type '${ident.name}' is undefined."))
  }

  def error(msg: String) = throw new PRuntimeException(msg)
}

object Evaluator {

  case object OutOfFuelException extends Exception("Out of fuel")
}
