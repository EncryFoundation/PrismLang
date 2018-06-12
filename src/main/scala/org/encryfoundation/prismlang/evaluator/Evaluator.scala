package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.core.Ast._
import org.encryfoundation.prismlang.core.wrapped._
import org.encryfoundation.prismlang.core.{Constants, TypeSystem, Types}
import org.encryfoundation.prismlang.evaluator.Evaluator.OutOfFuelException
import scorex.crypto.encode.{Base16, Base58}

case class Evaluator(initialEnv: ScopedRuntimeEnvironment, types: TypeSystem) {

  var environments: List[ScopedRuntimeEnvironment] = List(initialEnv)
  var fuel: Int = Constants.InitialFuelLimit

  def eval[T](expr: Expr): T = if (fuel > 0) {
    fuel = fuel - 1

    /** Invokes user-defined function. */
    def invoke(func: PFunction, args: Any*): func.body.tpe.Underlying = {
      environments = currentEnvironment.emptyChild :: environments
      if (args.size != func.args.size) error(s"Wrong number of arguments, ${func.args.size} required, ${args.size} given.")
      args.zip(func.args).foreach { case (value, (id, argT)) => addToEnv(id, PValue(argT)(value)) }
      val result: func.body.tpe.Underlying = eval[func.body.tpe.Underlying](func.body)
      environments = environments.tail
      result
    }

    /** Applies given function to the given collection. */
    def applyFunction(coll: Expr, func: Expr): Any = eval[coll.tpe.Underlying](coll) match {
      case elts: List[_] if func.tpe.isFunc => eval[func.tpe.Underlying](func) match {
        case func: PFunction => elts.map {
          case packedArgs: List[_] => invoke(func, packedArgs:_*)
          case plain => invoke(func, plain)
        }
      }
      case _ => error(s"'$coll' is not a collection")
    }

    expr match {
      /** Evaluate `value`, wrap it with `PValue` and add to the scope */
      case Expr.Let(name, value, _) =>
        val valT: Types.PType = value.tpe
        addToEnv(name.name, PValue(valT)(eval[valT.Underlying](value)))

      /** Just construct `PFunction` instance with resolved `args`. */
      case Expr.Lambda(args, body, tpe) => PFunction(types.resolveArgs(args), tpe, body)

      /** Resolve args, construct `PFunction` and add it to the scope. */
      case Expr.Def(name, args, body, _) =>
        addToEnv(name.name, PFunction(types.resolveArgs(args), body.tpe, body))

      /** Evaluate `test`, then evaluate either `body` or `orelse` depending on test result. */
      case Expr.If(test, body, orelse, tpe) =>
        val testR: Boolean = eval[Boolean](test)
        environments = currentEnvironment.emptyChild :: environments
        val result: tpe.Underlying = if (testR) eval[tpe.Underlying](body) else eval[tpe.Underlying](orelse)
        environments = environments.tail
        result

      /** Check whether evaluated value of `target` can be cast to required type, if it
        * can, create nested scope, put `local` there and evaluate `body`, else do the same
        * for `orelse` branch. */
      case Expr.IfLet(local, typeIdent, target, body, orelse, tpe) =>
        val requiredT: Types.PType = types.resolveType(typeIdent)
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

      /** Acts much like `IfLet`, but performs shallow type matching by
        * type fingerprint when type descriptor is unavailable (in case
        * of user-defined struct matching). */
      case Expr.IfLetR(local, typeFingerprint, target, body, orelse, tpe) =>
        val targetR: target.tpe.Underlying = eval[target.tpe.Underlying](target)
        environments = currentEnvironment.emptyChild :: environments
        val result: tpe.Underlying = targetR match {
          case obj: PObject if obj.hasSameFingerprint(typeFingerprint) =>
            addToEnv(local.name, PValue(obj.tpe)(obj))
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
      case Expr.Unary(op, operand) =>
        op match {
          case UnaryOp.Not => !eval[Boolean](operand)
          case UnaryOp.Invert => eval[Long](operand) * (-1)
        }

      /** Evaluate left operand then compare it with evaluated
        * operands using `ops`. */
      case Expr.Compare(left, ops, comps) =>
        val leftV: left.tpe.Underlying = eval[left.tpe.Underlying](left)
        ops.zip(comps).forall {
          case (CompOp.Eq, comp) => CompareOps.eq(leftV, eval[comp.tpe.Underlying](comp))
          case (CompOp.NotEq, comp) => !CompareOps.eq(leftV, eval[comp.tpe.Underlying](comp))
          case (CompOp.Gt, comp) => CompareOps.gt(leftV, eval[comp.tpe.Underlying](comp))
          case (CompOp.GtE, comp) => CompareOps.gte(leftV, eval[comp.tpe.Underlying](comp))
          case (CompOp.Lt, comp) => CompareOps.lt(leftV, eval[comp.tpe.Underlying](comp))
          case (CompOp.LtE, comp) => CompareOps.lte(leftV, eval[comp.tpe.Underlying](comp))
          case (CompOp.In, comp) => eval[List[_]](comp).exists(elt => CompareOps.eq(elt, leftV))
          case (CompOp.NotIn, comp) => !eval[List[_]](comp).exists(elt => CompareOps.eq(elt, leftV))
        }

      /** Get referenced name from environment. */
      case Expr.Name(ident, _) =>
        currentEnvironment.get(ident.name) match {
          case Some(v: PValue) => v.value
          case Some(f: PFunction) => f
          case Some(pf: PFunctionPredef) => pf
          case None => error(s"Unresolved reference '${ident.name}'")
        }

      /** Get called function from env. In case regular function is called, create nested
        * env, put there resolved arguments and evaluate `body`. In case predefined function
        * is called, just call body passing to it list of resolved arguments. */
      case Expr.Call(name: Expr.Name, args, _) =>
        eval[name.tpe.Underlying](name) match {
          case func: PFunction => invoke(func, args.map(arg => eval[arg.tpe.Underlying](arg)))
          case PFunctionPredef(varargs, body) =>
            val argsR: List[(String, PValue)] = args.map(arg => eval[arg.tpe.Underlying](arg)).zip(varargs)
              .map { case (value, (id, argT)) => id -> PValue(argT)(value) }
            body(argsR).getOrElse(error("Predef function execution failed"))
        }

      /** Evaluate `value`, in case it is an `PObject`, get corresponding field. If evaluated
        * `value` is a `PTuple` then parse index contained in the `attr` name and return
        * corresponding element from underlying collection. */
      case Expr.Attribute(value, attr, _) =>
        eval[value.tpe.Underlying](value) match {
          case obj: PObject => obj.getAttr(attr.name).getOrElse(error(s"No such attribute '${obj.tpe.ident}.${attr.name}'"))
          case tuple: List[_] if value.tpe.isTuple => tuple(attr.name.stripPrefix("_").toInt)
          case _ => error("Illegal expression")
        }

      /** Evaluate `value`, in case `op` is by-index subscription evaluate index and return
        * element of collection at corresponding position. In case `op` is slicing, evaluate
        * lower and upper indexes and perform slicing. Third element of `Subscript` is omitted
        * for now. */
      case Expr.Subscript(value, op, _) =>
        eval[value.tpe.Underlying](value) match {
          case coll: List[_] if value.tpe.isCollection => op match {
            case SliceOp.Index(idx) => coll(eval[Long](idx).toInt)
            case SliceOp.Slice(lower, upper) =>
              val lowerR: Long = lower.map(idx => eval[Long](idx)).getOrElse(0)
              val upperR: Long = upper.map(idx => eval[Long](idx)).getOrElse(coll.size)
              //val stepR = step.map(i => eval[i.tpe.Underlying](i)).getOrElse(0)
              coll.slice(lowerR.toInt, upperR.toInt)
          }
          case _ => error("Illegal operation")
        }
      case Expr.Map(coll, func, _) => applyFunction(coll, func)
      case Expr.Exists(coll, func) => applyFunction(coll, func) match {
        case bools: List[Boolean@unchecked] => bools.contains(true)
        case _ => error("Illegal operation")
      }
      case Expr.SizeOf(coll) => eval[coll.tpe.Underlying](coll) match {
        case list: List[_] => list.size.toLong
        case _ => error("Illegal operation")
      }
      case Expr.Sum(coll) => eval[coll.tpe.Underlying](coll) match {
        case list: List[Long@unchecked] => list.sum
        case _ => error("Illegal operation")
      }
      case Expr.Collection(elts, _) => elts.map(elt => eval[elt.tpe.Underlying](elt))
      case Expr.Tuple(elts, _) => elts.map(elt => eval[elt.tpe.Underlying](elt))
      case Expr.Base58Str(value) => Base58.decode(value).getOrElse(error("Base58 string decoding failed"))
      case Expr.Base16Str(value) => Base16.decode(value).getOrElse(error("Base16 string decoding failed"))

      /** For simple constants just return their value. */
      case Expr.IntConst(value) => value
      case Expr.ByteConst(value) => value
      case Expr.Str(value) => value
      case Expr.True => true
      case Expr.False => false
      case _ => error("Illegal operation")
    }
  }.asInstanceOf[T] else throw OutOfFuelException

  def currentEnvironment: ScopedRuntimeEnvironment = environments.head

  /** Add member to current environment. */
  def addToEnv(name: String, member: PWrappedMember): Unit = {
    val updatedEnv: ScopedRuntimeEnvironment = environments.head.updated(name, member)
    environments = environments.updated(0, updatedEnv)
  }

  def error(msg: String) = throw new PRuntimeException(msg)
}

object Evaluator {

  case object OutOfFuelException extends Exception("Out of fuel")

  def default: Evaluator = Evaluator(ScopedRuntimeEnvironment.empty(1), TypeSystem.default)
}
