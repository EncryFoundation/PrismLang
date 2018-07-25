package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.compiler.scope.PredefinedScope
import org.encryfoundation.prismlang.core.Ast.{Expr, Ident}
import org.encryfoundation.prismlang.core.CostTable

case class CostEstimator(initialEnv: Map[String, Int]) {

  import CostTable._
  type Cost = PartialFunction[Expr, Int]

  private var env: Map[String, Int] = initialEnv
  private val blockDepthController: RestrictedStack = RestrictedStack.default
  private val scriptLengthController: RestrictedStack = RestrictedStack.default

  def costOf: Cost = {
    case exp => scriptLengthController.pushNext()
      costOfExpr(exp)
  }

  def costOfExpr: Cost = costOfSyntacticalConstr orElse
    costOfTransformer orElse
    costOfConst orElse
    costOfRef orElse
    costOfOp

  def costOfSyntacticalConstr: Cost = {
    case Expr.Def(Ident(name), _, body, _) =>
      env = env.updated(name, costOf(body))
      FuncDeclarationC
    case Expr.Lambda(_, body, _) => LambDeclarationC + costOf(body)
    case Expr.Let(_, value, _) => ConstDeclarationC + costOf(value)
    case Expr.Block(body, _) =>
      blockDepthController.pushNext()
      val cost: Int = BlockDeclarationC + body.map(costOf).sum +
        math.ceil(blockDepthController.penalty * NestedBlockPenalty).round.toInt
      blockDepthController.pop
      cost
    case Expr.If(test, body, orelse, _) => IfC + costOf(test) + math.max(costOf(body), costOf(orelse))
    case Expr.IfLet(_, _, target, body, orelse, _) => IfLetC + costOf(target) + math.max(costOf(body), costOf(orelse))
    case Expr.IfLetR(_, _, target, body, orelse, _) => IfLetC + costOf(target) + math.max(costOf(body), costOf(orelse))
  }

  def costOfRef: Cost = {
    case Expr.Call(func @ Expr.Name(Ident(name), _), args, _) =>
      CallC + costOf(func) + args.map(costOf).sum + env.getOrElse(name, 0)
    case Expr.Attribute(value, _, _) => NameRefC + costOf(value)
    case Expr.Subscript(value, _, _) => SubscriptC + costOf(value)
    case _: Expr.Name => NameRefC
  }

  def costOfOp: Cost = {
    case Expr.Bool(_, values) => BoolOpC + values.map(costOf).sum
    case Expr.Bin(left, _, right) => BinOpC + costOf(left) + costOf(right)
    case Expr.Unary(_, operand, _) => UnaryOpC + costOf(operand)
    case Expr.Compare(left, _, comparators) => CompOpC + costOf(left) + comparators.map(costOf).sum
  }

  def costOfConst: Cost = {
    case Expr.Collection(elts, _) => CollEltC * elts.length + elts.map(costOf).sum
    case Expr.Tuple(elts, _) => TupleEltC * elts.length + elts.map(costOf).sum
    case Expr.Base58Str(value) => CharC * value.length + DecodingC
    case Expr.Base16Str(value) => CharC * value.length + DecodingC
    case Expr.IntConst(_) => IntConstC
    case Expr.ByteConst(_) => ByteConstC
    case Expr.Str(value) => CharC * value.length
    case Expr.True => BoolLeafC
    case Expr.False => BoolLeafC
  }

  def costOfTransformer: Cost = {
    case Expr.SizeOf(coll) => SizeOfC + costOf(coll)
    case Expr.Exists(coll, Expr.Name(Ident(name), _)) => ExistsC + (costOf(coll) / CollEltC) * env.getOrElse(name, 0)
    case Expr.Exists(coll, lamb: Expr.Lambda) => ExistsC + (costOf(coll) / CollEltC) * costOf(lamb)
    case Expr.Sum(coll) => (costOf(coll) / CollEltC) * SumC
    case Expr.Map(coll, Expr.Name(Ident(name), _), _) => MapC + (costOf(coll) / CollEltC) * env.getOrElse(name, 0)
    case Expr.Map(coll, lamb: Expr.Lambda, _) => MapC + (costOf(coll) / CollEltC) * costOf(lamb)
  }
}

object CostEstimator {
  def default: CostEstimator = CostEstimator(PredefinedScope.all.map(m => m.name -> m.cost).toMap)
}
