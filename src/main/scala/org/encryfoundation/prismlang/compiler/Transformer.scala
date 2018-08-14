package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.core.Ast._

object Transformer {

  def transform(expr: Expr): Expr =
    expr match {

      /** Rule: Call(Attribute(coll, "exists"), Func) -> Exists(coll, Func) */
      case Expr.Call(Expr.Attribute(value, attr, _), args, _)
        if attr.name == "exists" && args.size == 1 => Expr.Exists(value, args.head)

      /** Rule: Call(Attribute(coll, "map"), Func) -> Map(coll, Func) */
      case Expr.Call(Expr.Attribute(value, attr, _), args, _)
        if attr.name == "map" && args.size == 1 => Expr.Map(value, args.head)

      /** Rule: Call(Attribute(coll, "filter"), Func) -> Filter(coll, Func) */
      case Expr.Call(Expr.Attribute(value, attr, _), args, _)
        if attr.name == "filter" && args.size == 1 => Expr.Filter(value, args.head)

      /** Rule: Attribute(coll, "size") -> SizeOf(coll) */
      case Expr.Attribute(value, attr, _) if attr.name == "size" => Expr.SizeOf(value)

      /** Rule: Attribute(coll, "sum") -> Sum(coll) */
      case Expr.Attribute(value, attr, _) if attr.name == "sum" => Expr.Sum(value)

      /** Rule: Attribute(IntConst(v:Long), "toByte") -> ByteConst(v:Byte) */
      case Expr.Attribute(Expr.IntConst(v), attr, _)
        if attr.name == "toByte" && v <= Byte.MaxValue && v >= Byte.MinValue => Expr.ByteConst(v.toByte)

      /** Below come patterns for AST traversing */
      case let @ Expr.Let(_, value, _) => let.copy(value = transform(value))

      case lam @ Expr.Lambda(_, body, _) => lam.copy(body = transform(body))

      case fun @ Expr.Def(_, _, body, _) => fun.copy(body = transform(body))

      case ifelse @ Expr.If(test, body, orelse, _) => ifelse.copy(transform(test), transform(body), transform(orelse))

      case iflet @ Expr.IfLet(_, _, target, body, orelse, _) => iflet.copy(target = transform(target), body = transform(body), orelse = transform(orelse))

      case block @ Expr.Block(body, _) => block.copy(body.map(transform))

      case bin @ Expr.Bin(left, _, right) => bin.copy(left = transform(left), right = transform(right))

      case bool @ Expr.Bool(_, operands) => bool.copy(values = operands.map(transform))

      case unary @ Expr.Unary(_, operand, _) => unary.copy(operand = transform(operand))

      case cmp @ Expr.Compare(left, _, comps) => cmp.copy(left = transform(left), comparators = comps.map(transform))

      case call @ Expr.Call(_, args, _) => call.copy(args = args.map(transform))

      case attr @ Expr.Attribute(value, _, _) => attr.copy(transform(value))

      case sub @ Expr.Subscript(value, _, _) => sub.copy(transform(value))

      case coll @ Expr.Collection(elts, _) => coll.copy(elts.map(transform))
      case set @ Expr.Set(elts, _) => set.copy(elts.map(transform))
      case tuple @ Expr.Tuple(elts, _) => tuple.copy(elts.map(transform))

      case other => other
    }
}
