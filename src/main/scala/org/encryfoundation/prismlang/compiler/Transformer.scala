package org.encryfoundation.prismlang.compiler

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{manytd, rewrite, strategy}
import org.encryfoundation.prismlang.core.Ast

object Transformer {

  def transform(expr: Ast.Expr): Ast.Expr = rewrite(manytd(strategy[Ast.Expr]({

    /** Rule: Call(Attribute(coll, "exists"), Func) -> Exists(coll, Func) */
    case Ast.Expr.Call(Ast.Expr.Attribute(value, attr, _), args, _)
      if attr.name == "exists" && args.size == 1 => Some(Ast.Expr.Exists(value, args.head))

    /** Rule: Call(Attribute(coll, "map"), Func) -> Map(coll, Func) */
    case Ast.Expr.Call(Ast.Expr.Attribute(value, attr, _), args, _)
      if attr.name == "map" && args.size == 1 => Some(Ast.Expr.Map(value, args.head))

    /** Rule: Attribute(coll, "size") -> SizeOf(coll) */
    case Ast.Expr.Attribute(value, attr, _) if attr.name == "size" => Some(Ast.Expr.SizeOf(value))

    /** Rule: Attribute(coll, "sum") -> Sum(coll) */
    case Ast.Expr.Attribute(value, attr, _) if attr.name == "sum" => Some(Ast.Expr.Sum(value))

//    // Rule: Attribute(option, "isDefined") -> IsDefined(option)
//    case EXPR.Attribute(value, attr, _)
//      if value.tipe.isOption && attr.name == "isDefined" =>
//      Some(EXPR.IsDefined(value))
//
//    // Rule: Attribute(option, "get") -> Get(option)
//    case EXPR.Attribute(value, attr, _) if attr.name == "get" =>
//      value.tipe match {
//        case ESOption(inT) => Some(EXPR.Get(value, inT))
//        case ESFunc(_, ESOption(inT)) => Some(EXPR.Get(value, inT))
//        case _ => None
//      }

    case _ => None
  })))(expr)
}
