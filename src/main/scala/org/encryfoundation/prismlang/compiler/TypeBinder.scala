package org.encryfoundation.prismlang.compiler

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{manytd, rewrite, strategy}
import org.encryfoundation.prismlang.core.{Ast, Types}

object TypeBinder {

  def bind(expr: Ast.Expr, tags: List[Types.StructTag]): Ast.Expr = rewrite(manytd(strategy[Ast.Expr]({
    /** Replaces verbose type name with its fingerprint. */
    case ifLet @ Ast.Expr.IfLet(_, Ast.TypeIdent(name, _), _, _, _, _) if tags.map(_.ident).contains(name) =>
      tags.find(_.ident == name).map { tag => ifLet.copy(typeIdent = Ast.TypeIdent(name, List.empty)) }
    case _ => None
  })))(expr)
}
