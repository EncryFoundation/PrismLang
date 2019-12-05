package org.encryfoundation.prismlang.compiler.scope

import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.core.TypeSystem
import org.encryfoundation.prismlang.core.Types.{PFunc, PType}

sealed trait Symbol

object Symbol {
  case class VariableSymbol(name: String, tpe: PType) extends Symbol
  case class FunctionSymbol(name: String, tpe: PFunc) extends Symbol
  object FunctionSymbol {
    def apply(func: Expr.Def, tpe: PFunc): FunctionSymbol =
      new FunctionSymbol(func.name.name, tpe)
  }
}
