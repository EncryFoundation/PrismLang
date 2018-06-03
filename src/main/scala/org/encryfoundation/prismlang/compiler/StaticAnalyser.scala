package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.compiler.scope.ScopedSymbolTable
import org.encryfoundation.prismlang.core.{TypeSystem, Types}
import org.encryfoundation.prismlang.core.Ast._

class StaticAnalyser(types: TypeSystem) {

  var scopes: List[ScopedSymbolTable] = List(ScopedSymbolTable.initial)

  def currentScope: ScopedSymbolTable = scopes.head

  def getType(ident: Ident): Types.PType = types.typeByIdent(ident.name)
    .getOrElse(throw UnresolvedTypeException(ident.name))
}
