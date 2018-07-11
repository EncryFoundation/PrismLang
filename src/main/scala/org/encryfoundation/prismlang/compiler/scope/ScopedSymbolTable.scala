package org.encryfoundation.prismlang.compiler.scope

import scala.collection.immutable.TreeMap

case class ScopedSymbolTable(scopeLevel: Int,
                             parentalScopeOpt: Option[ScopedSymbolTable] = None,
                             initialMembers: List[Symbol] = List.empty,
                             isFunc: Boolean = false) {

  var symbols: TreeMap[String, Symbol] = TreeMap(initialMembers.map(m => m.name -> m):_*)

  def insert(sym: Symbol): Unit = {
    lookup(sym.name).foreach(_ => throw new Exception(s"${sym.name} is already defined in scope"))
    symbols = symbols.updated(sym.name, sym)
  }

  def lookup(name: String, currentScopeOnly: Boolean = false): Option[Symbol] =
    symbols.get(name).orElse(if (!currentScopeOnly) parentalScopeOpt.flatMap(_.lookup(name)) else None)

  def nested(members: List[Symbol], isFunc: Boolean = false): ScopedSymbolTable =
    ScopedSymbolTable(this.scopeLevel + 1, Some(this), members, isFunc)

  def nested(isFunc: Boolean): ScopedSymbolTable = nested(List.empty, isFunc)

  override def toString: String = s"L$scopeLevel ${this.symbols}"
}
