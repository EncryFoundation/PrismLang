package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.compiler.scope.ScopedSymbolTable
import org.encryfoundation.prismlang.core.{TypeSystem, Types}
import org.encryfoundation.prismlang.core.Ast._
import org.encryfoundation.prismlang.compiler.scope.Symbol

case class StaticAnalyser(types: TypeSystem) {

  import StaticAnalyser._

  var scopes: List[ScopedSymbolTable] = List(ScopedSymbolTable.initial)

  def scan: Scan =
    scanLet orElse
      scanDef orElse
      pass

  def scanLet: Scan = {
    /** Scan the value to be assigned to the const, compute
      * its type, compare it with the declared one (if declared)
      * and add name to the scope. */
    case Expr.Let(name, value, typeIdentOpt) =>
      scan(value)
      val valueType: Types.PType = value.tpe
      typeIdentOpt.foreach(t => matchType(valueType, getType(t)))
      addToScope(name, valueType)
  }

  def scanDef: Scan = {
    /** Resolve arguments and return type, insert function symbol
      * to the current scope, create scope for the function body
      * with argument inserted, scan body and compare its type
      * with the declared one, pop function scope. */
    case Expr.Def(ident, args, body, returnTypeIdent) =>
      val declaredReturnType: Types.PType = getType(returnTypeIdent)
      val params: List[(String, Types.PType)] = args.map { case (id, typeId) => id.name -> getType(typeId) }
      currentScope.insert(Symbol(ident.name, Types.PFunc(params, declaredReturnType)))
      val bodyScope: ScopedSymbolTable = ScopedSymbolTable.nested(currentScope, isFunc = true)
      params.foreach(p => bodyScope.insert(Symbol(p._1, p._2)))
      scopes = bodyScope :: scopes
      scan(body)
      matchType(declaredReturnType, body.tpe)
      scopes = scopes.tail
  }

  def pass: Scan = {
    case _ =>
  }

  def currentScope: ScopedSymbolTable = scopes.head

  /** Resolves the type from its string representation
    * (including type parameters) */
  def getType(ident: TypeIdent): Types.PType = {
    val typeParams: List[Types.PType] = ident.typeParams.map(p => types.typeByIdent(p)
      .getOrElse(error(s"Type '$p' is undefined.")))
    types.typeByIdent(ident.name).map {
      case Types.PArray(_) =>
        if (typeParams.size == 1) Types.PArray(typeParams.head)
        else error("'Array[T]' takes exactly one type parameter")
      case Types.POption(_) =>
        if (typeParams.size == 1) Types.POption(typeParams.head)
        else error("'Option[T]' takes exactly one type parameter")
      case otherT: Types.PType =>
        if (typeParams.isEmpty) otherT
        else error(s"'$otherT' does not take type parameters")
    }.getOrElse(error(s"Type '${ident.name}' is undefined."))
  }

  def addToScope(ident: Ident, tpe: Types.PType): Unit =
    currentScope.insert(Symbol(ident.name, tpe))

  def matchType(t1: Types.PType, t2: Types.PType): Unit =
    if (!(t1 == t2 || t2.isSubtypeOf(t1))) error(s"Type mismatch: $t1 != $t2")

  def error(msg: String) = throw new SemanticAnalysisException(msg)
}

object StaticAnalyser {

  type Scan = PartialFunction[Expr, Unit]
}
