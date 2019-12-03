package org.encryfoundation.prismlang.compiler.scope

import org.encryfoundation.prismlang.compiler.{SemanticAnalysisException, TypeMatching}
import org.encryfoundation.prismlang.compiler.scope.Symbol.{FunctionSymbol, VariableSymbol}

import scala.collection.immutable.{HashSet, TreeMap}
import cats.implicits._
import cats.instances.set
import cats.instances.map
import cats.instances.string
import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.core.Ast.Expr.Call
import org.encryfoundation.prismlang.core.TypeSystem
import org.encryfoundation.prismlang.core.Types.{PFunc, PType}

case class ScopedSymbolTable(scopeLevel: Int,
                             parentalScopeOpt: Option[ScopedSymbolTable] = None,
                             initialVariables: List[VariableSymbol] = List.empty,
                             initialFunctions: List[FunctionSymbol] = List.empty,
                             isFunc: Boolean = false) extends TypeMatching {

  var variablesSymbols: Map[String, VariableSymbol] =
    TreeMap(initialVariables.map(m => m.name -> m):_*)
  var functionsSymbols: Map[String, Set[FunctionSymbol]] =
    TreeMap(initialFunctions.map(m => m.name -> Set(m)):_*)

  def insert(sym: Symbol): Unit = {
    sym match {
      case variable @ Symbol.VariableSymbol(name, _) =>
        lookupVariable(name).foreach(_ => throw SemanticAnalysisException(s"Variable $name is already defined in scope"))
        variablesSymbols = variablesSymbols.updated(variable.name, variable)
      case function @ Symbol.FunctionSymbol(name, tpe) =>
        //println(s"insert: $function")
        lookupFunction(name, tpe.args.map(_._2)).foreach(_ =>
          throw SemanticAnalysisException(s"Function $name with args list of type (${tpe.args.map(_._2).mkString(",")}) is already defined in scope")
        )
        functionsSymbols = functionsSymbols |+| Map(name -> Set(function))
    }
  }

  def lookupVariable(name: String, currentScopeOnly: Boolean = false): Option[VariableSymbol] = {
    //println(variablesSymbols)
    variablesSymbols.get(name).orElse(if (!currentScopeOnly) parentalScopeOpt.flatMap(_.lookupVariable(name)) else None)
  }

  def lookupFunction(name: String, args: List[PType], currentScopeOnly: Boolean = false): Option[FunctionSymbol] = {
    functionsSymbols.get(name).flatMap(_.find(elem => {
      println(s"${elem.name}||${elem.tpe.args}||${args}")
      elem.tpe.args.map(_._2).zip(args).forall { case (dt, ft) => matchType(dt, ft); true}
    }))
      .orElse(if (!currentScopeOnly) parentalScopeOpt.flatMap(_.lookupFunction(name, args)) else None)
  }

  def lookupVariable(name: Expr.Name): Option[VariableSymbol] = lookupVariable(name.ident.name)

  def possiblePredicateFunc(name: Expr.Name, argType: PType, typeSystem: TypeSystem): Option[FunctionSymbol] = {
    lookupFunction(name.ident.name, List(argType))
  }

  def lookupFunctionByCall(funcCall: Call, typeSystem: TypeSystem, typesResolver: Expr => PType): Option[FunctionSymbol] = funcCall match {
    case Expr.Call(func @ Expr.Name(_, _), args, _) =>
      println(s"Looking for $func func. Args: ${args}")
      val argsTypes = args.flatMap {
        case call@ Expr.Call(_, _, _) =>
          lookupFunctionByCall(call, typeSystem, typesResolver).map(func => List(func.tpe))
        case name: Expr.Name =>
          lookupVariable(name).map( variable => List(variable.tpe))
        case anotherExpr: Expr =>
          println(s"another expr: ${anotherExpr.getClass}")
          List(typesResolver(anotherExpr)).some
      }.flatten
      //todo: Add check for eq args.length and argsTypes.length
      val res = lookupFunction(func.ident.name, argsTypes)
      println(s"res of looking: ${res}")
      res
    case el =>
      println(s"Looking for $el")
      None
  }

  def lookup(name: String, currentScopeOnly: Boolean = false): Option[Symbol] =
    variablesSymbols.get(name).orElse(if (!currentScopeOnly) parentalScopeOpt.flatMap(_.lookup(name)) else None)

  def nested(variables: List[VariableSymbol], functions: List[FunctionSymbol], isFunc: Boolean = false): ScopedSymbolTable =
    ScopedSymbolTable(this.scopeLevel + 1, Some(this), variables, functions, isFunc)

  def nested(isFunc: Boolean): ScopedSymbolTable = nested(List.empty, List.empty)

  override def toString: String = s"L$scopeLevel ${this.variablesSymbols}"
}
