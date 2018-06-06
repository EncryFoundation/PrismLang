package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.compiler.scope.ScopedSymbolTable
import org.encryfoundation.prismlang.core.{Constants, TypeSystem, Types}
import org.encryfoundation.prismlang.core.Ast._
import org.encryfoundation.prismlang.compiler.scope.Symbol
import scorex.crypto.encode.{Base16, Base58}

import scala.util.Try

case class StaticAnalyser(types: TypeSystem) {

  import StaticAnalyser._

  var scopes: List[ScopedSymbolTable] = List(ScopedSymbolTable.initial)

  def scanContract(contract: Expr.Contract): Try[Expr.Contract] = Try {
    val args: List[(String, Types.PType)] = resolveArgs(contract.args)
    args.foreach(p => currentScope.insert(Symbol(p._1, p._2)))
    val bodyS: Expr = scan(contract.body)
    matchType(contract.body.tpe, Types.PBoolean)
    contract.copy(bodyS)
  }

  /** Scan each node according to the specific rule, then
    * compute its type (if the node is untyped by default)
    * and return modified copy of the node. */
  def scan: Scan =
    scanLet orElse
      scanDef orElse
      scanLambda orElse
      scanIf orElse
      scanBlock orElse
      scanSimpleExpr orElse
      scanRef orElse
      scanCollection orElse
      scanConstant orElse
      pass

  def scanLet: Scan = {
    /** Scan the value to be assigned to the const, compute
      * its type, compare it with the declared one (if declared)
      * and add name to the scope. */
    case let @ Expr.Let(name, value, typeIdentOpt) =>
      val valueS: Expr = scan(value)
      val valueT: Types.PType = valueS.tpe
      typeIdentOpt.foreach(t => matchType(valueT, resolveType(t)))
      addToScope(name, valueT)
      let.copy(name, valueS, typeIdentOpt)
  }

  def scanDef: Scan = {
    /** Resolve arguments and return type, insert function symbol
      * to the current scope, create scope for the function body
      * with argument inserted, scan body and compare its type
      * with the declared one, pop function scope. */
    case func @ Expr.Def(ident, args, body, returnTypeIdent) =>
      val declaredReturnType: Types.PType = resolveType(returnTypeIdent)
      val params: List[(String, Types.PType)] = resolveArgs(args)
      currentScope.insert(Symbol(ident.name, Types.PFunc(params, declaredReturnType)))
      val bodyScope: ScopedSymbolTable = ScopedSymbolTable.nested(currentScope, isFunc = true)
      params.foreach(p => bodyScope.insert(Symbol(p._1, p._2)))
      scopes = bodyScope :: scopes
      val bodyS: Expr = scan(body)
      matchType(declaredReturnType, body.tpe)
      scopes = scopes.tail
      func.copy(ident, args, bodyS, returnTypeIdent)
  }

  def scanLambda: Scan = {
    /** Resolve arguments, create scope for the function body
      * with argument inserted, scan body, pop function scope. */
    case lamb @ Expr.Lambda(args, body, _) =>
      val params: List[(String, Types.PType)] = resolveArgs(args)
      val bodyScope: ScopedSymbolTable = ScopedSymbolTable.nested(currentScope, isFunc = true)
      params.foreach(p => bodyScope.insert(Symbol(p._1, p._2)))
      scopes = bodyScope :: scopes
      val bodyS: Expr = scan(body)
      scopes = scopes.tail
      lamb.copy(args, bodyS, computeType(lamb))
  }

  def scanIf: Scan = {
    /** Scan test expression ensuring its type is `Bool`,
      * then scan bodies of each branch. */
    case ifExp @ Expr.If(test, body, orelse, _) =>
      val testS: Expr = scan(test)
      matchType(test.tpe, Types.PBoolean)
      val bodyScope: ScopedSymbolTable = ScopedSymbolTable.nested(currentScope)
      scopes = bodyScope :: scopes
      val bodyS: Expr = scan(body)
      scopes = scopes.tail
      val elseScope: ScopedSymbolTable = ScopedSymbolTable.nested(currentScope)
      scopes = elseScope :: scopes
      val orelseS: Expr = scan(orelse)
      scopes = scopes.tail
      ifExp.copy(testS, bodyS, orelseS, computeType(ifExp))
    /** Scan the target to be assigned, ensure that its type
      * can be cast to the declared local type, then scan
      * bodies of each branch. */
    case letIf @ Expr.IfLet(local, typeIdent, target, body, orelse, _) =>
      val targetS: Expr = scan(target)
      val localT: Types.PType = resolveType(typeIdent)
      if (localT.isSubtypeOf(target.tpe)) error(s"${target.tpe} can not be cast to $localT")
      val bodyScope: ScopedSymbolTable = ScopedSymbolTable.nested(currentScope)
      bodyScope.insert(Symbol(local.name, localT))
      scopes = bodyScope :: scopes
      val bodyS: Expr = scan(body)
      scopes = scopes.tail
      val elseScope: ScopedSymbolTable = ScopedSymbolTable.nested(currentScope)
      scopes = elseScope :: scopes
      val orelseS: Expr = scan(orelse)
      scopes = scopes.tail
      letIf.copy(local, typeIdent, targetS, bodyS, orelseS, computeType(letIf))
  }

  def scanBlock: Scan = {
    /** Scan each expression in `body`. */
    case block @ Expr.Block(body, _) =>
      val bodyS: List[Expr] = body.map(scan)
      block.copy(bodyS, computeType(block))
  }

  def scanSimpleExpr: Scan = {
    /** Scan operands ensuring they are of `PInt` type. */
    case bin @ Expr.Bin(left, op, right) =>
      val leftS: Expr = scan(left)
      val rightS: Expr = scan(right)
      List(leftS, rightS).foreach(exp => matchType(Types.PInt, exp.tpe))
      bin.copy(leftS, op, rightS)
    /** Scan operands ensuring they support `op`. */
    case bool @ Expr.Bool(op, values) =>
      // TODO: Check whether all operands support `op`.
      val valuesS: List[Expr] = values.map(scan)
      bool.copy(op, valuesS)
    /** Scan operand ensuring it is of `PInt` type. */
    case unary @ Expr.Unary(op, operand) =>
      val operandS: Expr = scan(operand)
      matchType(Types.PInt, operandS.tpe, Some(s"${operandS.tpe} does not support '$op'"))
      unary.copy(op, operandS)
    /** Scan operands ensuring it supports `op`. */
    case compare @ Expr.Compare(left, ops, comparators) =>
      // TODO: Check whether all operands support `op`.
      val leftS: Expr = scan(left)
      val comparatorsS: List[Expr] = comparators.map(scan)
      compare.copy(leftS, ops, comparatorsS)
  }

  def scanRef: Scan = {
    /** Compute type of the referenced name and return its modified copy. */
    case name @ Expr.Name(ident, _) => name.copy(ident, computeType(name))
    /** Case when some plain function is called. Scan each expr
      * in `args`, lookup name of the called function in the scope
      * ensuring it is of `PFunc` type, check that the type of each
      * of them matches the declared one. */
    case call @ Expr.Call(func @ Expr.Name(ident, _), args, _) =>
      val funcS: Expr = scan(func)
      val argsS: List[Expr] = args.map(scan)
      currentScope.lookup(ident.name).foreach { symbol =>
        symbol.tpe match {
          case Types.PFunc(declaredArgs, _) =>
            declaredArgs.map(_._2).zip(argsS.map(_.tpe)).foreach { case (dt, ft) => matchType(dt, ft) }
          case _ => error(s"${ident.name} is not a function")
        }
      }
      call.copy(funcS, argsS, computeType(call))
    /** Scan value, its type will be checked in `computeType()`. */
    case attr @ Expr.Attribute(value, attrName, _) =>
      val valueS: Expr = scan(value)
      attr.copy(valueS, attrName, computeType(attr.copy(valueS)))
  }

  def scanCollection: Scan = {
    /** Scan each element contained in collection ensuring actual coll
      * size does not overflow `CollMaxElements`, then check type
      * consistency of all elements. */
    case coll @ Expr.Collection(elts, _) =>
      if (elts.size > Constants.CollMaxLength)
        error(s"Collection size limit overflow (${elts.size} > ${Constants.CollMaxLength})")
      val eltsS: List[Expr] = elts.map(scan)
      eltsS.foreach(elt => matchType(eltsS.head.tpe, elt.tpe, Some(s"Collection is inconsistent, ${elt.tpe} stands out.")))
      coll.copy(eltsS, computeType(coll.copy(eltsS)))
  }

  def scanConstant: Scan = {
    /** Scan each element of tuple ensuring its actual size does not
      * overflow `TupleMaxLength`, then check type consistency of all elements. */
    case tuple @ Expr.Tuple(elts, _) =>
      if (elts.size > Constants.TupleMaxLength) error(s"Tuple size limit overflow (${elts.size} > ${Constants.TupleMaxLength})")
      val eltsS: List[Expr] = elts.map(scan)
      eltsS.foreach(elt => matchType(eltsS.head.tpe, elt.tpe, Some(s"Tuple is inconsistent, ${elt.tpe} stands out.")))
      tuple.copy(eltsS, computeType(tuple.copy(eltsS)))
    /** Has default type, check max length overflow and base58-string validity. */
    case base58 @ Expr.Base58Str(value) =>
      if (value.length > Constants.ByteStringMaxLength)
        error(s"String max length overflow (${value.length} > ${Constants.ByteStringMaxLength})")
      else if (Base58.decode(value).isFailure) error(s"Invalid Base58 string '$value'")
      base58
    /** Has default type, check max length overflow and base16-string validity. */
    case base16 @ Expr.Base16Str(value) =>
      if (value.length > Constants.ByteStringMaxLength)
        error(s"String max length overflow (${value.length} > ${Constants.ByteStringMaxLength})")
      else if (Base16.decode(value).isFailure) error(s"Invalid Base16 string '$value'")
      base16
    /** Has default type, check max length overflow. */
    case string @ Expr.Str(value) =>
      if (value.length > Constants.StringMaxLength)
        error(s"String max length overflow (${value.length} > ${Constants.StringMaxLength})")
      string
    /** Make sure given `value` does not overflow `Long.MaxSize`. */
    case int @ Expr.IntConst(value) =>
      if (value > Long.MaxValue) error(s"int64 max size overflow ($value > ${Long.MaxValue})")
      int
  }

  def pass: Scan = {
    case any => any
  }

  def computeType(expr: Expr): Types.PType = if (!expr.tpe.isNit) expr.tpe else expr match {
    /** Type of the block is the type of its last expr. */
    case Expr.Block(body, _) => computeType(body.last)
    /** Type of the referenced name is looked up in the scope. */
    case Expr.Name(Ident(name), _) => currentScope.lookup(name).map(_.tpe).getOrElse(error(s"$name is undefined"))
    /** In this case some referenced name is called, the type
      * is inferred from the return-type of the ref, which is
      * looked up in the scope. */
    case Expr.Call(func @ Expr.Name(ident, _), _, _) => computeType(func) match {
      case Types.PFunc(_, retT) => retT
      case _ => error(s"${ident.name} is not a function")
    }
    /** Type of attribute is inferred from the type of
      * corresponding field of the object. */
    case Expr.Attribute(value, attr, _) => computeType(value) match {
      case prod: Types.Product => prod.getAttrType(attr.name).getOrElse(error(s"${attr.name} is not defined in ${prod.ident}"))
      case other => error(s"${other.ident} is not an object")
    }
    case Expr.If(_, body, orelse, _) => findCommonType(computeType(body), computeType(orelse))
    case Expr.IfLet(_, _, _, body, orelse, _) => findCommonType(computeType(body), computeType(orelse))
    case Expr.Lambda(args, body, _) => Types.PFunc(resolveArgs(args), computeType(body))
    case Expr.Tuple(elts, _) => Types.PTuple(computeType(elts.head), elts.size)
    case Expr.Collection(elts, _) => Types.PCollection(computeType(elts.head))
  }

  def currentScope: ScopedSymbolTable = scopes.head

  /** Resolves the type from its string representation
    * (including type parameters). */
  def resolveType(ident: TypeIdent): Types.PType = {
    val typeParams: List[Types.PType] = ident.typeParams.map(p => types.typeByIdent(p)
      .getOrElse(error(s"Type '$p' is undefined.")))
    types.typeByIdent(ident.name).map {
      case Types.PCollection(_) =>
        if (typeParams.size == 1) Types.PCollection(typeParams.head)
        else error("'Array[T]' takes exactly one type parameter")
      case Types.PTuple(_, qty) =>
        if (typeParams.size == 1) Types.PTuple(typeParams.head, qty)
        else error("'Tuple[T]' takes exactly one type parameter")
      case Types.POption(_) =>
        if (typeParams.size == 1) Types.POption(typeParams.head)
        else error("'Option[T]' takes exactly one type parameter")
      case otherT: Types.PType =>
        if (typeParams.isEmpty) otherT
        else error(s"'$otherT' does not take type parameters")
    }.getOrElse(error(s"Type '${ident.name}' is undefined."))
  }

  def resolveArgs(args: List[(Ident, TypeIdent)]): List[(String, Types.PType)] =
    args.map { case (id, typeId) => id.name -> resolveType(typeId) }

  def addToScope(ident: Ident, tpe: Types.PType): Unit =
    currentScope.insert(Symbol(ident.name, tpe))

  def matchType(required: Types.PType, actual: Types.PType, msgOpt: Option[String] = None): Unit =
    if (!(required == actual || actual.isSubtypeOf(required))) error(msgOpt.getOrElse(s"Type mismatch: $required != $actual"))

  /** Find common type for `t1` and `t2`. */
  def findCommonType(t1: Types.PType, t2: Types.PType): Types.PType = {
    if (t1 == t2) t1
    else if (t2.isSubtypeOf(t1)) t1
    else if (t1.isSubtypeOf(t2)) t2
    else (t1, t2) match {
      case (p1: Types.Product, p2: Types.Product) => findCommonType(p1.superType, p2.superType)
      case (_, _) => Types.PAny
    }
  }

  def error(msg: String) = throw new SemanticAnalysisException(msg)
}

object StaticAnalyser {

  type Scan = PartialFunction[Expr, Expr]
}
