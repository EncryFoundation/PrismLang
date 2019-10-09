package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.compiler.scope.{PredefinedScope, ScopedSymbolTable, Symbol}
import org.encryfoundation.prismlang.core.{Constants, TypeSystem, Types}
import org.encryfoundation.prismlang.core.Ast._
import scorex.crypto.encode.{Base16, Base58}

case class StaticAnalyser(initialScope: ScopedSymbolTable, types: TypeSystem) extends TypeMatching {

  import StaticAnalyser._

  var scopes: List[ScopedSymbolTable] = List(initialScope)

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
      scanTransformers orElse
      pass

  def scanLet: Scan = {
    /** Scan the value to be assigned to the const, compute
      * its type, compare it with the declared one (if declared)
      * and add name to the scope. Note, explicit type annotation
      * which is stored in `typeIdentOpt` is no longer matters
      * when the node is scanned, so we just throw it out. */
    case let @ Expr.Let(name, value, typeIdentOpt) =>
      val valueS: Expr = scan(value)
      val valueT: Types.PType = valueS.tpe
      typeIdentOpt.foreach(t => matchType(types.resolveType(t), valueT))
      addToScope(name, valueT)
      let.copy(name, valueS, typeIdentOpt)
  }

  def scanDef: Scan = {
    /** Resolve arguments and return type, insert function symbol
      * to the current scope, create scope for the function body
      * with argument inserted, scan body and compare its type
      * with the declared one, pop function scope. */
    case func @ Expr.Def(ident, args, body, returnTypeIdent) =>
      val declaredReturnType: Types.PType = types.resolveType(returnTypeIdent)
      val params: List[(String, Types.PType)] = types.resolveArgs(args)
      currentScope.insert(Symbol(ident.name, Types.PFunc(params, declaredReturnType)))
      val bodyScope: ScopedSymbolTable = currentScope.nested(params.map(p => Symbol(p._1, p._2)), isFunc = true)
      scopes = bodyScope :: scopes
      val bodyS: Expr = scan(body)
      if (bodyS.toString.contains(ident.name)) error("Recursion not allowed")
      matchType(declaredReturnType, bodyS.tpe)
      scopes = scopes.tail
      func.copy(ident, args, bodyS, returnTypeIdent)
  }

  def scanLambda: Scan = {
    /** Resolve arguments, create scope for the function body
      * with argument inserted, scan body, pop function scope. */
    case lamb @ Expr.Lambda(args, body, _) =>
      val params: List[(String, Types.PType)] = types.resolveArgs(args)
      val bodyScope: ScopedSymbolTable = currentScope.nested(params.map(p => Symbol(p._1, p._2)), isFunc = true)
      scopes = bodyScope :: scopes
      val bodyS: Expr = scan(body)
      scopes = scopes.tail
      lamb.copy(args, bodyS, computeType(lamb.copy(args, bodyS)))
  }

  def scanIf: Scan = {
    /** Scan test expression ensuring its type is `Bool`,
      * then scan bodies of each branch. */
    case ifExp @ Expr.If(test, body, orelse, _) =>
      val testS: Expr = scan(test)
      matchType(testS.tpe, Types.PBoolean)
      val bodyScope: ScopedSymbolTable = currentScope.nested(isFunc = false)
      scopes = bodyScope :: scopes
      val bodyS: Expr = scan(body)
      scopes = scopes.tail
      val elseScope: ScopedSymbolTable = currentScope.nested(isFunc = false)
      scopes = elseScope :: scopes
      val orelseS: Expr = scan(orelse)
      scopes = scopes.tail
      ifExp.copy(testS, bodyS, orelseS, computeType(ifExp.copy(testS, bodyS, orelseS)))

    /** Scan the target to be assigned, ensure that its type
      * can be cast to the declared local type, then scan
      * bodies of each branch. If required type is `StructTag`
      * then return special `IfLetR` node. */
    case ifLet @ Expr.IfLet(local, typeIdent, target, body, orelse, _) =>
      val targetS: Expr = scan(target)
      val localT: Types.PType = types.resolveType(typeIdent)
      if (localT.isSubtypeOf(target.tpe)) error(s"${target.tpe} can not be cast to $localT")
      val bodyScope: ScopedSymbolTable = currentScope.nested(List(Symbol(local.name, localT)))
      scopes = bodyScope :: scopes
      val bodyS: Expr = scan(body)
      scopes = scopes.tail
      val elseScope: ScopedSymbolTable = currentScope.nested(isFunc = false)
      scopes = elseScope :: scopes
      val orelseS: Expr = scan(orelse)
      scopes = scopes.tail
      val innerCopy: Expr = ifLet.copy(target = targetS, body = bodyS, orelse = orelseS)
      localT match {
        case tag: Types.StructTag => Expr.IfLetR(local, tag.fingerprint, targetS, bodyS, orelseS, computeType(innerCopy))
        case _ => ifLet.copy(local, typeIdent, targetS, bodyS, orelseS, computeType(innerCopy))
      }
  }

  def scanBlock: Scan = {
    /** Scan each expression in `body`. */
    case block @ Expr.Block(body, _) =>
      val bodyScope: ScopedSymbolTable = currentScope.nested(isFunc = false)
      scopes = bodyScope :: scopes
      val bodyS: List[Expr] = body.map(scan)
      val blockType: Types.PType = computeType(block.copy(bodyS))
      scopes = scopes.tail
      block.copy(bodyS, blockType)
  }

  def scanSimpleExpr: Scan = {
    /** Scan operands ensuring they are of `PInt` type. */
    case bin @ Expr.Bin(left, op, right) =>
      val leftS: Expr = scan(left)
      val rightS: Expr = scan(right)
      isValidBinaryOperation(leftS, rightS, op)
      bin.copy(leftS, op, rightS)

    /** Scan operands ensuring they support `op`. */
    case bool @ Expr.Bool(op, values) =>
      val valuesS: List[Expr] = values.map(scan)
      valuesS.foreach(exp => matchType(Types.PBoolean, exp.tpe))
      bool.copy(op, valuesS)

    /** Scan operand ensuring it is of `PInt` type. */
    case unary @ Expr.Unary(op, operand, _) =>
      val operandS: Expr = scan(operand)
      op match {
        case UnaryOp.Not => matchType(Types.PBoolean, operandS.tpe, Some(s"${operandS.tpe} does not support '$op'"))
        case UnaryOp.Invert => matchType(Types.PInt, operandS.tpe, Some(s"${operandS.tpe} does not support '$op'"))
      }
      unary.copy(op, operandS, computeType(unary.copy(operand = operandS)))

    /** Scan operands ensuring they all support `op`. */
    case compare @ Expr.Compare(left, ops, comparators) =>
      val leftS: Expr = scan(left)
      val comparatorsS: List[Expr] = comparators.map(scan)
      ops.foreach { op => if (!op.leftTypeResolution.exists(t => rightType(t, leftS.tpe))) error(s"$op is not supported on ${leftS.tpe}") }
      if (!ops.zip(comparatorsS).forall { case (op, comp) => op.rightTypeResolution.exists(t => rightType(t, comp.tpe)) })
        error(s"Comparison between unsupported types")
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

    /** Scan value, its type will be checked in `computeType()` (should be Object). */
    case attr @ Expr.Attribute(value, attrName, _) =>
      val valueS: Expr = scan(value)
      attr.copy(valueS, attrName, computeType(attr.copy(valueS)))

    /** Scan value, its type will be checked in `computeType()` (should be Collection). */
    case slice @ Expr.Subscript(value, op, _) =>
      val valueS: Expr = scan(value)
      slice.copy(valueS, op, computeType(slice.copy(valueS)))
  }

  def scanCollection: Scan = {
    /** Scan each element contained in collection ensuring actual coll
      * size does not overflow `CollMaxElements`, then check type
      * consistency of all elements and collection nesting. */
    case coll @ Expr.Collection(elts, _) =>
      if (elts.size > Constants.CollMaxLength) error(s"Collection size limit overflow (${elts.size} > ${Constants.CollMaxLength})")
      else if (elts.size < 1) error("Empty collection")
      val eltsS: List[Expr] = elts.map(scan)
      eltsS.foreach(elt => matchType(eltsS.head.tpe, elt.tpe, Some(s"Collection is inconsistent, ${elt.tpe} stands out.")))
      eltsS.head.tpe match {
        case Types.PCollection(inT) if inT.isCollection => error("Illegal level of nesting")
        case _ => // Do nothing
      }
      coll.copy(eltsS, computeType(coll.copy(eltsS)))
  }

  def scanConstant: Scan = {
    /** Scan each element of tuple ensuring its actual size does not
      * overflow `TupleMaxLength`, then check type consistency of all elements. */
    case tuple @ Expr.Tuple(elts, _) =>
      if (elts.size > Constants.TupleMaxDim) error(s"Tuple size limit overflow (${elts.size} > ${Constants.TupleMaxDim})")
      else if (elts.size < 1) error("Empty tuple")
      val eltsS: List[Expr] = elts.map(scan)
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

  def scanTransformers: Scan = {
    /** Ensure `coll` is of type `Collection` and `func` is of
      * type `Func`, check whether `func` can be applied to `coll`. */
    case map @ Expr.Map(coll, func, _) =>
      val collS: Expr = scan(coll)
      val funcS: Expr = scan(func)
      if (!collS.tpe.isCollection) error(s"'map()' is inapplicable to ${collS.tpe}")
      else if (!funcS.tpe.isFunc) error(s"'${funcS.tpe}' is not a function")
      else if (!isApplicableTo(collS, funcS)) error(s"${funcS.tpe} is inapplicable to ${collS.tpe}")
      map.copy(collS, funcS, computeType(map.copy(collS, funcS)))

    /** Ensure `coll` is of type `Collection` and `func` is of
      * type `Func`, check whether `func` can be applied to `coll`. */
    case exists @ Expr.Exists(coll, predicate) =>
      val collS: Expr = scan(coll)
      val predicateS: Expr = scan(predicate)
      if (!collS.tpe.isCollection) error(s"'exists()' is inapplicable to ${collS.tpe}")
      else if (!predicateS.tpe.isFunc) error(s"'${predicateS.tpe}' is not a function")
      else if (!isApplicableTo(collS, predicateS)) error(s"${predicateS.tpe} is inapplicable to ${collS.tpe}")
      exists.copy(collS, predicateS)

    /** Ensure `coll` is of type `Collection` and `func` is of
      * type `Func`, check whether `func` can be applied to `coll`. */
    case filter @ Expr.Filter(coll, predicate, _) =>
      val collS: Expr = scan(coll)
      val predicateS: Expr = scan(predicate)
      if (!collS.tpe.isCollection) error(s"'filter()' is inapplicable to ${collS.tpe}")
      else if (!predicateS.tpe.isFunc) error(s"'${predicateS.tpe}' is not a function")
      else if (!isApplicableTo(collS, predicateS)) error(s"${predicateS.tpe} is inapplicable to ${collS.tpe}")
      filter.copy(collS, predicateS, computeType(filter.copy(collS, predicateS)))
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
      case tag: Types.TaggedType if tag.isProduct => tag.underlyingType.asInstanceOf[Types.Product].getAttrType(attr.name).getOrElse(error(s"${attr.name} is not defined in ${tag.underlyingType.ident}"))
      case other => error(s"${other.ident} is not an object")
    }

    /** Infer type for the `value`, ensure it is of type `Collection`,
      * in case of by-index subscription we infer resulted type as the
      * type of collection element, in case of slicing we have the same
      * type as subscripted collection has. */
    case Expr.Subscript(value, op, _) => computeType(value) match {
      case coll @ Types.PCollection(inT) => op match {
        case _: SliceOp.Index => inT
        case _: SliceOp.Slice => coll
      }
      case otherT => error(s"$otherT does not support subscription")
    }
    case Expr.Unary(_, operand, _) => computeType(operand)
    case Expr.If(_, body, orelse, _) => findCommonType(computeType(body), computeType(orelse))
    case Expr.IfLet(_, _, _, body, orelse, _) => findCommonType(computeType(body), computeType(orelse))
    case Expr.Lambda(args, body, _) => Types.PFunc(types.resolveArgs(args), computeType(body))
    case Expr.Tuple(elts, _) => Types.PTuple(elts.map(elt => computeType(elt)))
    case Expr.Collection(elts, _) => Types.PCollection(computeType(elts.head))
    case Expr.Map(_, func, _) => computeType(func) match {
      case Types.PFunc(_, retT) => Types.PCollection(retT)
      case otherT => error(s"$otherT is not a function")
    }
    case Expr.Filter(coll, _, _) => computeType(coll)
  }

  def currentScope: ScopedSymbolTable = scopes.head

  def isApplicableTo(coll: Expr, func: Expr): Boolean = (coll.tpe, func.tpe) match {
    case (coll: Types.PCollection, func: Types.PFunc) => coll.isApplicable(func)
    case (tag: Types.TaggedType, func: Types.PFunc) => tag.underlyingType.isApplicable(func)
    case _ =>
      println("here")
      false
  }

  def addToScope(ident: Ident, tpe: Types.PType): Unit =
    currentScope.insert(Symbol(ident.name, tpe))

  /** Find common type for `t1` and `t2`. */
  def findCommonType(t1: Types.PType, t2: Types.PType): Types.PType = {
    if (t1 == t2) t1
    else if (t2.isSubtypeOf(t1)) t1
    else if (t1.isSubtypeOf(t2)) t2
    else (t1, t2) match {
      case (p1: Types.Product, p2: Types.Product) => (p1.ancestor, p2.ancestor) match {
        case (Some(a1), Some(a2)) => findCommonType(a1, a2)
        case _ => Types.PAny
      }
      case _ => Types.PAny
    }
  }
}

object StaticAnalyser {

  type Scan = PartialFunction[Expr, Expr]

  def apply(initialMembers: List[(String, Types.PType)], types: List[Types.PType]): StaticAnalyser =
    StaticAnalyser(ScopedSymbolTable(1, None, initialMembers.map(m => Symbol(m._1, m._2))), TypeSystem(types))

  def default: StaticAnalyser = StaticAnalyser(ScopedSymbolTable(1, None, PredefinedScope.members.map(m => Symbol(m._1, m._2))), TypeSystem.default)
}
