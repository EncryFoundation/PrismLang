package org.encryfoundation.prismlang.core

object Ast {

  import Types._

  sealed trait Node

  case class Module(contract: Contract, schemas: List[Struct]) extends Node

  case class Struct(id: Ident, typeDescriptor: TypeDescriptor) extends Node

  case class Contract(body: Expr, args: List[(Ident, TypeIdent)]) extends Node

  sealed trait Expr { val tpe: PType }
  object Expr {

    case class Block(body: List[Expr], override val tpe: PType = Nit) extends Expr {
      override def toString: String = body.map(expr => s"$expr").mkString("\n")
    }

    // Syntactical constructions
    case class Let(name: Ident, value: Expr, typeIdentOpt: Option[TypeIdent]) extends Expr {
      override def toString: String = s"let $name = $value"
      override val tpe: PType = PUnit
    }

    case class Def(name: Ident, args: List[(Ident, TypeIdent)], body: Expr, returnTypeIdent: TypeIdent) extends Expr {
      override def toString: String = s"def $name(${args.map{case (argName, argType) => s"$argName: $argType"}.mkString(",")}): $returnTypeIdent = {\n$body\n}"
      override val tpe: PType = PUnit
    }

    case class Lambda(args: List[(Ident, TypeIdent)], body: Expr, override val tpe: PType = Nit) extends Expr {
      override def toString: String = s"lamb (${args.map{case (name, typeIdent) => s"$name: $typeIdent"}.mkString(",")}) = $body"
    }

    case class If(test: Expr, body: Expr, orelse: Expr, override val tpe: PType = Nit) extends Expr

    case class IfLet(name: Ident, typeIdent: TypeIdent, target: Expr, body: Expr, orelse: Expr, override val tpe: PType = Nit) extends Expr {
      override def toString: String = s"if (let $name: $typeIdent = $target) {\n$body\n} else {\n$orelse\n}"
    }
    /** `IfLet` variant for shallow struct matching not by type itself but by its fingerprint. */
    case class IfLetR(name: Ident, typeFingerprint: String, target: Expr, body: Expr, orelse: Expr, override val tpe: PType = Nit) extends Expr

    // Operations
    case class Bool(op: BooleanOp, values: List[Expr]) extends Expr {
      override def toString: String = values.map(_.toString).mkString(s"$op")
      override val tpe: PType = PBoolean
    }

    case class Bin(left: Expr, op: Operator, right: Expr) extends Expr {
      override def toString: String = s"($left$op$right)"
      override val tpe: PType = PInt
    }

    case class Unary(op: UnaryOp, operand: Expr, override val tpe: PType = Nit) extends Expr

    /** Sequences are required for compare to distinguish between: x < 4 < 3 and (x < 4) < 3 */
    case class Compare(left: Expr, ops: List[Ast.CompOp], comparators: List[Expr]) extends Expr {
      override def toString: String = s"$left ${ops.zip(comparators).map{case (comOp, comp) => s"$comOp $comp"}.mkString(" ")}"
      override val tpe: PType = PBoolean
    }

    // Refs
    case class Name(ident: Ident, override val tpe: PType = Nit) extends Expr {
      override def toString: String = ident.toString
    }

    case class Call(func: Expr, args: List[Expr], override val tpe: PType = Nit) extends Expr {
      override def toString: String = s"${func.toString}(${args.map(_.toString).mkString(", ")})"
    }

    case class Attribute(value: Expr, attr: Ident, override val tpe: PType = Nit) extends Expr {
      override def toString: String = s"$value.$attr"
    }

    case class Subscript(value: Expr, slice: SliceOp, override val tpe: PType = Nit) extends Expr {
      override def toString: String = s"$value$slice"
    }

    // Constants
    case class IntConst(value: Long) extends Expr {
      override def toString: String = value.toString
      override val tpe: PType = PInt
    }

    case class ByteConst(value: Byte) extends Expr {
      override def toString: String = value.toString
      override val tpe: PType = PByte
    }

    case class Str(value: String) extends Expr {
      override def toString: String = value
      override val tpe: PType = PString
    }

    case class Collection(elts: List[Expr], override val tpe: PType = Nit) extends Expr {
      override def toString: String = s"(${elts.map(_.toString).mkString(", ")})"
    }

    case class Tuple(elts: List[Expr], override val tpe: PType = Nit) extends Expr {
      override def toString: String = s"(${elts.map(_.toString).mkString(", ")})"
    }

    case class Base58Str(value: String) extends Expr {
      override def toString: String = s"base58'$value'"
      override val tpe: PType = PCollection.ofByte
    }

    case class Base16Str(value: String) extends Expr {
      override def toString: String = s"base16'$value'"
      override val tpe: PType = PCollection.ofByte
    }

    case object True extends Expr {
      override def toString: String = "true"
      override val tpe: PType = PBoolean
    }

    case object False extends Expr {
      override def toString: String = "false"
      override val tpe: PType = PBoolean
    }

    // Transformers
    case class SizeOf(coll: Expr) extends Expr {
      override def toString: String = s"$coll.sizeOf"
      override val tpe: PType = PInt
    }

    case class Exists(coll: Expr, predicate: Expr) extends Expr {
      override def toString: String = s"$coll.exists($predicate)"
      override val tpe: PType = PBoolean
    }

    case class Sum(coll: Expr) extends Expr {
      override def toString: String = s"$coll.sum"
      override val tpe: PType = PInt
    }

    case class Map(coll: Expr, func: Expr, override val tpe: PType = Nit) extends Expr {
      override def toString: String = s"$coll.map($func)"
    }

    case class Filter(coll: Expr, func: Expr, override val tpe: PType = Nit) extends Expr {
      override def toString: String = s"$coll.filter($func)"
    }
  }

  sealed trait SliceOp
  object SliceOp {

    case class Slice(lower: Option[Expr], upper: Option[Expr]) extends SliceOp

    case class Index(value: Expr) extends SliceOp {
      override def toString: String = s"[$value]"
    }
  }

  sealed trait BooleanOp
  object BooleanOp {

    case object And extends BooleanOp {
      override def toString: String = " && "
    }

    case object Or extends BooleanOp {
      override def toString: String = " || "
    }
  }

  sealed trait Operator
  case object Operator {

    case object Add extends Operator {
      override def toString: String = " + "
    }

    case object Sub extends Operator {
      override def toString: String = " - "
    }

    case object Mult extends Operator {
      override def toString: String = " * "
    }

    case object Div extends Operator {
      override def toString: String = " / "
    }

    case object Mod extends Operator

    case object Pow extends Operator
  }

  sealed trait UnaryOp
  object UnaryOp {

    case object Invert extends UnaryOp

    case object Not extends UnaryOp
  }

  sealed trait CompOp { val leftTypeResolution: List[Types.PType]; val rightTypeResolution: List[Types.PType] }
  object CompOp {

    case object Eq extends CompOp {
      override def toString: String = "=="
      lazy val leftTypeResolution: List[Types.PType] = Types.regularTypes
      lazy val rightTypeResolution: List[Types.PType] = Types.regularTypes
    }

    case object NotEq extends CompOp {
      override def toString: String = "!="
      lazy val leftTypeResolution: List[Types.PType] = Eq.leftTypeResolution
      lazy val rightTypeResolution: List[Types.PType] = Eq.leftTypeResolution
    }

    case object Lt extends CompOp {
      override def toString: String = "<"
      lazy val leftTypeResolution: List[Types.PType] = Types.numericTypes
      lazy val rightTypeResolution: List[Types.PType] = Types.numericTypes
    }

    case object LtE extends CompOp {
      override def toString: String = "<="
      lazy val leftTypeResolution: List[Types.PType] = Lt.leftTypeResolution
      lazy val rightTypeResolution: List[Types.PType] = Lt.rightTypeResolution
    }

    case object Gt extends CompOp {
      override def toString: String = ">"
      lazy val leftTypeResolution: List[Types.PType] = Lt.leftTypeResolution
      lazy val rightTypeResolution: List[Types.PType] = Lt.rightTypeResolution
    }

    case object GtE extends CompOp {
      override def toString: String = ">="
      lazy val leftTypeResolution: List[Types.PType] = Lt.leftTypeResolution
      lazy val rightTypeResolution: List[Types.PType] = Lt.rightTypeResolution
    }

    case object In extends CompOp {
      lazy val leftTypeResolution: List[Types.PType] = Types.regularTypes
      lazy val rightTypeResolution: List[Types.PType] = Types.regularTypes.map(PCollection.apply)
    }

    case object NotIn extends CompOp {
      lazy val leftTypeResolution: List[Types.PType] = Types.regularTypes
      lazy val rightTypeResolution: List[Types.PType] = Types.regularTypes.map(PCollection.apply)
    }
  }

  sealed trait TypeDescriptor
  object TypeDescriptor {

    type Field = (Ident, TypeDescriptor)

    case class SimpleType(id: Ident, typeParams: List[TypeDescriptor]) extends TypeDescriptor

    case class ProductType(fields: List[Field]) extends TypeDescriptor
  }

  case class Ident(name: String) {
    override def toString: String = name
  }

  case class TypeIdent(name: String, typeParams: List[TypeIdent] = List.empty) {
    override def toString: String = s"$name"
  }
}
