package org.encryfoundation.prismlang.core

object Ast {

  import Types._

  sealed trait Node

  case class Module(contract: Contract, schemas: List[Struct]) extends Node

  case class Struct(id: Ident, typeDescriptor: TypeDescriptor) extends Node

  case class Contract(body: Expr, args: List[(Ident, TypeIdent)]) extends Node

  sealed trait Expr { val tpe: PType }
  object Expr {

    case class Block(body: List[Expr], override val tpe: PType = Nit) extends Expr

    // Syntactical constructions
    case class Let(name: Ident, value: Expr, typeIdentOpt: Option[TypeIdent]) extends Expr { override val tpe: PType = PUnit }

    case class Def(name: Ident, args: List[(Ident, TypeIdent)], body: Expr, returnTypeIdent: TypeIdent) extends Expr { override val tpe: PType = PUnit }

    case class Lambda(args: List[(Ident, TypeIdent)], body: Expr, override val tpe: PType = Nit) extends Expr

    case class If(test: Expr, body: Expr, orelse: Expr, override val tpe: PType = Nit) extends Expr

    case class IfLet(name: Ident, typeIdent: TypeIdent, target: Expr, body: Expr, orelse: Expr, override val tpe: PType = Nit) extends Expr
    /** `IfLet` variant for shallow struct matching not by type itself but by its fingerprint. */
    case class IfLetR(name: Ident, typeFingerprint: String, target: Expr, body: Expr, orelse: Expr, override val tpe: PType = Nit) extends Expr

    // Operations
    case class Bool(op: BooleanOp, values: List[Expr]) extends Expr { override val tpe: PType = PBoolean }

    case class Bin(left: Expr, op: Operator, right: Expr) extends Expr { override val tpe: PType = PInt }

    case class Unary(op: UnaryOp, operand: Expr) extends Expr { override val tpe: PType = PInt }

    /** Sequences are required for compare to distinguish between: x < 4 < 3 and (x < 4) < 3 */
    case class Compare(left: Expr, ops: List[Ast.CompOp], comparators: List[Expr]) extends Expr { override val tpe: PType = PBoolean }

    // Refs
    case class Name(ident: Ident, override val tpe: PType = Nit) extends Expr

    case class Call(func: Expr, args: List[Expr], override val tpe: PType = Nit) extends Expr

    case class Attribute(value: Expr, attr: Ident, override val tpe: PType = Nit) extends Expr

    case class Subscript(value: Expr, slice: SliceOp, override val tpe: PType = Nit) extends Expr

    // Constants
    case class IntConst(value: Long) extends Expr { override val tpe: PType = PInt }

    case class ByteConst(value: Byte) extends Expr { override val tpe: PType = PByte }

    case class Str(value: String) extends Expr { override val tpe: PType = PString }

    case class Collection(elts: List[Expr], override val tpe: PType = Nit) extends Expr

    case class Tuple(elts: List[Expr], override val tpe: PType = Nit) extends Expr

    case class Base58Str(value: String) extends Expr { override val tpe: PType = PCollection.ofByte }

    case class Base16Str(value: String) extends Expr { override val tpe: PType = PCollection.ofByte }

    case object True extends Expr { override val tpe: PType = PBoolean }

    case object False extends Expr { override val tpe: PType = PBoolean }

    // Transformers
    case class SizeOf(coll: Expr) extends Expr { override val tpe: PType = PInt }

    case class Exists(coll: Expr, predicate: Expr) extends Expr { override val tpe: PType = PBoolean }

    case class Sum(coll: Expr) extends Expr { override val tpe: PType = PInt }

    case class Map(coll: Expr, func: Expr, override val tpe: PType = Nit) extends Expr
  }

  sealed trait SliceOp
  object SliceOp {

    case class Slice(lower: Option[Expr], upper: Option[Expr]) extends SliceOp

    case class Index(value: Expr) extends SliceOp
  }

  sealed trait BooleanOp
  object BooleanOp {

    case object And extends BooleanOp

    case object Or extends BooleanOp
  }

  sealed trait Operator
  case object Operator {

    case object Add extends Operator

    case object Sub extends Operator

    case object Mult  extends Operator

    case object Div  extends Operator

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
      lazy val leftTypeResolution: List[Types.PType] = Types.regularTypes
      lazy val rightTypeResolution: List[Types.PType] = Types.regularTypes
    }

    case object NotEq extends CompOp {
      lazy val leftTypeResolution: List[Types.PType] = Eq.leftTypeResolution
      lazy val rightTypeResolution: List[Types.PType] = Eq.leftTypeResolution
    }

    case object Lt extends CompOp {
      lazy val leftTypeResolution: List[Types.PType] = Types.numericTypes
      lazy val rightTypeResolution: List[Types.PType] = Types.numericTypes
    }

    case object LtE extends CompOp {
      lazy val leftTypeResolution: List[Types.PType] = Lt.leftTypeResolution
      lazy val rightTypeResolution: List[Types.PType] = Lt.rightTypeResolution
    }

    case object Gt extends CompOp {
      lazy val leftTypeResolution: List[Types.PType] = Lt.leftTypeResolution
      lazy val rightTypeResolution: List[Types.PType] = Lt.rightTypeResolution
    }

    case object GtE extends CompOp {
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

  case class Ident(name: String)

  case class TypeIdent(name: String, typeParams: List[String])
}
