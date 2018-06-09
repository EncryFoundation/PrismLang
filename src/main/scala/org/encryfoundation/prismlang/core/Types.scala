package org.encryfoundation.prismlang.core

import org.encryfoundation.prismlang.core.wrapped.{PFunction, PObject}

object Types {

  type Field = (String, PType)

  /** Base trait each type-class inherits. */
  sealed trait PType {
    type Underlying
    val ident: String

    val isPrimitive: Boolean = false
    val isCollection: Boolean = false
    val isTuple: Boolean = false
    val isOption: Boolean = false
    val isProduct: Boolean = false
    val isFunc: Boolean = false
    val isNumeric: Boolean = false
    val isNit: Boolean = false

    def isSubtypeOf(thatT: PType): Boolean = thatT match {
      case _: PAny.type => true
      case _ => false
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case s: Primitive => s.ident == this.ident
      case p: Product => p == this
      case _ => false
    }
  }

  sealed trait Primitive extends PType {
    override val isPrimitive = true
  }

  /** Each type inherits this one by default. */
  case object PAny extends PType with Primitive {
    override type Underlying = Any
    override val ident: String = "Any"
  }
  case object PUnit extends PType with Primitive {
    override type Underlying = Unit
    override val ident: String = "Unit"
  }
  case object PBoolean extends PType with Primitive {
    override type Underlying = Boolean
    override val ident: String = "Bool"
  }
  case object PInt extends PType with Primitive {
    override type Underlying = Int
    override val ident: String = "Int"
    override val isNumeric: Boolean = true
  }
  case object PByte extends PType with Primitive {
    override type Underlying = Byte
    override val ident: String = "Byte"
  }
  case object PString extends PType with Primitive {
    override type Underlying = String
    override val ident: String = "String"
  }

  /** Trait each type that have type-parameters inherits. */
  sealed trait Parametrized extends PType

  case class PCollection(valT: PType) extends PType with Parametrized {
    override type Underlying = List[valT.Underlying]
    override val ident: String = "Array"
    override val isCollection: Boolean = true

    def isApplicable(func: Types.PFunc): Boolean =
      func.args.size == 1 && (valT.isSubtypeOf(func.args.head._2) || valT == func.args.head._2)

    override def equals(obj: Any): Boolean = obj match {
      case coll: PCollection => coll.valT == this.valT
      case _ => false
    }
  }
  object PCollection {
    val ofByte: PCollection = PCollection(PByte)
    val ofInt: PCollection = PCollection(PInt)
    val ofBool: PCollection = PCollection(PBoolean)
    val ofString: PCollection = PCollection(PString)
  }

  case class POption(inT: PType) extends PType with Parametrized {
    override type Underlying = Option[inT.Underlying]
    override val ident: String = "Option"
    override val isOption: Boolean = true

    override def equals(obj: Any): Boolean = obj match {
      case option: POption => option.inT == this.inT
      case _ => false
    }
  }

  case class PFunc(args: List[(String, PType)], retT: PType) extends PType {
    override type Underlying = PFunction
    override val ident: String = "Func"
    override val isFunc: Boolean = true

    override def equals(obj: Any): Boolean = obj match {
      case f: PFunc =>
        (this.retT == f.retT || this.retT.isSubtypeOf(f.retT)) &&
          this.args.size == f.args.size &&
          this.args.zip(f.args).forall { case ((_, a1), (_, a2)) => a1 == a2 }
      case _ => false
    }
  }

  /** Special type to represent untyped values.
    * Interpreter raises an error when encounter a Value with this type.
    * All Value nodes with this type should be elimitanted during typing.
    * If no specific type can be assigned statically during typing,
    * then either error should be raised or type SAny should be assigned
    * which is interpreted as dynamic typing. */
  case object Nit extends PType {
    override type Underlying = Nothing
    override val isNit: Boolean = true
    override val ident: String = "Nit"
  }

  /** Base trait all complex type-classes inherit
    * (complex - means types which have properties). */
  sealed trait Product extends PType {
    override val isProduct: Boolean = true
    val superType: Product = BaseObject
    def fields: Map[String, PType] = superType.fields

    def getAttrType(n: String): Option[PType] = fields.get(n)
      .orElse(superType.getAttrType(n))

    override def isSubtypeOf(thatT: PType): Boolean =
      superType == thatT || superType.isSubtypeOf(thatT)

    override def equals(obj: Any): Boolean = obj match {
      case p: Product =>
        if (p.fields.size != this.fields.size) false
        else p.fields.zip(this.fields).forall { case ((f1, _), (f2, _)) => f1 == f2 }
      case _ => false
    }
  }

  case class PTuple(valT: PType, eltsQty: Int) extends PType with Product with Parametrized {
    override type Underlying = List[valT.Underlying]
    override val isTuple: Boolean = true
    override val ident: String = "Tuple"

    override def fields: Map[String, PType] = (1 to eltsQty).map(i => s"_$i" -> valT).toMap

    override def equals(obj: Any): Boolean = obj match {
      case tuple: PTuple => tuple.valT == this.valT
      case _ => false
    }
  }

  /** Abstract type each product type inherits */
  case object BaseObject extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "Object"
    override val fields: Map[String, PType] = Map.empty
  }

  /** Used to describe dynamic data structures */
  case class Struct(override val ident: String, props: List[(String, PType)]) extends PType with Product {
    override type Underlying = PObject
    override val fields: Map[String, PType] = props.toMap
  }

  case object EncryContract extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "Contract"

    override val fields: Map[String, PType] = Map(
      "fingerprint" -> PCollection.ofByte
    )
  }

  // Abstract type
  case object EncryProof extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "Proof"

    override val fields: Map[String, PType] = Map(
      "typeId" -> PInt
    )
  }

  // ESProof impl
  case object Signature25519 extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "Signature25519"

    override val superType: Product = EncryProof

    override val fields: Map[String, PType] = Map(
      "sigBytes" -> PCollection.ofByte
    )
  }

  // ESProof impl
  case object MultiSig extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "MultiSig"

    override val superType: Product = EncryProof

    override val fields: Map[String, PType] = Map(
      "proofs" -> PCollection(Signature25519)
    )
  }

  case object EncryProposition extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "Proposition"

    override val fields: Map[String, PType] = Map(
      "typeId" -> PInt,
      "fingerprint" -> PCollection.ofByte
    )
  }

  // Abstract type
  case object EncryBox extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "Box"

    override val fields: Map[String, PType] = Map(
      "proposition" -> EncryProposition,
      "typeId" -> PInt,
      "id" -> PCollection.ofByte
    )
  }

  // ESBox impl
  case object AssetBox extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "AssetBox"

    override val superType: Product = EncryBox

    override val fields: Map[String, PType] = Map(
      "amount" -> PInt,
      "tokenIdOpt" -> POption(PCollection.ofByte)
    )
  }

  // ESBox impl
  case object AssetIssuingBox extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "AssetIssuingBox"

    override val superType: Product = EncryBox

    override val fields: Map[String, PType] = Map(
      "amount" -> PInt
    )
  }

  // ESBox impl
  case object DataBox extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "DataBox"

    override val superType: Product = EncryBox

    override val fields: Map[String, PType] = Map(
      "data" -> PCollection.ofByte
    )
  }

  case object EncryUnlocker extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "Unlocker"

    override val fields: Map[String, PType] = Map(
      "boxId" -> EncryTransaction,
      "proofOpt" -> POption(EncryProof)
    )
  }

  case object EncryTransaction extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "Transaction"

    override val fields: Map[String, PType] = Map(
      "accountPubKey" -> PCollection.ofByte,
      "fee" -> PInt,
      "timestamp" -> PInt,
      "signature" -> PCollection.ofByte,
      "unlockers" -> PCollection(EncryUnlocker),
      "outputs" -> PCollection(EncryBox),
      "messageToSign" -> PCollection.ofByte
    )
  }

  case object EncryState extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "State"

    override val fields: Map[String, PType] = Map(
      "height" -> PInt,
      "lastBlockTimestamp" -> PInt,
      "stateDigest" -> PCollection.ofByte
    )
  }

  val primitiveTypes: Seq[Primitive] = Seq(
    PAny,
    PUnit,
    PBoolean,
    PInt,
    PByte,
    PString
  )

  val parametrizedTypes: Seq[Parametrized] = Seq(
    PCollection(Nit),
    PTuple(Nit, 0),
    POption(Nit)
  )

  val productTypes: Seq[Product] = Seq(
    PTuple(Nit, 0),
    EncryTransaction,
    EncryProof,
    EncryProposition,
    EncryBox,
    EncryState,
    Signature25519,
    MultiSig,
    AssetBox,
    AssetIssuingBox,
    DataBox
  )

  val numericTypes: Seq[PType] = Seq(
    PInt
  )

  def liftType(d: Any): PType = d match {
    case _: Int => PInt
    case _: Long => PInt
    case _: Boolean => PBoolean
    case _: String => PString
    case _: Array[Byte] => PCollection.ofByte
    case _: Array[Int] => PCollection.ofInt
    case _: Array[Boolean] => PCollection.ofBool
    case _: Array[String] => PCollection.ofString
  }
}

case class TypeSystem(externalTypes: Seq[Types.Struct]) {

  import Types._

  lazy val allTypes: Seq[PType] = primitiveTypes ++ productTypes ++ parametrizedTypes ++ externalTypes :+ PFunc(List.empty, Nit)

  lazy val typesMap: Map[String, PType] = allTypes.map(t => t.ident -> t).toMap

  def typeByIdent(id: String): Option[PType] = typesMap.get(id)
}

object TypeSystem {

  def default: TypeSystem = TypeSystem(Seq.empty)
}
