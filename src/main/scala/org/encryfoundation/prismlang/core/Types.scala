package org.encryfoundation.prismlang.core

import org.encryfoundation.prismlang.codec.PCodec
import org.encryfoundation.prismlang.core.wrapped.{PFunction, PObject}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

object Types {

  type Field = (String, PType)

  /** Base trait each type-class inherits. */
  sealed trait PType {
    type Underlying
    val ident: String
    val dataCost: Int

    val isPrimitive: Boolean = false
    val isCollection: Boolean = false
    val isTuple: Boolean = false
    val isOption: Boolean = false
    val isProduct: Boolean = false
    val isFunc: Boolean = false
    val isNumeric: Boolean = false
    val isTag: Boolean = false
    val isNit: Boolean = false

    def isSubtypeOf(thatT: PType): Boolean = thatT match {
      case PAny => true
      case _ => false
    }

    def canBeDerivedTo(thatT: PType): Boolean = false

    def deriveValue(v: Underlying, thatT: PType): Option[thatT.Underlying] = None

    def isApplicable(func: Types.PFunc): Boolean = false

    override def equals(obj: scala.Any): Boolean = obj match {
      case prim: Primitive => prim.ident == this.ident
      case prod: Product => prod == this
      case tag: TaggedType => tag.underlyingType == this
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
    override val dataCost: Int = 100
  }
  case object PUnit extends PType with Primitive {
    override type Underlying = Unit
    override val ident: String = "Unit"
    override val dataCost: Int = 0
  }
  case object PBoolean extends PType with Primitive {
    override type Underlying = Boolean
    override val ident: String = "Bool"
    override val dataCost: Int = 1
  }
  case object PInt extends PType with Primitive {
    override type Underlying = Int
    override val ident: String = "Int"
    override val isNumeric: Boolean = true
    override val dataCost: Int = 8
  }
  case object PByte extends PType with Primitive {
    override type Underlying = Byte
    override val ident: String = "Byte"
    override val dataCost: Int = 4

    override def canBeDerivedTo(thatT: PType): Boolean = thatT match {
      case PInt => true
      case _ => false
    }

    override def deriveValue(v: Byte, thatT: PType): Option[thatT.Underlying] = thatT match {
      case PInt => Some(v.toLong.asInstanceOf[thatT.Underlying])
      case _ => None
    }
  }
  case object PString extends PType with Primitive {
    override type Underlying = String
    override val ident: String = "String"
    override val dataCost: Int = 5
  }

  /** Trait each type that have type-parameters inherits. */
  sealed trait Parametrized extends PType

  case class PCollection(valT: PType) extends PType with Parametrized {
    override type Underlying = List[valT.Underlying]
    override val ident: String = "Array"
    override val isCollection: Boolean = true
    override val dataCost: Int = 10 * valT.dataCost

    override def isApplicable(func: Types.PFunc): Boolean =
      func.args.size == 1 && (valT.isSubtypeOf(func.args.head._2) || valT == func.args.head._2)

    override def equals(obj: Any): Boolean = obj match {
      case coll: PCollection =>
        coll.valT == this.valT || coll.valT.isSubtypeOf(this.valT) || coll.valT.canBeDerivedTo(this.valT)
      case tag: TaggedType if tag.isCollection => tag.underlyingType == this
      case _ => false
    }
  }
  object PCollection {
    val ofByte: PCollection = PCollection(PByte)
    val ofByteArrays: PCollection = PCollection(PCollection(PByte))
    val ofInt: PCollection = PCollection(PInt)
    val ofBool: PCollection = PCollection(PBoolean)
    val ofString: PCollection = PCollection(PString)
  }

  case class PSet(valT : PType) extends PType with Parametrized {
    override type Underlying = Set[valT.Underlying]
    override val ident: String = "Set"
    override val isCollection: Boolean = true
    override val dataCost: Int = 10 * valT.dataCost

    override def isApplicable(func: PFunc): Boolean =
      func.args.size == 1 && (valT.isSubtypeOf(func.args.head._2) || valT == func.args.head._2)

    override def equals(obj: Any): Boolean = obj match {
      case set: PSet =>
        set.valT == this.valT || set.valT.isSubtypeOf(this.valT) || set.valT.canBeDerivedTo(this.valT)
      case tag: TaggedType if tag.isCollection => tag.underlyingType == this
      case _ => false
    }
  }
  object PSet {
    val ofByte: PSet = PSet(PByte)
    val ofByteArrays: PSet = PSet(PSet(PByte))
    val ofInt: PSet = PSet(PInt)
    val ofBool: PSet = PSet(PBoolean)
    val ofString: PSet = PSet(PString)
  }

  case class PFunc(args: List[(String, PType)], retT: PType) extends PType {
    override type Underlying = PFunction
    override val ident: String = "Func"
    override val isFunc: Boolean = true
    override val dataCost: Int = 20

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
    * All nodes with this type should be eliminated during typing.
    * If no specific type can be assigned statically during typing,
    * then either error should be raised or type PAny should be assigned */
  case object Nit extends PType {
    override type Underlying = Nothing
    override val isNit: Boolean = true
    override val ident: String = "Nit"
    override val dataCost: Int = 0
  }

  sealed trait TaggedType extends PType {
    val underlyingType: PType

    override val isTag: Boolean = true

    override def equals(obj: Any): Boolean = obj match {
      case tag: TaggedType => tag.underlyingType == this.underlyingType
      case otherT: PType => otherT == underlyingType
      case _ => false
    }
  }

  /** User-defined data structure tag */
  case class StructTag(ident: String, underlyingType: PType) extends PType with TaggedType {
    override type Underlying = underlyingType.Underlying
    override val dataCost: Int = 10

    override val isNumeric: Boolean = underlyingType.isNumeric
    override val isCollection: Boolean = underlyingType.isCollection
    override val isProduct: Boolean = underlyingType.isProduct
    override val isOption: Boolean = underlyingType.isOption

    def fingerprint: String = Base58.encode(
      Blake2b256.hash(PCodec.typeCodec.encode(underlyingType).require.toByteArray).sliding(1, 8).reduce(_ ++ _)
    )
  }

  case object Signature25519 extends PType with TaggedType {
    override type Underlying = underlyingType.Underlying
    override val ident: String = "Signature25519"
    override val underlyingType: PType = PCollection.ofByte
    override val dataCost: Int = 15

    override val isCollection: Boolean = true
  }

  case object MultiSig extends PType with TaggedType {
    override type Underlying = underlyingType.Underlying
    override val ident: String = "MultiSig"
    override val underlyingType: PType = PCollection.ofByteArrays
    override val dataCost: Int = 35

    override val isCollection: Boolean = true
  }

  /** Base trait all complex type-classes inherit
    * (types which have properties). */
  sealed trait Product extends PType {
    override val isProduct: Boolean = true

    val ancestor: Option[Product] = None
    def fields: Map[String, PType] = ancestor.map(_.fields).getOrElse(Map.empty)

    def getAttrType(n: String): Option[PType] = fields.get(n)
      .orElse(ancestor.flatMap(_.getAttrType(n)))

    override def isSubtypeOf(thatT: PType): Boolean =
      ancestor.contains(thatT) || ancestor.exists(_.isSubtypeOf(thatT))

    override def equals(obj: Any): Boolean = obj match {
      case p: Product =>
        if (p.fields.size != this.fields.size) false
        else p.fields.zip(this.fields).forall { case ((f1, _), (f2, _)) => f1 == f2 }
      case _ => false
    }
  }

  case class PTuple(eltsT: List[PType]) extends PType with Product with Parametrized {
    override type Underlying = List[Any]
    override val ident: String = s"Tuple${eltsT.size}"
    override val dataCost: Int = 10

    override val isTuple: Boolean = true

    override def fields: Map[String, PType] = (1 to eltsT.size).map(i => s"_$i" -> eltsT(i-1)).toMap

    override def equals(obj: Any): Boolean = obj match {
      case tuple: PTuple if tuple.eltsT.size == this.eltsT.size =>
        tuple.eltsT.zip(this.eltsT).forall { case (thatT, thisT) => thatT == thisT }
      case _ => false
    }
  }

  /** Used to describe user-defined arbitrary product type */
  case class ArbitraryProduct(override val ident: String, props: List[(String, PType)]) extends PType with Product {
    override type Underlying = PObject
    override val fields: Map[String, PType] = props.toMap
    override val dataCost: Int = 100

    def fingerprint: String = Base58.encode(
      Blake2b256.hash(PCodec.typeCodec.encode(this).require.toByteArray).sliding(1, 8).reduce(_ ++ _)
    )
  }

  // Abstract type
  case object EncryBox extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "Box"
    override val dataCost: Int = 10

    override val fields: Map[String, PType] = Map(
      "contractHash" -> PCollection.ofByte,
      "typeId" -> PByte,
      "id" -> PCollection.ofByte
    )
  }

  // ESBox impl
  case object AssetBox extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "AssetBox"
    override val dataCost: Int = 10

    override val ancestor: Option[Product] = Some(EncryBox)

    override val fields: Map[String, PType] = Map(
      "amount" -> PInt,
      "tokenId" -> PCollection.ofByte
    )
  }

  // ESBox impl
  case object AssetIssuingBox extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "AssetIssuingBox"
    override val dataCost: Int = 10

    override val ancestor: Option[Product] = Some(EncryBox)

    override val fields: Map[String, PType] = Map(
      "amount" -> PInt
    )
  }

  // ESBox impl
  case object DataBox extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "DataBox"
    override val dataCost: Int = 10

    override val ancestor: Option[Product] = Some(EncryBox)

    override val fields: Map[String, PType] = Map(
      "data" -> PCollection.ofByte
    )
  }

  case object EncryTransaction extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "Transaction"
    override val dataCost: Int = 20

    override val fields: Map[String, PType] = Map(
      "inputs" -> PCollection(PCollection.ofByte),
      "outputs" -> PCollection(EncryBox),
      "messageToSign" -> PCollection.ofByte
    )
  }

  case object EncryState extends PType with Product {
    override type Underlying = PObject
    override val ident: String = "State"
    override val dataCost: Int = 30

    override val fields: Map[String, PType] = Map(
      "height" -> PInt,
      "lastBlockTimestamp" -> PInt,
      "stateDigest" -> PCollection.ofByte
    )
  }

  val primitiveTypes: List[Primitive] = List(
    PAny,
    PUnit,
    PBoolean,
    PInt,
    PByte,
    PString
  )

  /** All types with type parameters including `PTuple` instances
    * of all possible dimensions. */
  val parametrizedTypes: List[Parametrized] = List(
    PCollection(Nit), PSet(Nit)
  ) ++ (1 to Constants.TupleMaxDim).map(i => PTuple((1 to i).map(_ => Nit).toList))

  val productTypes: List[Product] = List(
    EncryTransaction,
    EncryBox,
    EncryState,
    AssetBox,
    AssetIssuingBox,
    DataBox
  )

  val taggedTypes: List[TaggedType] = List(
    Signature25519,
    MultiSig
  )

  val numericTypes: List[PType] = List(
    PInt,
    PByte
  )

  /** All types except `Nit` and `PFunc` */
  val regularTypes: List[PType] = primitiveTypes ++ parametrizedTypes ++ productTypes ++ taggedTypes

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
