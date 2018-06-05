package org.encryfoundation.prismlang.core

object Types {

  type Field = (String, PType)

  sealed trait PType {
    type Underlying
    val ident: String

    def isPrimitive: Boolean = this.isInstanceOf[ESPrimitive]

    def isCollection: Boolean = this.isInstanceOf[ESCollection]

    def isOption: Boolean = this.isInstanceOf[POption]

    def isProduct: Boolean = this.isInstanceOf[Product]

    def isFunc: Boolean = this.isInstanceOf[PFunc]

    def isNit: Boolean = this.isInstanceOf[Nit.type]

    def isSubtypeOf(thatT: PType): Boolean = thatT match {
      case _: PAny.type => true
      case _ => false
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case s: ESPrimitive => s.ident == this.ident
      case p: Product => p == this
      case _ => false
    }
  }

  sealed trait ESPrimitive extends PType

  case object PUnit extends PType with ESPrimitive {
    override type Underlying = Unit
    override val ident: String = "Any"
  }
  case object PAny extends PType with ESPrimitive {
    override type Underlying = Any
    override val ident: String = "Any"
  }
  case object ESUnit extends PType with ESPrimitive {
    override type Underlying = Unit
    override val ident: String = "Unit"
  }
  case object PBoolean extends PType with ESPrimitive {
    override type Underlying = Boolean
    override val ident: String = "Bool"
  }
  case object PInt extends PType with ESPrimitive {
    override type Underlying = Int
    override val ident: String = "Int"
  }
  case object PString extends PType with ESPrimitive {
    override type Underlying = String
    override val ident: String = "String"
  }
  case object PByteVector extends PType with ESPrimitive {
    override type Underlying = Array[Byte]
    override val ident: String = "Bytes"
  }

  sealed trait Product extends PType {
    val superType: Product = PObject
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

  /** Abstract type each product type inherits */
  case object PObject extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "Object"
    override val fields: Map[String, PType] = Map.empty
  }

  /** Used to describe dynamic data structures */
  case class ESTypedObject(override val ident: String, fs: List[Field]) extends PType with Product {
    override type Underlying = Unit
    override val fields: Map[String, PType] = fs.toMap
  }

  /** TL schema object type tag */
  case object SDObject extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "SelfDescribingObject"
  }

  case object ESContext extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "Context"

    override val fields: Map[String, PType] = Map(
      "proof" -> ESProof,
      "transaction" -> ESTransaction,
      "state" -> ESState,
      "self" -> ESScript
    )
  }

  case object ESScript extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "Script"

    override val fields: Map[String, PType] = Map(
      "fingerprint" -> PByteVector
    )
  }

  // Abstract type
  case object ESProof extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "Proof"

    override val fields: Map[String, PType] = Map(
      "typeId" -> PInt
    )
  }

  // ESProof impl
  case object Signature25519 extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "Signature25519"

    override val superType: Product = ESProof

    override val fields: Map[String, PType] = Map(
      "sigBytes" -> PByteVector
    )
  }

  // ESProof impl
  case object MultiSig extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "MultiSig"

    override val superType: Product = ESProof

    override val fields: Map[String, PType] = Map(
      "proofs" -> PArray(Signature25519)
    )
  }

  // Abstract type
  case object ESProposition extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "Proposition"

    override val fields: Map[String, PType] = Map(
      "typeId" -> PInt
    )
  }

  // ESProposition impl
  case object AccountProposition extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "AccountProposition"

    override val superType: Product = ESProposition

    override val fields: Map[String, PType] = Map(
      "accountAddress" -> PString
    )
  }

  // ESProposition impl
  case object ContractProposition extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "ContractProposition"

    override val superType: Product = ESProposition

    override val fields: Map[String, PType] = Map(
      "fingerprint" -> PByteVector
    )
  }

  // ESProposition impl
  case object HeightProposition extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "HeightProposition"

    override val superType: Product = ESProposition

    override val fields: Map[String, PType] = Map(
      "height" -> PInt
    )
  }

  // ESProposition impl
  case object OpenProposition extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "OpenProposition"

    override val superType: Product = ESProposition
  }

  // Abstract type
  case object ESBox extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "Box"

    override val fields: Map[String, PType] = Map(
      "proposition" -> ESProposition,
      "typeId" -> PInt,
      "id" -> PByteVector
    )
  }

  // ESBox impl
  case object AssetBox extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "AssetBox"

    override val superType: Product = ESBox

    override val fields: Map[String, PType] = Map(
      "amount" -> PInt,
      "tokenIdOpt" -> POption(PByteVector)
    )
  }

  // ESBox impl
  case object AssetIssuingBox extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "AssetIssuingBox"

    override val superType: Product = ESBox

    override val fields: Map[String, PType] = Map(
      "amount" -> PInt
    )
  }

  // ESBox impl
  case object DataBox extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "DataBox"

    override val superType: Product = ESBox

    override val fields: Map[String, PType] = Map(
      "data" -> PByteVector
    )
  }

  case object ESUnlocker extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "Unlocker"

    override val fields: Map[String, PType] = Map(
      "boxId" -> ESTransaction,
      "proofOpt" -> POption(ESProof)
    )
  }

  case object ESTransaction extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "Transaction"

    override val fields: Map[String, PType] = Map(
      "accountPubKey" -> PByteVector,
      "fee" -> PInt,
      "timestamp" -> PInt,
      "signature" -> PByteVector,
      "unlockers" -> PArray(ESUnlocker),
      "outputs" -> PArray(ESBox),
      "messageToSign" -> PByteVector
    )
  }

  case object ESState extends PType with Product {
    override type Underlying = Unit
    override val ident: String = "State"

    override val fields: Map[String, PType] = Map(
      "height" -> PInt,
      "lastBlockTimestamp" -> PInt,
      "stateDigest" -> PByteVector
    )
  }

  sealed trait Parametrized

  sealed trait ESCollection extends Product with Parametrized {

    override def fields: Map[String, PType] = Map(
      "size" -> PInt
    )
  }

  case class PArray(valT: PType) extends PType with ESCollection {
    override type Underlying = List[valT.Underlying]
    override val ident: String = "List"

    override def fields: Map[String, PType] =
      if (numericTypes.contains(valT)) {
        super.fields ++ Map(
          "exists" -> PFunc(List("predicate" -> PFunc(List("any" -> valT), PBoolean)), PBoolean),
          "map" -> PFunc(List("fn" -> PFunc(List("any" -> valT), PAny)), PArray(PAny)),
          "sum" -> valT
        )
      } else {
        super.fields ++ Map(
          "exists" -> PFunc(List("predicate" -> PFunc(List("any" -> valT), PBoolean)), PBoolean),
          "map" -> PFunc(List("fn" -> PFunc(List("any" -> valT), PAny)), PArray(PAny))
        )
      }

    override def equals(obj: Any): Boolean = obj match {
      case l: PArray => l.valT == this.valT
      case _ => false
    }
  }

  case class ESDict(keyT: PType, valT: PType) extends PType with ESCollection {
    override type Underlying = Map[keyT.Underlying, valT.Underlying]
    override val ident: String = "Dict"

    override def fields: Map[String, PType] = super.fields ++ Map(
      "exists" -> PFunc(List("predicate" -> PFunc(List("any" -> keyT, "any" -> valT), PBoolean)), PBoolean),
      "map" -> PFunc(List("fn" -> PFunc(List("any" -> keyT, "any" -> valT), PAny)), PArray(PAny))
    )

    override def equals(obj: Any): Boolean = obj match {
      case d: ESDict => d.keyT == this.keyT && d.valT == this.valT
      case _ => false
    }
  }

  case class POption(inT: PType) extends PType with Product with Parametrized {
    override type Underlying = Option[inT.Underlying]
    override val ident: String = "Option"
    override val fields: Map[String, PType] = POption.fields ++ Map("get" -> inT)
  }
  object POption {
    val fields: Map[String, PType] = Map(
      "isDefined" -> PBoolean
    )
  }

  case class PFunc(args: List[(String, PType)], retT: PType) extends PType {
    override type Underlying = retT.Underlying
    override val ident: String = "Func"

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
    override val ident: String = "Nit"
  }

  lazy val primitiveTypes: Seq[ESPrimitive] = Seq(
    ESUnit,
    PBoolean,
    PInt,
    PString,
    PByteVector
  )

  lazy val productTypes: Seq[Product] = Seq(
    ESTransaction,
    ESProof,
    ESProposition,
    ESContext,
    ESBox,
    ESState,
    Signature25519,
    MultiSig,
    AssetBox,
    AssetIssuingBox,
    DataBox,
    AccountProposition,
    OpenProposition,
    ContractProposition,
    HeightProposition,
    SDObject,
    POption(Nit)
  )

  lazy val collTypes: Seq[ESCollection] = Seq(
    ESDict(Nit, Nit),
    PArray(Nit)
  )

  lazy val numericTypes: Seq[PType] = Seq(
    PInt
  )

  def liftType(d: Any): PType = d match {
    case _: Int => PInt
    case _: Long => PInt
    case _: Boolean => PBoolean
    case _: String => PString
    case _: Array[Byte] => PByteVector
  }
}

case class TypeSystem(externalTypes: Seq[Types.ESTypedObject]) {

  import Types._

  lazy val allTypes: Seq[PType] = primitiveTypes ++ productTypes ++ collTypes ++ externalTypes :+ PFunc(List.empty, Nit)

  lazy val typesMap: Map[String, PType] = allTypes.map(t => t.ident -> t).toMap

  def typeByIdent(id: String): Option[PType] = typesMap.get(id)
}

object TypeSystem {

  def default: TypeSystem = TypeSystem(Seq.empty)
}
