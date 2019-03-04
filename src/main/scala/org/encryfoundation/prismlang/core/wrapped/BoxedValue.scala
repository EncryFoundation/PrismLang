package org.encryfoundation.prismlang.core.wrapped

import org.encryfoundation.prismlang.core.Types

/** `PValue` presets for most commonly used data types.
  * Can be automatically serialized by scodec. */
sealed trait BoxedValue extends PValue

object BoxedValue {

  case class IntValue(value: Long) extends BoxedValue { override val tpe: Types.PType = Types.PInt }
  case class ByteValue(value: Byte) extends BoxedValue { override val tpe: Types.PType = Types.PByte }
  case class BoolValue(value: Boolean) extends BoxedValue { override val tpe: Types.PType = Types.PBoolean }
  case class StringValue(value: String) extends BoxedValue { override val tpe: Types.PType = Types.PString }
  case class ByteCollectionValue(value: List[Byte]) extends BoxedValue { override val tpe: Types.PType = Types.PCollection.ofByte }
  case class Signature25519Value(value: List[Byte]) extends BoxedValue { override val tpe: Types.PType = Types.Signature25519 }
  case class MultiSignatureValue(value: List[List[Byte]]) extends BoxedValue { override val tpe: Types.PType = Types.PCollection.ofByteArrays }
}
