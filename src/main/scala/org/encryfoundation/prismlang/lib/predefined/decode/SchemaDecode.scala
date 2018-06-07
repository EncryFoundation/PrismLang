package org.encryfoundation.prismlang.lib.predefined.decode

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PObject, PValue}
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder

import scala.util.Try

/** EncryTL serialized data decoding. */
object SchemaDecode extends BuiltInFunctionHolder {

  val name: String = "read"

  override def asFunc: PFunctionPredef = PFunctionPredef(name, args, body)

  val args = IndexedSeq("data" -> Types.PCollection.ofByte)

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = (args: Seq[(String, PValue)]) => {
    val validNumberOfArgs = args.size == 1
    val validArgTypes = args.forall { case (_, v) => v.tpe == Types.PCollection.ofByte }
    if (validNumberOfArgs && validArgTypes) {
      val fnArg = args.map(_._2.value.asInstanceOf[Array[Byte]]).head
      Right(decodeData(fnArg).toOption)
    } else {
      Left(PredefFunctionExecFailure)
    }
  }

  private def decodeData(data: Array[Byte]): Try[PObject] = ???
}
