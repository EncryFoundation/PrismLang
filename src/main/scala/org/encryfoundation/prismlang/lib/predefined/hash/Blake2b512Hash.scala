package org.encryfoundation.prismlang.lib.predefined.hash

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder
import scorex.crypto.hash.Blake2b512

object Blake2b512Hash extends BuiltInFunctionHolder {

  val name: String = "blake2b512hash"

  def asFunc: PFunctionPredef = PFunctionPredef(args, body)

  val args = IndexedSeq("input" -> Types.PCollection.ofByte)

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = (args: Seq[(String, PValue)]) => {
    val validNumberOfArgs = args.size == 1
    val validArgTypes = args.forall { case (_, v) => v.tpe == Types.PCollection.ofByte }
    if (validNumberOfArgs && validArgTypes) {
      val fnArgs = args.map(_._2.value.asInstanceOf[Array[Byte]])
      Right(Blake2b512.hash(fnArgs.head).toList)
    } else {
      Left(PredefFunctionExecFailure)
    }
  }
}