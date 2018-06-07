package org.encryfoundation.prismlang.lib.predefined.hash

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder
import scorex.crypto.hash.Keccak512

object Keccak512Hash extends BuiltInFunctionHolder {

  val name = "keccak512hash"

  def asFunc: PFunctionPredef = PFunctionPredef(name, args, body)

  val args = IndexedSeq("input" -> Types.PCollection.ofByte)

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = (args: Seq[(String, PValue)]) => {
    val validNumberOfArgs = args.size == 1
    val validArgTypes = args.forall { case (_, v) => v.tpe == Types.PCollection.ofByte }
    if (validNumberOfArgs && validArgTypes) {
      val fnArgs = args.map(_._2.value.asInstanceOf[Array[Byte]])
      Right(Keccak512.hash(fnArgs.head).toList)
    } else {
      Left(PredefFunctionExecFailure)
    }
  }
}