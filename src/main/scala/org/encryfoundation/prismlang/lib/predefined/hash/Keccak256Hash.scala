package org.encryfoundation.prismlang.lib.predefined.hash

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder
import scorex.crypto.hash.Keccak256

object Keccak256Hash extends BuiltInFunctionHolder {

  val name = "keccak256hash"

  def asFunc: PFunctionPredef = PFunctionPredef(name, args, body)

  val args = IndexedSeq("input" -> Types.PCollection.ofByte)

  private val body = (args: Seq[(String, PValue)]) => {
    val validNumberOfArgs = args.size == 1
    val validArgTypes = args.forall { case (_, v) => v.tpe == Types.PCollection.ofByte }
    if (validNumberOfArgs && validArgTypes) {
      val fnArgs = args.map(_._2.value.asInstanceOf[Array[Byte]])
      Right(Keccak256.hash(fnArgs.head).toList)
    } else {
      Left(PredefFunctionExecFailure)
    }
  }
}