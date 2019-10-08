package org.encryfoundation.prismlang.lib.predefined.hash

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder
import scorex.crypto.hash.Digest

trait HashFunctionHolder extends BuiltInFunctionHolder {

  val cost: Int = 25

  def asFunc: PFunctionPredef = PFunctionPredef(args, body)

  val args: IndexedSeq[(String, Types.PType)] = IndexedSeq("input" -> Types.PCollection.ofByte)

  val hashFunc: Array[Byte] => Digest

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = (pArgs: Seq[(String, PValue)]) => {
    if(checkArgs(args, pArgs)) {
      val fnArgs = pArgs.map(_._2.value.asInstanceOf[List[Byte]].toArray)
      Right(hashFunc(fnArgs.head).toList)
    } else {
      Left(PredefFunctionExecFailure)
    }
  }
}
