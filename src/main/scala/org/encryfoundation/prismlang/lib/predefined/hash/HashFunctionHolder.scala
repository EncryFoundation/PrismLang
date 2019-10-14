package org.encryfoundation.prismlang.lib.predefined.hash

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder
import scorex.crypto.hash.Digest

trait HashFunctionHolder extends BuiltInFunctionHolder {

  val cost: Int = 25
  val hashFunc: Array[Byte] => Digest

  val args: IndexedSeq[(String, Types.PCollection)] = IndexedSeq("input" -> Types.PCollection.ofByte)

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, List[Byte]] = (pArgs: Seq[(String, PValue)]) => {
    if(checkArgs(args, pArgs))
      Right(hashFunc(pArgs.map(_._2.value.asInstanceOf[List[Byte]].toArray).head).toList)
    else
      Left(PredefFunctionExecFailure)
  }
}
