package org.encryfoundation.prismlang.lib.predefined.base

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder

trait BaseFunctionHolder extends BuiltInFunctionHolder {

  val cost: Int = 10

  def asFunc: PFunctionPredef = PFunctionPredef(args, body)

  val args: IndexedSeq[(String, Types.PType)] = IndexedSeq("input" -> Types.PCollection.ofByte)

  val baseFunc: Array[Byte] => String

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = (pArgs: Seq[(String, PValue)]) => {
    if(checkArgs(args, pArgs)) {
      val fnArgs = pArgs.map(_._2.value.asInstanceOf[List[Number]].map(_.byteValue()).toArray)
      Right(baseFunc(fnArgs.head))
    } else {
      Left(PredefFunctionExecFailure)
    }
  }
}
