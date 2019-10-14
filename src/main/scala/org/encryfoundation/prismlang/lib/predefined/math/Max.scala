package org.encryfoundation.prismlang.lib.predefined.math

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder

object Max extends BuiltInFunctionHolder {

  val name = "max"
  val cost: Int = 20

  val args: IndexedSeq[(String, Types.PInt.type)] = IndexedSeq("a" -> Types.PInt, "b" -> Types.PInt)

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Long] = (pArgs: Seq[(String, PValue)]) => {
    if(checkArgs(args, pArgs)) {
      val leftOperand: Long = pArgs.head._2.value.asInstanceOf[Long]
      val rightOperand: Long = pArgs.last._2.value.asInstanceOf[Long]
      Right(Math.max(leftOperand, rightOperand))
    } else
      Left(PredefFunctionExecFailure)
  }
}
