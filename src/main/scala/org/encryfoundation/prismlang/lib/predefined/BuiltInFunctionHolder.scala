package org.encryfoundation.prismlang.lib.predefined

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}

trait BuiltInFunctionHolder {
  val name: String
  val cost: Int
  def asFunc: PFunctionPredef

  def checkArgs(dArgs: Seq[(String, Types.PType)], pArgs: Seq[(String, PValue)]): Boolean = dArgs.zip(pArgs)
    .foldLeft(true) { case (acc, ((_, tpe), (_, v))) => acc && tpe == v.tpe } && dArgs.lengthCompare(pArgs.size) == 0

  def runSafe(block: => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any]): Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] =
    try { block } catch { case _: Throwable => Left(PredefFunctionExecFailure) }
}
