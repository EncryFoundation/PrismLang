package org.encryfoundation.prismlang.lib.predefined.math

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder

object Min extends BuiltInFunctionHolder {

  val name = "min"

  def asFunc: PFunctionPredef = PFunctionPredef(args, body)

  val args = IndexedSeq("a" -> Types.PInt, "b" -> Types.PInt)

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = (varargs: Seq[(String, PValue)]) => {
    val validNumberOfArgs: Boolean = varargs.size == args.size
    val validArgTypes: Boolean = varargs.zip(args).forall { case ((_, value), (_, tpe)) => value.tpe == tpe }
    if (validNumberOfArgs && validArgTypes) {
      val leftOperand: Long = varargs.head._2.value.asInstanceOf[Long]
      val rightOperand: Long = varargs.last._2.value.asInstanceOf[Long]
      Right(Math.min(leftOperand, rightOperand))
    } else {
      Left(PredefFunctionExecFailure)
    }
  }
}
