package org.encryfoundation.prismlang.core.wrapped

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.EvalResult

case class PFunctionPredef(args: IndexedSeq[(String, Types.PType)],
                           body: (Seq[(String, PValue)]) => EvalResult) extends PWrappedMember

object PFunctionPredef {

  case object PredefFunctionExecFailure

  type EvalResult = Either[PredefFunctionExecFailure.type, Any] // Use either `PValue` or `Any`?
}
