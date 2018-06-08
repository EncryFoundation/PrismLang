package org.encryfoundation.prismlang.lib.predefined.time

import java.text.SimpleDateFormat

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder

object Str2Time extends BuiltInFunctionHolder {

  val name = "unixTime"

  def asFunc: PFunctionPredef = PFunctionPredef(args, body)

  val args = IndexedSeq("input" -> Types.PString)

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = (args: Seq[(String, PValue)]) => {
    val validNumberOfArgs = args.size == 1
    val validArgTypes = args.forall { case (_, v) => v.tpe == Types.PString }
    if (validNumberOfArgs && validArgTypes) {
      val fnArgs = args.map(_._2.value.asInstanceOf[String])
      val dateFormat = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
      Right(dateFormat.parse(fnArgs.head).getTime)
    } else {
      Left(PredefFunctionExecFailure)
    }
  }
}
