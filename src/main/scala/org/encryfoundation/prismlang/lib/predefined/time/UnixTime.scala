package org.encryfoundation.prismlang.lib.predefined.time

import java.text.SimpleDateFormat

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder

object UnixTime extends BuiltInFunctionHolder {

  val name = "unixTime"
  val cost: Int = 2

  val args: IndexedSeq[(String, Types.PString.type)] = IndexedSeq("input" -> Types.PString)

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Long] = (pArgs: Seq[(String, PValue)]) => {
    if(checkArgs(args, pArgs)) {
      val fnArgs = pArgs.map(_._2.value.asInstanceOf[String])
      val dateFormat = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
      Right(dateFormat.parse(fnArgs.head).getTime)
    } else {
      Left(PredefFunctionExecFailure)
    }
  }
}
