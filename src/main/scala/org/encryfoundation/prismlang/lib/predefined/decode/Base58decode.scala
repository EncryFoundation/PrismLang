package org.encryfoundation.prismlang.lib.predefined.decode

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder
import scorex.crypto.encode.Base58

object Base58decode extends BuiltInFunctionHolder {

  val name: String = "decode"

  def asFunc: PFunctionPredef = PFunctionPredef(name, args, body)

  val args = IndexedSeq("s" -> Types.PString)

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = (args: Seq[(String, PValue)]) => {
    val validNumberOfArgs = args.size == 1
    val validArgTypes = args.forall { case (_, v) => v.tpe == Types.PString }
    if (validNumberOfArgs && validArgTypes) {
      val fnArg = args.map(_._2.value.asInstanceOf[String]).head
      Right(Base58.decode(fnArg).map(_.toList).toOption)
    } else {
      Left(PredefFunctionExecFailure)
    }
  }
}
