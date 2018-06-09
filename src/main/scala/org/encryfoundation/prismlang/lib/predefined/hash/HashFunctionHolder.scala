package org.encryfoundation.prismlang.lib.predefined.hash

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder
import org.encryfoundation.prismlang.lib.predefined.hash.Sha256Hash.body

trait HashFunctionHolder extends BuiltInFunctionHolder {

  def asFunc: PFunctionPredef = PFunctionPredef(args, body)

  val args = IndexedSeq("input" -> Types.PCollection.ofByte)

  protected def bodyValue(hash: Array[Byte] => Array[Byte]): Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = (varargs: Seq[(String, PValue)]) => {
    val validNumberOfArgs = varargs.size == args.size
    val validArgTypes = varargs.zip(args).forall { case ((_, value), (_, tpe)) => value.tpe == tpe }
    if (validNumberOfArgs && validArgTypes) {
      val fnArgs = varargs.map(_._2.value.asInstanceOf[Array[Byte]])
      Right(hash(fnArgs.head).toList)
    } else {
      Left(PredefFunctionExecFailure)
    }
  }
}
