package org.encryfoundation.prismlang.lib.predefined.collection

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder

object AnyOf extends BuiltInFunctionHolder {

  val name = "anyOf"

  val cost: Int = 10

  def asFunc: PFunctionPredef = PFunctionPredef(args, body)

  val args = IndexedSeq("coll" -> Types.PCollection.ofBool)

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = (varargs: Seq[(String, PValue)]) => {
    val validNumberOfArgs: Boolean = varargs.size == args.size
    val validArgTypes: Boolean = varargs.zip(args).forall { case ((_, value), (_, tpe)) => value.tpe == tpe }
    if (validNumberOfArgs && validArgTypes) Right(varargs.head._2.value.asInstanceOf[List[Boolean]].contains(true))
    else Left(PredefFunctionExecFailure)
  }
}
