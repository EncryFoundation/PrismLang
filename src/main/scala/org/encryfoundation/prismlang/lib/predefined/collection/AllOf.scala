package org.encryfoundation.prismlang.lib.predefined.collection

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder

object AllOf extends BuiltInFunctionHolder {

  val name: String = "allOf"

  val cost: Int = 10

  def asFunc: PFunctionPredef = PFunctionPredef(args, body)

  val args: IndexedSeq[(String, Types.PCollection)] = IndexedSeq("coll" -> Types.PCollection.ofBool)

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = (varargs: Seq[(String, PValue)]) => {
    val validNumberOfArgs: Boolean = varargs.size == args.size
    val validArgTypes: Boolean = varargs.zip(args).forall { case ((_, value), (_, tpe)) => value.tpe == tpe }
    if (validNumberOfArgs && validArgTypes) Right(varargs.head._2.value.asInstanceOf[List[Boolean]].forall(i => i))
    else Left(PredefFunctionExecFailure)
  }
}
