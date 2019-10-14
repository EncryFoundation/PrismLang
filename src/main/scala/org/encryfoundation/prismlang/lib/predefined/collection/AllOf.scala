package org.encryfoundation.prismlang.lib.predefined.collection

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder

object AllOf extends BuiltInFunctionHolder {

  val name: String = "allOf"

  val cost: Int = 10

  val args: IndexedSeq[(String, Types.PCollection)] = IndexedSeq("coll" -> Types.PCollection.ofBool)

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Boolean] = (pArgs: Seq[(String, PValue)]) => {
    if(checkArgs(args, pArgs))
      Right(pArgs.head._2.value.asInstanceOf[List[Boolean]].forall(i => i))
    else
      Left(PredefFunctionExecFailure)
  }
}
