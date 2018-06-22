package org.encryfoundation.prismlang.lib.predefined.signature

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

object CheckSig extends BuiltInFunctionHolder {

  val name: String = "checkSig"

  val cost: Int = 50

  def asFunc: PFunctionPredef = PFunctionPredef(args, body)

  val args: IndexedSeq[(String, Types.PCollection)] = IndexedSeq("sig" -> Types.PCollection.ofByte, "msg" -> Types.PCollection.ofByte, "pubKey" -> Types.PCollection.ofByte)

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = (args: Seq[(String, PValue)]) => {
    val validNumberOfArgs: Boolean = args.size == 3
    val validArgTypes: Boolean = args.forall { case (_, v) => v.tpe == Types.PCollection.ofByte }
    if (validNumberOfArgs && validArgTypes) {
      val fnArgs: Seq[Array[Byte]] = args.map(_._2.value.asInstanceOf[List[Byte]].toArray)
      Right(Curve25519.verify(Signature @@ fnArgs.head, fnArgs(1), PublicKey @@ fnArgs.last))
    } else Left(PredefFunctionExecFailure)
  }
}
