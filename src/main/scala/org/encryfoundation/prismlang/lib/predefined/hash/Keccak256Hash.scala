package org.encryfoundation.prismlang.lib.predefined.hash

import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import scorex.crypto.hash.Keccak256

object Keccak256Hash extends HashFunctionHolder {

  val name = "keccak256"

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = bodyValue(Keccak256.hash)
}