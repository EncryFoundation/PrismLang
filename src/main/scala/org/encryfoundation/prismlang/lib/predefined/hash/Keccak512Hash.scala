package org.encryfoundation.prismlang.lib.predefined.hash

import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import scorex.crypto.hash.Keccak512

object Keccak512Hash extends HashFunctionHolder {

  val name = "keccak512"

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = bodyValue(Keccak512.hash)
}