package org.encryfoundation.prismlang.lib.predefined.hash

import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import scorex.crypto.hash.Blake2b256

object Blake2b256Hash extends HashFunctionHolder {

  val name: String = "blake2b256"

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = bodyValue(Blake2b256.hash)
}
