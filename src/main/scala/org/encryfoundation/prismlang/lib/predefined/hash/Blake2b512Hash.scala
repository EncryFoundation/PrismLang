package org.encryfoundation.prismlang.lib.predefined.hash

import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import scorex.crypto.hash.Blake2b512

object Blake2b512Hash extends HashFunctionHolder {

  val name: String = "blake2b512hash"

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = bodyValue(Blake2b512.hash)
}