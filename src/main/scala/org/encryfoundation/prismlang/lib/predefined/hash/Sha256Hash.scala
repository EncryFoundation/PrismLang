package org.encryfoundation.prismlang.lib.predefined.hash

import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import scorex.crypto.hash.{Sha256 => ScorexSha256}

object Sha256Hash extends HashFunctionHolder {

  val name: String = "sha256hash"

  val body: Seq[(String, PValue)] => Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = bodyValue(ScorexSha256.hash)
}