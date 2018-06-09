package org.encryfoundation.prismlang.lib.predefined.hash

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef.PredefFunctionExecFailure
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.encryfoundation.prismlang.lib.predefined.BuiltInFunctionHolder
import scorex.crypto.hash.{Sha256 => ScorexSha256}

object Sha256Hash extends BuiltInHashHolder {

  val name: String = "sha256hash"

  val body = bodyValue(ScorexSha256.hash)
}