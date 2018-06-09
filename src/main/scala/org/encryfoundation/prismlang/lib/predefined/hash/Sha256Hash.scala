package org.encryfoundation.prismlang.lib.predefined.hash

import scorex.crypto.hash.{Sha256 => ScorexSha256}

object Sha256Hash extends HashFunctionHolder {

  val name: String = "sha256hash"

  val body = bodyValue(ScorexSha256.hash)
}