package org.encryfoundation.prismlang.lib.predefined.hash

import scorex.crypto.hash.Blake2b256

object Blake2b256Hash extends BuiltInHashHolder {

  val name: String = "blake2b256hash"

  val body = bodyValue(Blake2b256.hash)
}
