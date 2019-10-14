package org.encryfoundation.prismlang.lib.predefined.hash

import scorex.crypto.hash.{Blake2b256, Digest32}

object Blake2b256Hash extends HashFunctionHolder {

  val name: String = "blake2b256"

  val hashFunc: Blake2b256.Message => Digest32 = Blake2b256.hash

}
