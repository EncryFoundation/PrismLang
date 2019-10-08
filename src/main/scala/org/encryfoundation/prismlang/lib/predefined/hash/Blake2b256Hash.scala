package org.encryfoundation.prismlang.lib.predefined.hash

import scorex.crypto.hash.{Blake2b256, Digest32}

object Blake2b256Hash extends HashFunctionHolder {

  override val name: String = "blake2b256"

  override val hashFunc: Blake2b256.Message => Digest32 = Blake2b256.hash

}
