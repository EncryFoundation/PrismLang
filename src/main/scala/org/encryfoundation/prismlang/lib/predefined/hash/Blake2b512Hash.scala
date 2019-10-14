package org.encryfoundation.prismlang.lib.predefined.hash

import scorex.crypto.hash.{Blake2b512, Digest64}

object Blake2b512Hash extends HashFunctionHolder {

  val name: String = "blake2b512"

  val hashFunc: Blake2b512.Message => Digest64 = Blake2b512.hash

}