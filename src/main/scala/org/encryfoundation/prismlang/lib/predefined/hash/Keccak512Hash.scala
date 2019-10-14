package org.encryfoundation.prismlang.lib.predefined.hash

import scorex.crypto.hash.{Digest64, Keccak512}

object Keccak512Hash extends HashFunctionHolder {

  val name = "keccak512"

  val hashFunc: Keccak512.Message => Digest64 = Keccak512.hash

}