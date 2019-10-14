package org.encryfoundation.prismlang.lib.predefined.hash

import scorex.crypto.hash.{Digest32, Keccak256}

object Keccak256Hash extends HashFunctionHolder {

  val name = "keccak256"

  val hashFunc: Keccak256.Message => Digest32 = Keccak256.hash

}