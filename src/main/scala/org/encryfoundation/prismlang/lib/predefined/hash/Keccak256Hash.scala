package org.encryfoundation.prismlang.lib.predefined.hash

import scorex.crypto.hash.{Digest32, Keccak256}

object Keccak256Hash extends HashFunctionHolder {

  override val name = "keccak256"

  override val hashFunc: Keccak256.Message => Digest32 = Keccak256.hash

}