package org.encryfoundation.prismlang.lib.predefined.hash

import scorex.crypto.hash.{Digest64, Keccak512}

object Keccak512Hash extends HashFunctionHolder {

  override val name = "keccak512"

  override val hashFunc: Keccak512.Message => Digest64 = Keccak512.hash

}