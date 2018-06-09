package org.encryfoundation.prismlang.lib.predefined.hash

import scorex.crypto.hash.Keccak512

object Keccak512Hash extends HashFunctionHolder {

  val name = "keccak512hash"

  val body = bodyValue(Keccak512.hash)
}