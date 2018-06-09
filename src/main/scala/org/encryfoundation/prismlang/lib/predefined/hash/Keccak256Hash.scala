package org.encryfoundation.prismlang.lib.predefined.hash

import scorex.crypto.hash.Keccak256

object Keccak256Hash extends BuiltInHashHolder {

  val name = "keccak256hash"

  val body = bodyValue(Keccak256.hash)
}