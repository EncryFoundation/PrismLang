package org.encryfoundation.prismlang.lib.predefined.hash

import scorex.crypto.hash.{Digest32, Sha256}

object Sha256Hash extends HashFunctionHolder {

  val name: String = "sha256"

  val hashFunc: Array[Byte] => Digest32 = Sha256.hash

}