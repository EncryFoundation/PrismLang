package org.encryfoundation.prismlang.lib.predefined.base

import scorex.crypto.encode.Base58

object Base58encode extends BaseFunctionHolder {

  val name: String = "encode58"

  val baseFunc = Base58.encode

}
