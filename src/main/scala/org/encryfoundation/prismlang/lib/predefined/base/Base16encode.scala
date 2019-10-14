package org.encryfoundation.prismlang.lib.predefined.base

import scorex.crypto.encode.Base16

object Base16encode extends BaseFunctionHolder {

  val name: String = "encode16"

  val baseFunc = Base16.encode

}
