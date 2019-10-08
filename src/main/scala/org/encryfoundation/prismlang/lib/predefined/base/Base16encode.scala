package org.encryfoundation.prismlang.lib.predefined.base

import scorex.crypto.encode.Base16

object Base16encode extends BaseFunctionHolder {

  override val name: String = "encode16"

  override val baseFunc = Base16.encode

}
