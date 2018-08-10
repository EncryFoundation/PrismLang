package org.encryfoundation.utils.encoding

import org.scalatest.{Matchers, PropSpec}
import scorex.utils.Random

class Base58CheckSpec extends PropSpec with Matchers {

  private val iData = Random.randomBytes()

  property("encode/decode") {

    val dEncoded = Base58Check.encode(iData)

    val dDecoded = Base58Check.decode(dEncoded)

    dDecoded.isSuccess shouldBe true

    iData sameElements dDecoded.get shouldBe true
  }
}
