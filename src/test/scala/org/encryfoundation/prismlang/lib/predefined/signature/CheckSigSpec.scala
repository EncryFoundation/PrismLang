package org.encryfoundation.prismlang.lib.predefined.signature

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.{PFunctionPredef, PValue}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.utils.Random

class CheckSigSpec extends PropSpec with Matchers {

  property("checkSig") {

    val msg: Array[Byte] = Random.randomBytes(100)

    val pair: (PrivateKey, PublicKey) = Curve25519.createKeyPair(Random.randomBytes())

    val sig: List[Byte] = Curve25519.sign(pair._1, msg).toList

    val res: Either[PFunctionPredef.PredefFunctionExecFailure.type, Any] = CheckSig.body(Seq(
      "sig"    -> PValue(sig, Types.PCollection.ofByte),
      "msg"    -> PValue(msg.toList, Types.PCollection.ofByte),
      "pubKey" -> PValue(pair._2.toList, Types.PCollection.ofByte)
    ))

    (res match {
      case Right(b: Boolean) => b
      case _ => false
    }) shouldBe true
  }
}
