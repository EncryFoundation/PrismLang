package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast.Ident
import org.encryfoundation.prismlang.core.Types._
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class CompiledContractSerializerSpec extends PropSpec with Matchers {

  property("toBytes/parseBytes") {

    val contract: CompiledContract = CompiledContract(
      List(
        ("signature", Signature25519),
        ("tx", EncryTransaction)
      ),
      Block(
        List(
          Let(
            Ident("ownerPubKey"),
            Base58Str("GtBn7qJwK1v1EbB6CZdgmkcvt849VKVfWoJBMEWsvTew"),
            None
          ),
          Call(
            Name(
              Ident("checkSig"),
              PFunc(
                List(
                  ("sig", PCollection(PByte)),
                  ("msg", PCollection(PByte)),
                  ("pubKey", PCollection(PByte))
                ),
                PBoolean)
            ),
            List(
              Attribute(
                Name(Ident("tx"),EncryTransaction),
                Ident("messageToSign"),
                PCollection(PByte)
              ),
              Name(Ident("ownerPubKey"),PCollection(PByte)),
              Attribute(
                Name(Ident("signature"),Signature25519),
                Ident("sigBytes"),
                PCollection(PByte)
              )
            ),
            PBoolean
          )
        ),
        PBoolean
      )
    )

    val contactSerialized: Array[Byte] = contract.bytes

    val contactDeserializedTry: Try[CompiledContract] = CompiledContractSerializer.parseBytes(contactSerialized)

    contactDeserializedTry.isSuccess shouldBe true

    contactDeserializedTry.get.toString shouldEqual contract.toString
  }
}
