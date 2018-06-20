package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast.Ident
import org.encryfoundation.prismlang.core.Types._
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class PCompilerSpec extends PropSpec with Matchers {

  property("Module compilation") {
    val source: String =
      """
        |struct CustomerBox:Object(
        |  person:Object(name:String; age:Int);
        |  orders:Array[Object(product_id:Int; amount:Int;)];
        |  id:Int;
        |)
        |
        |contract (signature: Signature25519, tx: Transaction) = {
        |  let ownerPubKey = base58"GtBn7qJwK1v1EbB6CZdgmkcvt849VKVfWoJBMEWsvTew"
        |  checkSig(tx.messageToSign, ownerPubKey, signature)
        |}
      """.stripMargin

    val expected = CompiledContract(
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
              Name(Ident("signature"),Signature25519)
            ),
            PBoolean
          )
        ),
        PBoolean
      )
    )

    val compiledTry: Try[CompiledContract] = PCompiler.compile(source)

    compiledTry.isSuccess shouldBe true

    compiledTry.get.toString shouldEqual expected.toString
  }
}
