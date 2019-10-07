package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.ValueGenerator.generateRandomString
import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast.TypeDescriptor.{ProductType, SimpleType}
import org.encryfoundation.prismlang.core.Ast.{Expr, Ident, Struct, TypeIdent}
import org.encryfoundation.prismlang.core.Types._
import org.scalatest.{Matchers, PropSpec}

import scala.util.{Random, Try}

class StructSpec extends PropSpec with Matchers {

  property("struct check") {

    def fieldToStr(name: String, ident: String) = s"$name:$ident"

    val types = List(/*PString.ident, */PInt.ident)

    val fieldsStr = types.map { t => fieldToStr(generateRandomString(Random.nextInt(10)+1), t)}.mkString(";")

    val source: String =
      s"""
        |struct CustomerBox:Object(i: Int)
        |
        |contract (signature: Signature25519, tx: Transaction) = {
        |  let customerBox = CustomerBox(1)
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
              Ident("CustomerBox"),
              ProductType(
                List(
                  (Ident("i"), TypeIdent("Int", List()))
                ))),
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
