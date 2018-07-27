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

  property("Large module compilation") {
    val source: String =
      """
        |contract (signature: Signature25519, transaction: Transaction, self: AssetBox) = {
        |  let exchangeRate = 2
        |  let lessBase = true
        |  let acceptableTokenId    = base58'5QCPz4eZAgT8DLAoZDSeouLMk1Kcf6DjJzrURiSV9U9'
        |  let intrinsicTokenId     = base58'11NDaGfSWVg9qjjPc4QjGYJL8ErvGRrmKGEW5FSMq3i'
        |  let requiredContractHash = base58'75Gs7HHUNnoEzsPgRRVABzQaC3UZVcayw9NY457Kx5p'
        |
        |  let recallPubKey         = base58'9ch4H7KMZqfPCAvqDqwZBY8dF7WkCfzj3voyS3UW9CqW'
        |
        |  def isReturnBox(bx: Box): Bool = {
        |    if (let assetBx: AssetBox = bx) {
        |      assetBx.tokenId == acceptableTokenId &&
        |      assetBx.contractHash == requiredContractHash
        |    } else false
        |  }
        |
        |  (if (let returnBox: AssetBox = transaction.outputs.filter(isReturnBox)[0]) {
        |    transaction.outputs.exists(lamb (bx: Box) = if (let assetBx: AssetBox = bx) {
        |      assetBx.tokenId == intrinsicTokenId &&
        |      assetBx.contractHash == requiredContractHash &&
        |      ((self.amount - returnBox.amount) / exchangeRate) >= assetBx.amount
        |    } else false)
        |  } else false) || checkSig(signature, transaction.messageToSign, recallPubKey)
        |}
      """.stripMargin

    val compiledTry: Try[CompiledContract] = PCompiler.compile(source)

    compiledTry.isSuccess shouldBe true
  }
}
