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
        |  let rate = 2
        |  let lessBase = true
        |  let acceptableTokenId = Array((101).toByte, (100).toByte)
        |  let yourRegularContractHash = base58'GtBn7qJwK1v1EbB6CZdgmkcvt849VKVfWoJBMEWsvTew'
        |
        |  if (let flag : AssetBox = transaction) {
        |    flag.contractHash == yourRegularContractHash
        |  } else false
        |
        |  def getNumToExchange(amount: Int, rate: Int, lessBase: Bool): Int = {
        |    if(lessBase) {
        |      if (amount%rate==0) amount/rate else amount/rate + 1
        |    } else {
        |      amount * rate
        |    }
        |  }
        |
        |  def amAbleToOpen(box: Box, yourRegularContractHash: Array[Byte]): Bool = {
        |    if (let assetBox : AssetBox = box) {
        |      assetBox.contractHash == yourRegularContractHash
        |    } else false
        |  }
        |
        |  def getAmountFromBox(tokenId: Array[Byte], box : Box): Int = {
        |    if (let flag1: AssetBox = box) {
        |      if (flag1.tokenId == tokenId) flag1.amount else 0
        |    } else 0
        |  }
        |
        |  def hasTokenId(tokenId: Array[Byte], box : Box): Bool = {
        |    if(let assetBox : AssetBox = box) {
        |      if(assetBox.tokenId == tokenId) true else false
        |    } else false
        |  }
        |
        |  let buyAmount = getAmountFromBox(transaction.outputs.filter(lamb(x: Box) = !hasTokenId(acceptableTokenId, x))[0])
        |  let wantBuyOurs = getNumToExchange(buyAmount, rate, lessBase)
        |  if (wantBuyOurs > 0){
        |    let changeBox = transaction.outputs.filter(lamb(x: Box) = !hasTokenId(self.tokenId, x))[0]
        |    let declaredChangeAmount = getAmountFromBox(changeBox)
        |    if ((self.amount - wantBuyOurs == declaredChangeAmount) && amAbleToOpen(changeBox, yourRegularContractHash)) true else false
        |  } else false
        |}
      """.stripMargin

    val compiledTry: Try[CompiledContract] = PCompiler.compile(source)

    compiledTry.isSuccess shouldBe true
  }
}
