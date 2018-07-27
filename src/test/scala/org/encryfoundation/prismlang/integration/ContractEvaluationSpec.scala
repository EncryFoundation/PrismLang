package org.encryfoundation.prismlang.integration

import org.encryfoundation.prismlang.compiler.{CompiledContract, PCompiler}
import org.encryfoundation.prismlang.integration.ContractEvaluation.{Box, Context}
import org.scalatest.{Matchers, PropSpec, TryValues}
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32

import scala.util.Try

class ContractEvaluationSpec extends PropSpec with Matchers with ContractEvaluation with TryValues {

  property("PubKey lock") {

    val contractSource: String =
      s"""
        |contract (sig: Signature25519, tx: Transaction) = {
        |  let pubKey = base16'${Base16.encode(pair._2)}'
        |  checkSig(sig, tx.messageToSign, pubKey)
        |}
      """.stripMargin

    val compiledContract: Try[CompiledContract] = PCompiler.compile(contractSource)

    canBeUnlocked(compiledContract.success.value)(dummyContext, Seq(proof), compiledContract.success.value.hash) shouldBe true
  }

  property("Time window lock") {

    val contractSource: String =
      s"""
         |contract (sig: Signature25519, tx: Transaction, state: State) = {
         |  let pubKey = base16'${Base16.encode(pair._2)}'
         |  let timeWindow = Array(50, 500)
         |  state.height > timeWindow[0] && state.height < timeWindow[1] && checkSig(sig, tx.messageToSign, pubKey)
         |}
      """.stripMargin

    val compiledContract: Try[CompiledContract] = PCompiler.compile(contractSource)

    canBeUnlocked(compiledContract.success.value)(dummyContext, Seq(proof), compiledContract.success.value.hash) shouldBe true
  }

  property("Token exchange (valid exchange case)") {

    val tokenId: Array[Byte] = dummyDigest
    val intrinsicTokenId: Array[Byte] = dummyMessage
    val contractHashEmitterCanSpend: Digest32 = dummyContractHash

    val exchangeRate: Int    = 2
    val boxAmount: Long      = 20000
    val spendingAmount: Long = 5000

    val returnBox: Box = Box(boxAmount - spendingAmount, contractHashEmitterCanSpend, tokenId)
    val paymentBox: Box = Box(spendingAmount / exchangeRate, contractHashEmitterCanSpend, intrinsicTokenId)
    val transaction: ContractEvaluation.Transaction = transactionWithOutputs(List(returnBox, paymentBox))

    val context: Context = Context(transaction, dummyBox, dummyState)

    val contractSource: String =
      s"""
         |contract (signature: Signature25519, transaction: Transaction, self: AssetBox) = {
         |  let exchangeRate         = $exchangeRate
         |  let acceptableTokenId    = base16'${Base16.encode(tokenId)}'
         |  let intrinsicTokenId     = base16'${Base16.encode(intrinsicTokenId)}'
         |  let requiredContractHash = base16'${Base16.encode(contractHashEmitterCanSpend)}'
         |
         |  let recallPubKey         = base16'${Base16.encode(pair._2)}'
         |
         |  def isReturnBox(bx: Box): Bool = {
         |    if (let assetBx: AssetBox = bx) {
         |      assetBx.tokenId == acceptableTokenId &&
         |      assetBx.contractHash == requiredContractHash
         |    } else false
         |  }
         |
         |  checkSig(signature, transaction.messageToSign, recallPubKey) ||
         |  (if (let returnBox: AssetBox = transaction.outputs.filter(isReturnBox)[0]) {
         |    transaction.outputs.exists(lamb (bx: Box) = if (let assetBx: AssetBox = bx) {
         |      assetBx.tokenId == intrinsicTokenId &&
         |      assetBx.contractHash == requiredContractHash &&
         |      ((self.amount - returnBox.amount) / exchangeRate) <= assetBx.amount
         |    } else false)
         |  } else false)
         |}
      """.stripMargin

    val compiledContract: Try[CompiledContract] = PCompiler.compile(contractSource)

    canBeUnlocked(compiledContract.success.value)(context, Seq(proof), compiledContract.success.value.hash) shouldBe true
  }

  property("Token exchange (recall case)") {

    val tokenId: Array[Byte] = dummyDigest
    val intrinsicTokenId: Array[Byte] = dummyMessage
    val contractHashEmitterCanSpend: Digest32 = dummyContractHash

    val exchangeRate: Int = 2

    val contractSource: String =
      s"""
         |contract (signature: Signature25519, transaction: Transaction, self: AssetBox) = {
         |  let exchangeRate         = $exchangeRate
         |  let acceptableTokenId    = base16'${Base16.encode(tokenId)}'
         |  let intrinsicTokenId     = base16'${Base16.encode(intrinsicTokenId)}'
         |  let requiredContractHash = base16'${Base16.encode(contractHashEmitterCanSpend)}'
         |
         |  let recallPubKey         = base16'${Base16.encode(pair._2)}'
         |
         |  def isReturnBox(bx: Box): Bool = {
         |    if (let assetBx: AssetBox = bx) {
         |      assetBx.tokenId == acceptableTokenId &&
         |      assetBx.contractHash == requiredContractHash
         |    } else false
         |  }
         |
         |  checkSig(signature, transaction.messageToSign, recallPubKey) ||
         |  (if (let returnBox: AssetBox = transaction.outputs.filter(isReturnBox)[0]) {
         |    transaction.outputs.exists(lamb (bx: Box) = if (let assetBx: AssetBox = bx) {
         |      assetBx.tokenId == intrinsicTokenId &&
         |      assetBx.contractHash == requiredContractHash &&
         |      ((self.amount - returnBox.amount) / exchangeRate) >= assetBx.amount
         |    } else false)
         |  } else false)
         |}
      """.stripMargin

    val compiledContract: Try[CompiledContract] = PCompiler.compile(contractSource)

    canBeUnlocked(compiledContract.success.value)(dummyContext, Seq(proof), compiledContract.success.value.hash) shouldBe true
  }
}
