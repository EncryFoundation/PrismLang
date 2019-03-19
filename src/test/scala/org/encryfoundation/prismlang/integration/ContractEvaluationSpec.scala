package org.encryfoundation.prismlang.integration

import org.encryfoundation.prismlang.compiler.{CompiledContract, PCompiler}
import org.encryfoundation.prismlang.core.wrapped.BoxedValue.MultiSignatureValue
import org.encryfoundation.prismlang.integration.ContractEvaluation.{Box, Context, Proof}
import org.scalatest.{Matchers, PropSpec, TryValues}
import scorex.crypto.encode.{Base16, Base58}
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}

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

  property("PubKey lock (invalid sig case)") {

    val contractSource: String =
      s"""
         |contract (sig: Signature25519, tx: Transaction) = {
         |  let pubKey = base16'${Base16.encode(pair._2.reverse)}'
         |  checkSig(sig, tx.messageToSign, pubKey)
         |}
      """.stripMargin

    val compiledContract: Try[CompiledContract] = PCompiler.compile(contractSource)

    canBeUnlocked(compiledContract.success.value)(dummyContext, Seq(proof), compiledContract.success.value.hash) shouldBe false
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

  property("Multisig") {

    val owners: Seq[(PrivateKey, PublicKey)] = pairsForMultisig

    val contractSource: String =
      s"""
        |contract (signature: MultiSig, transaction: Transaction) = {
        |  def isValidSig(signature: MultiSig, message: Array[Byte], key: Array[Byte]): Bool = {
        |    anyOf(signature.map(lamb (x: Array[Byte]) = checkSig(x, message, key)))
        |  }
        |
        |  let ownerPubKey = base58'${Base58.encode(owners.head._2)}'
        |  let garantPubKey = base58'${Base58.encode(owners(1)._2)}'
        |  let receiverPubKey = base58'${Base58.encode(owners(2)._2)}'
        |  let keys = Array(ownerPubKey, garantPubKey, receiverPubKey)
        |  let all: Array[Int] = keys.map(lamb(x: Array[Byte]) = if(isValidSig(signature, transaction.messageToSign, x)) 1 else 0)
        |  all.sum > 1
        |}
      """.stripMargin

    val compiledContract: Try[CompiledContract] = PCompiler.compile(contractSource)
    val signatures: List[List[Byte]] = owners.take(2).map(_._1).map(k => Curve25519.sign(k, compiledContract.success.value.hash).toList).toList
    val tx: ContractEvaluation.Transaction = dummyTransaction.copy(messageToSign = compiledContract.success.value.hash)
    val proofs: Seq[Proof] = Seq(Proof(MultiSignatureValue(signatures), Some("signature")))

    canBeUnlocked(compiledContract.success.value)(dummyContext.copy(transaction = tx), proofs, compiledContract.success.value.hash) shouldBe true
  }
}
