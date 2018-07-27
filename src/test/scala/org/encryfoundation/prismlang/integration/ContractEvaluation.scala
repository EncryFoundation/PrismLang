package org.encryfoundation.prismlang.integration

import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.compiler.CompiledContract.ContractHash
import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.{BoxedValue, PObject, PValue}
import org.encryfoundation.prismlang.evaluator.Evaluator
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey, Signature}
import scorex.utils.Random

trait ContractEvaluation {

  import ContractEvaluation._

  val dummyDigest: Array[Byte] = Random.randomBytes()

  val dummyContractHash: Digest32 = Blake2b256.hash(dummyDigest)

  val dummyMessage: Digest32 = Blake2b256.hash(dummyContractHash)

  val dummyBox: Box = Box(10000, dummyDigest, dummyDigest)

  val dummyTransaction: Transaction = Transaction(List(dummyDigest), List(dummyBox), dummyMessage)

  val dummyState: BlockchainState = BlockchainState(100, 57848286354L, dummyDigest)

  val dummyContext: Context = Context(dummyTransaction, dummyBox, dummyState)

  val pair: (PrivateKey, PublicKey) = Curve25519.createKeyPair(dummyDigest)

  val sig: Signature = Curve25519.sign(pair._1, dummyMessage)

  val proof: Proof = Proof(BoxedValue.Signature25519Value(sig.toList), None)

  def transactionWithOutputs(outputs: List[Box]): Transaction = Transaction(List(dummyDigest), outputs, dummyMessage)

  def canBeUnlocked(contract: CompiledContract)
                   (ctx: Context, proofs: Seq[Proof], requiredContractHash: ContractHash): Boolean = {
    if (sameHash(requiredContractHash, contract.hash)) {
      val env: List[(Option[String], PValue)] =
        if (contract.args.isEmpty) List.empty
        else List((None, ctx.transaction.asVal), (None, ctx.state.asVal), (None, ctx.box.asVal)) ++
          proofs.map(proof => (proof.tagOpt, proof.value))
      val args: List[(String, PValue)] = contract.args.map { case (name, tpe) =>
        env.find(_._1.contains(name))
          .orElse(env.find(e => e._2.tpe == tpe || tpe.isSubtypeOf(e._2.tpe)))
          .map(elt => name -> elt._2)
          .getOrElse(throw new Exception("Not enough arguments for contact"))
      }
      Evaluator.initializedWith(args).eval[Boolean](contract.script)
    } else false
  }

  def sameHash(h1: Array[Byte], h2: Array[Byte]): Boolean = h1.sameElements(h2)
}

object ContractEvaluation {

  case class Proof(value: BoxedValue, tagOpt: Option[String])

  case class Box(amount: Long, contractHash: ContractHash, tokenId: Array[Byte]) {
    def asPrism: PObject = {
      val randomBytes: List[Byte] = Random.randomBytes(32).toList
      val fields: Map[String, PValue] = Map(
        "contractHash" -> PValue(contractHash.toList, Types.PCollection.ofByte),
        "typeId" -> PValue(randomBytes, Types.PInt),
        "id" -> PValue(randomBytes, Types.PInt),
        "amount" -> PValue(amount, Types.PInt),
        "tokenId" -> PValue(tokenId.toList, Types.PCollection.ofByte)
      )
      PObject(fields, Types.AssetBox)
    }
    def asVal: PValue = PValue(asPrism, Types.AssetBox)
  }

  case class Transaction(inputs: List[Array[Byte]], outputs: List[Box], messageToSign: Array[Byte]) {
    def asPrism: PObject = PObject(Map(
      "inputs" -> PValue(inputs.map(_.toList), Types.PCollection(Types.PCollection.ofByte)),
      "outputs" -> PValue(outputs.map(_.asPrism), Types.PCollection(Types.EncryBox)),
      "messageToSign" -> PValue(messageToSign.toList, Types.PCollection.ofByte)
    ), Types.EncryTransaction)
    def asVal: PValue = PValue(asPrism, Types.EncryTransaction)
  }

  case class BlockchainState(height: Int, lastBlockTimestamp: Long, stateDigest: Array[Byte]) {
    def asPrism: PObject = PObject(Map(
      "height" -> PValue(height.toLong, Types.PInt),
      "lastBlockTimestamp" -> PValue(lastBlockTimestamp, Types.PInt),
      "stateDigest" -> PValue(stateDigest.toList, Types.PCollection.ofByte)
    ), Types.EncryState)
    def asVal: PValue = PValue(asPrism, Types.EncryState)
  }

  case class Context(transaction: Transaction, box: Box, state: BlockchainState)
}
