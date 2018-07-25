package org.encryfoundation.prismlang.integration

import org.encryfoundation.prismlang.compiler.{CompiledContract, PCompiler}
import org.scalatest.PropSpec
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.SpanSugar._
import scala.util.{Failure, Success, Try}

class CostEstimatorSpec extends PropSpec with Utils with TimeLimits {
  property("Nested block attack") {
    val longMax: Long = Long.MaxValue
    val nestedCount: Int = 302
    val varNames: Iterator[String] = List.range(0, nestedCount).map(_ => generateRandomString(10)).toIterator
    val source: String = varNames.map(variable => s"{let $variable: Int = $longMax \n").mkString +
      List.fill(nestedCount)("}").mkString

    estimateCost(compiled(source).get).isSuccess shouldBe false
  }

  property("Ast Length attack") {
    val longMax: Long = Long.MaxValue
    val count: Int = 100000
    val varNames: Iterator[String] = List.range(0, count).map(_ => generateRandomString(10)).toIterator
    val source: String = "{\n" + varNames.map(variable => s"{let $variable: Int = $longMax }\n").mkString + "}"

    failAfter(60 seconds) {
      val tryCost: Try[Int] = estimateCost(compiled(source).get)
      tryCost.isSuccess shouldBe false
    }
  }

  property("Complex Contract Check") {
    val contractString =
      """
        |contract (signature: Signature25519, transaction: Transaction) = {
        |  let rate = 2
        |  let lessBase = true
        |  let acceptableTokenId = Array((101).toByte, (100).toByte)
        |  let yourRegularContractHash = base58'GtBn7qJwK1v1EbB6CZdgmkcvt849VKVfWoJBMEWsvTew'
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
        |    if(let assetBox: AssetBox = box) {
        |      assetBox.contractHash == yourRegularContractHash
        |    } else false
        |  }
        |
        |  def getAmountFromBox(tokenId: Array[Byte], box : Box): Int = {
        |    if(let assetBox: AssetBox = box) {
        |      if(assetBox.tokenId == tokenId) assetBox.amount else 0
        |    } else 0
        |  }
        |
        |  def hasTokenId(tokenId: Array[Byte], box : Box): Bool = {
        |    if(let assetBox: AssetBox = box) {
        |      if(assetBox.tokenId == tokenId) true else false
        |    } else false
        |  }
        |
        |  let byuAmount = getAmountFromBox(transaction.outputs.filter(lamb(x: Box) = !hasTokenId(acceptableTokenId, x))[0])
        |  let wantBuyOurs = getNumToExchange(buyAmount, rate, lessBase)
        |  if (wantBuyOurs > 0){
        |    let changeBox = transaction.outputs.filter(lamb(x: Box) = !hasTokenId(self.tokenId, x))[0]
        |    let declaredChangeAmount = getAmountFromBox(changeBox)
        |    if ((self.amount - wantBuyOurs == declaredChangeAmount) && amAbleToOpen(changeBox, yourRegularContractHash)) true else false
        |  } else false
        |}
      """.stripMargin

    val tryContract: Try[CompiledContract] = PCompiler.compile(contractString)
    tryContract.get
    tryContract.isSuccess shouldBe true
    tryContract match {
      case Success(contract) => estimateCost(contract.script) match {
        case Success(cost) => cost shouldEqual 975
        case Failure(e) => throw e
      }
      case _ => fail
    }
  }
}
