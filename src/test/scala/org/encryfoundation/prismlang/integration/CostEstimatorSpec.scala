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
          contract (signature: Array[Signature25519], transaction: Transaction, box: Box) = {
            let minObrPKey = base58'5QCPz4eZAgT8DLAoZDSeouLMk1Kcf6DjJzrURiSV9U9'
            let universityPk = base58'11NDaGfSWVg9qjjPc4QjGYJL8ErvGRrmKGEW5FSMq3i'
            let studentPk = base58'75Gs7HHUNnoEzsPgRRVABzQaC3UZVcayw9NY457Kx5p'

            let myAssetId = base58'GtBn7qJwK1v1EbB6CZdgmkcvt849VKVfWoJBMEWsvTew'
            let keys = Array(minObrPKey, universityPk, studentPk)

            def isMyAssetBox(box: Box): Bool = if (let assetBox: AssetBox = box) true else false

            def isSomeCriteria(): Bool = {
              let A : Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
              11 in A
            }

            def isSomeMoreCriteria(): Bool = {
              let A : Array[Int] = Array(1,2,3,4)
              allOf(Array(2 > 1, 1 == 1, 3 != 4, 10 < 100, 3 in A,true))
            }

            let checkedFSig = if (checkSig(transaction.messageToSign, signature[0], keys[0])) 1 else 0
            let checkedSSig = if (checkSig(transaction.messageToSign, signature[1], keys[1])) 1 else 0
            let checkedTSig = if (checkSig(transaction.messageToSign, signature[2], keys[2])) 1 else 0
            let sum = checkedFSig + checkedSSig + checkedTSig
            (sum > 1) || isMyAssetBox(box) || (isSomeCriteria() && isSomeMoreCriteria())
          }
      """.stripMargin

    val tryContract: Try[CompiledContract] = PCompiler.compile(contractString)
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
