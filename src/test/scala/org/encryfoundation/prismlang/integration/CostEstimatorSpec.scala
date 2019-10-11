package org.encryfoundation.prismlang.integration

import org.scalatest.PropSpec
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.SpanSugar._
import scala.util.Try

class CostEstimatorSpec extends PropSpec with Utils with TimeLimits {

  property("Nested block attack") {
    val longMax: Long = Long.MaxValue
    val nestedCount: Int = 300
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
}
