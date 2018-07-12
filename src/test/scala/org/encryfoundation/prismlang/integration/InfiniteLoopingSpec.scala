package org.encryfoundation.prismlang.integration

import org.scalatest.{Matchers, PropSpec}

class InfiniteLoopingSpec extends PropSpec with Matchers with Utils {
  property("Infinite recursion ") {
    val infiniteRecursion =
      s"""
                {
                  def sum(a: Int, b: Int): Int = {
                    sum(a,b)
                  }

                  let a = sum(5,7)
                }
        """.stripMargin
    val tryInfiniteRecursion = compiled(infiniteRecursion)
    tryInfiniteRecursion.isSuccess shouldBe true
    val evaluatedExpression = eval(tryInfiniteRecursion.get)
    evaluatedExpression.isSuccess shouldBe false
  }

  property("Cross infinite recursion") {
    val infiniteRecursion =
      s"""
                {
                  def sum2(d: Int, c: Int) : Int = {
                    sum(d,c)
                  }

                  def sum(a: Int, b: Int): Int = {
                    sum2(a,b)
                  }

                  let g = sum(5,7)
                  g
                }
        """.stripMargin
    val tryInfiniteRecursion = compiled(infiniteRecursion)
    tryInfiniteRecursion.isSuccess shouldBe false
  }

  property("Cross infinite recursion lambda version") {
    val infiniteRecursion =
      s"""
                {
                  def sum(a: Int, b: Int): Int = {
                    let v : Int = lamb(d : Int, c : Int) = sum(d,c)
                    v
                  }

                  let g = sum(5,7)
                  g
                }
        """.stripMargin
    val tryInfiniteRecursion = compiled(infiniteRecursion)
    tryInfiniteRecursion.isSuccess shouldBe false
  }
}
