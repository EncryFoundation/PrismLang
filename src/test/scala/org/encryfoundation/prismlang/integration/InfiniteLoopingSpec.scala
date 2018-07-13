package org.encryfoundation.prismlang.integration

import org.scalatest.{Matchers, PropSpec}

class InfiniteLoopingSpec extends PropSpec with Matchers with Utils {
  //FIXME somehow passes compilation and falls in runtime with
  //org.encryfoundation.prismlang.evaluator.PRuntimeException: Wrong number of arguments, 2 required, 1 given.
  //to get error change line 21 to true and uncomment following lines
  property("Infinite recursion ") {
    val infiniteRecursion =
      s"""
                {
                  def sum(a: Int, b: Int): Int = {
                    sum(a, b)
                  }
                  let b = 7
                  let c = 5
                  let g = sum(c, b)
                  g
                }
        """.stripMargin

    val tryInfiniteRecursion = compiled(infiniteRecursion)
    tryInfiniteRecursion.isSuccess shouldBe false
    //    val evaluatedExpression = eval(tryInfiniteRecursion.get)
    //    val f = evaluatedExpression.get
    //    evaluatedExpression.isSuccess shouldBe false
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
