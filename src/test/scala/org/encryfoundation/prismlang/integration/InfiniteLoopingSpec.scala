package org.encryfoundation.prismlang.integration

import org.scalatest.PropSpec

class InfiniteLoopingSpec extends PropSpec with Utils {
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

    testCompiledExpressionWithOptionalEvaluation(infiniteRecursion, compilationSuccess = false)
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

    testCompiledExpressionWithOptionalEvaluation(infiniteRecursion, compilationSuccess = false)
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

    testCompiledExpressionWithOptionalEvaluation(infiniteRecursion, compilationSuccess = false)
  }
}
