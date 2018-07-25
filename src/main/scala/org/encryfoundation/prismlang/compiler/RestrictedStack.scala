package org.encryfoundation.prismlang.compiler

case class RestrictedStack(depthLimit: Int) {
  private var maxDepth: Int = 0
  var stack: List[Int] = List()

  def currentDepth: Int = stack.size

  def getMaxDepth: Int = maxDepth

  private def push(value: Int): Unit = if (stack.size < depthLimit) {
    stack = value :: stack
    maxDepth = if (value > maxDepth) value else maxDepth
  } else throw new Exception("Maximum nested depth exceeded, allowed up to 300")

  def pushNext(): Unit = push(stack.headOption.getOrElse(0) + 1)

  def pop: Int = stack.headOption match {
    case Some(num) => stack = stack.tail
      num
    case _ => throw new Exception("Stack underflow")
  }

  def penalty: Double = math.pow(math.log(currentDepth), 2)
}

object RestrictedStack {
  def default: RestrictedStack = RestrictedStack(300)
} 

