package org.encryfoundation.prismlang.evaluator

object Arith {

  def checkType[T](v: Any): T = v match {
    case t: T@unchecked => t
    case otherT => throw new Exception(s"Unexpected type $otherT")
  }

  def add[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Long, o2: Long) => checkType[T](o1 + o2)
      case (o1: Byte, o2: Long) => checkType[T](o1 + o2)
      case (o1: Long, o2: Byte) => checkType[T](o1 + o2)
      case (o1: String, o2: String) => checkType[T](o1 + o2)
      case (leftT, rightT) => throw new Exception(s"$leftT and $rightT does not support `Add` operation")
    }
  }

  def sub[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Long, o2: Long) => checkType[T](o1 - o2)
      case (o1: Byte, o2: Long) => checkType[T](o1 - o2)
      case (o1: Long, o2: Byte) => checkType[T](o1 - o2)
      case (leftT, rightT) => throw new Exception(s"$leftT and $rightT does not support `Sub` operation")
    }
  }

  def mul[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Long, o2: Long) => checkType[T](o1 * o2)
      case (o1: Byte, o2: Long) => checkType[T](o1 * o2)
      case (o1: Long, o2: Byte) => checkType[T](o1 * o2)
      case (leftT, rightT) => throw new Exception(s"$leftT and $rightT does not support `Mul` operation")
    }
  }

  def div[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Long, o2: Long) => checkType[T](o1 / o2)
      case (o1: Byte, o2: Long) => checkType[T](o1 / o2)
      case (o1: Long, o2: Byte) => checkType[T](o1 / o2)
      case (leftT, rightT) => throw new Exception(s"$leftT and $rightT does not support `Div` operation")
    }
  }

  def mod[T](op1: Any, op2: Any): T = {
    (op1, op2) match {
      case (o1: Long, o2: Long) => checkType[T](o1 % o2)
      case (o1: Byte, o2: Long) => checkType[T](o1 % o2)
      case (o1: Long, o2: Byte) => checkType[T](o1 % o2)
      case (leftT, rightT) => throw new Exception(s"$leftT and $rightT does not support `Mod` operation")
    }
  }
}
