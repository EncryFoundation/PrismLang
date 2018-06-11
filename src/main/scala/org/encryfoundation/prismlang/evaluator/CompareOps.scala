package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.core.wrapped.PObject

object CompareOps {

  def eq(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Long, o2: Long) => o1 == o2
      case (o1: Byte, o2: Long) => o1 == o2
      case (o1: Long, o2: Byte) => o1 == o2
      case (o1: Boolean, o2: Boolean) => o1 == o2
      case (o1: Array[Byte], o2: Array[Byte]) => o1 sameElements o2
      case (o1: PObject, o2: PObject) => o1 == o2
      case (leftT, rightT) => throw new Exception(s"$leftT and $rightT does not support `Eq` operation")
    }
  }

  def gt(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Long, o2: Long) => o1 > o2
      case (o1: Byte, o2: Long) => o1 > o2
      case (o1: Long, o2: Byte) => o1 > o2
      case (leftT, rightT) => throw new Exception(s"$leftT and $rightT does not support `Gt` operation")
    }
  }

  def gte(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Long, o2: Long) => o1 >= o2
      case (o1: Byte, o2: Long) => o1 >= o2
      case (o1: Long, o2: Byte) => o1 >= o2
      case (leftT, rightT) => throw new Exception(s"$leftT and $rightT does not support `Gte` operation")
    }
  }

  def lt(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Long, o2: Long) => o1 < o2
      case (o1: Byte, o2: Long) => o1 < o2
      case (o1: Long, o2: Byte) => o1 < o2
      case (leftT, rightT) => throw new Exception(s"$leftT and $rightT does not support `Lt` operation")
    }
  }

  def lte(op1: Any, op2: Any): Boolean = {
    (op1, op2) match {
      case (o1: Long, o2: Long) => o1 <= o2
      case (o1: Byte, o2: Long) => o1 <= o2
      case (o1: Long, o2: Byte) => o1 <= o2
      case (leftT, rightT) => throw new Exception(s"$leftT and $rightT does not support `Lte` operation")
    }
  }
}
