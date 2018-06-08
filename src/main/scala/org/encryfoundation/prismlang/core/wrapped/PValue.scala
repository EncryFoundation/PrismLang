package org.encryfoundation.prismlang.core.wrapped

import org.encryfoundation.prismlang.core.Types

sealed trait PValue extends PWrappedMember {

  val tpe: Types.PType
  val value: tpe.Underlying

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: PValue => that.value == this.value
    case _ => false
  }
}

object PValue {

  def apply(t: Types.PType)(v: Any): PValue = {
    if (!v.isInstanceOf[t.Underlying])
      throw new Exception("Can't create PValue, actual value type mismatches the declared one")
    new PValue {
      override val tpe: Types.PType = t
      override val value: tpe.Underlying = v.asInstanceOf[tpe.Underlying]
    }
  }
}
