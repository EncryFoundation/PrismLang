package org.encryfoundation.prismlang.core.wrapped

import org.encryfoundation.prismlang.core.Types

trait PValue extends PWrappedMember {

  val tpe: Types.PType
  val value: Any

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: PValue => that.value == this.value
    case _ => false
  }
}

object PValue {

  def apply(v: Any, t: Types.PType): PValue = {
    if (!v.isInstanceOf[t.Underlying@unchecked])
      throw new Exception("Can't create PValue, actual value type mismatches the declared one")
    new PValue {
      override val tpe: Types.PType = t
      override val value: tpe.Underlying = v.asInstanceOf[tpe.Underlying]
    }
  }
}
