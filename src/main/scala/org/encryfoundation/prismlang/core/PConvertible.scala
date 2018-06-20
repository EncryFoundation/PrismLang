package org.encryfoundation.prismlang.core

import org.encryfoundation.prismlang.core.wrapped.PValue

trait PConvertible {
  val tpe: Types.Product
  def asVal: PValue
}
