package org.encryfoundation.prismlang.core

import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}

trait PConvertible {
  val esType: Types.Product
  def asVal: PValue
  def convert: PObject
}
