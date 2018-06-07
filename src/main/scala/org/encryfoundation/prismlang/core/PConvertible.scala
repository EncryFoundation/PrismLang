package org.encryfoundation.prismlang.core

trait PConvertible {
  val esType: Types.Product
  def asVal: PValue
  def convert: PObject
}
