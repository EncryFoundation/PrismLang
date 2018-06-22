package org.encryfoundation.prismlang.lib.predefined

import org.encryfoundation.prismlang.core.wrapped.PFunctionPredef

trait BuiltInFunctionHolder {
  val name: String
  val cost: Int
  def asFunc: PFunctionPredef
}
