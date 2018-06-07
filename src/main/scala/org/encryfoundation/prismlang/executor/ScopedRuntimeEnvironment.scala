package org.encryfoundation.prismlang.executor

import org.encryfoundation.prismlang.core.wrapped.PWrappedMember
import org.encryfoundation.prismlang.lib.predefined.PredefFunctions

case class ScopedRuntimeEnvironment(name: String,
                                    level: Int,
                                    members: Map[String, PWrappedMember],
                                    parentOpt: Option[ScopedRuntimeEnvironment] = None,
                                    isFunc: Boolean = false) {

  def updated(name: String, member: PWrappedMember): ScopedRuntimeEnvironment =
    ScopedRuntimeEnvironment(name, level, members.updated(name, member), parentOpt)

  def emptyChild(n: String): ScopedRuntimeEnvironment = ScopedRuntimeEnvironment.empty(n, level + 1, Some(this))

  def child(n: String,
            ms: Map[String, PWrappedMember],
            isFunc: Boolean = false): ScopedRuntimeEnvironment = ScopedRuntimeEnvironment(n, level + 1, ms, Some(this), isFunc)

  def get(id: String): Option[PWrappedMember] = members.get(id).orElse(parentOpt.flatMap(_.get(id)))

  override def toString: String = s"<ScopedContext name=$name lvl=$level size=${members.size}>"
}

object ScopedRuntimeEnvironment {

  def empty(n: String, l: Int, parent: Option[ScopedRuntimeEnvironment] = None): ScopedRuntimeEnvironment =
    ScopedRuntimeEnvironment(n, l, Map.empty, parent)

  def initialized(n: String, l: Int, env: Map[String, PWrappedMember]): ScopedRuntimeEnvironment =
    ScopedRuntimeEnvironment(n, l, env ++ PredefFunctions.all, None)
}
