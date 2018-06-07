package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.core.wrapped.PWrappedMember
import org.encryfoundation.prismlang.lib.predefined.PredefFunctions

case class ScopedRuntimeEnvironment(level: Int,
                                    members: Map[String, PWrappedMember],
                                    parentOpt: Option[ScopedRuntimeEnvironment] = None,
                                    isFunc: Boolean = false) {

  def updated(name: String, member: PWrappedMember): ScopedRuntimeEnvironment =
    ScopedRuntimeEnvironment(level, members.updated(name, member), parentOpt)

  def emptyChild: ScopedRuntimeEnvironment = ScopedRuntimeEnvironment.empty(level + 1, Some(this))

  def child(members: Map[String, PWrappedMember],
            isFunc: Boolean = false): ScopedRuntimeEnvironment = ScopedRuntimeEnvironment(level + 1, members, Some(this), isFunc)

  def get(id: String): Option[PWrappedMember] = members.get(id).orElse(parentOpt.flatMap(_.get(id)))

  override def toString: String = s"<ScopedContext lvl=$level size=${members.size}>"
}

object ScopedRuntimeEnvironment {

  def empty(level: Int, parent: Option[ScopedRuntimeEnvironment] = None): ScopedRuntimeEnvironment =
    ScopedRuntimeEnvironment(level, Map.empty, parent)

  def initialized(level: Int, env: Map[String, PWrappedMember]): ScopedRuntimeEnvironment =
    ScopedRuntimeEnvironment(level, env ++ PredefFunctions.all, None)
}
