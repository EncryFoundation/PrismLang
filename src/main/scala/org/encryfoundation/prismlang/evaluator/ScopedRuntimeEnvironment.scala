package org.encryfoundation.prismlang.evaluator

import org.encryfoundation.prismlang.core.wrapped.{PFunction, PWrappedMember}
import org.encryfoundation.prismlang.lib.predefined.PredefFunctions
import cats.implicits._
import org.encryfoundation.prismlang.core.Types.PType

case class ScopedRuntimeEnvironment(level: Int,
                                    members: Map[String, PWrappedMember],
                                    functions: Map[String, Set[PFunction]],
                                    parentOpt: Option[ScopedRuntimeEnvironment] = None,
                                    isFunc: Boolean = false) {

  def updated(name: String, member: PWrappedMember): ScopedRuntimeEnvironment =
    member match {
      case f: PFunction => ScopedRuntimeEnvironment(level, members, functions |+| Map(name -> Set(f)), parentOpt)
      case _ => ScopedRuntimeEnvironment(level, members.updated(name, member), functions, parentOpt)
    }

  def emptyChild: ScopedRuntimeEnvironment = ScopedRuntimeEnvironment.empty(level + 1, Some(this))

  def child(members: Map[String, PWrappedMember], sFunc: Boolean = false): ScopedRuntimeEnvironment =
    ScopedRuntimeEnvironment(level + 1, members, functions, Some(this), isFunc)

  def get(id: String): Option[PWrappedMember] = members.get(id).orElse(parentOpt.flatMap(_.get(id)))

  def getFunction(id: String, argsTypes: List[PType]): Option[PWrappedMember] =
    functions.get(id).flatMap(_.find(elem =>
      elem.args.length == argsTypes.length && elem.args.map(_._2).zip(argsTypes).forall { case (dt, ft) => dt == ft }
    )).orElse(parentOpt.flatMap(_.getFunction(id, argsTypes)))

  override def toString: String = s"<ScopedContext lvl=$level size=${members.size}>"
}

object ScopedRuntimeEnvironment {

  def empty(level: Int, parent: Option[ScopedRuntimeEnvironment] = None): ScopedRuntimeEnvironment =
    ScopedRuntimeEnvironment(level, Map.empty, Map.empty, parent)

  def initialized(level: Int, env: Map[String, PWrappedMember]): ScopedRuntimeEnvironment =
    ScopedRuntimeEnvironment(level, env ++ PredefFunctions.all, Map.empty, None)
}
