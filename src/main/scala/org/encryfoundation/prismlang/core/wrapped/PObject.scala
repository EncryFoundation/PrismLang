package org.encryfoundation.prismlang.core.wrapped

import org.encryfoundation.prismlang.core.Types

case class PObject(fields: Map[String, PValue], tpe: Types.Product) extends PWrappedMember {

  def getAttr(n: String): Option[PValue] = fields.get(n)

  def isInstanceOf(t: Types.PType): Boolean = t match {
    case p: Types.Product => this.tpe == p || this.tpe.isSubtypeOf(p)
    case _ => false
  }

  /** This method allows shallow type matching not by type descriptor
    * itself, but only by its fingerprint - unique identifier of
    * arbitrary data structure. */
  def hasSameFingerprint(thatF: String): Boolean = tpe match {
    case prod: Types.ArbitraryProduct => thatF == prod.fingerprint
    case _ => false
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case PObject(thatAttrs, _) => thatAttrs.zip(this.fields).forall { case ((_, v1), (_, v2)) => v1 == v2 }
    case _ => false
  }
}
