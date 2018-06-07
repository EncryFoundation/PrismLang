package org.encryfoundation.prismlang.core

case class PObject(fields: Map[String, PValue], tpe: Types.Product) {

  def getAttr(n: String): Option[PValue] = fields.get(n)

  def isInstanceOf(t: Types.PType): Boolean = t match {
    case p: Types.Product => this.tpe == p || this.tpe.isSubtypeOf(p)
    case _ => false
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case PObject(thatAttrs, _) => thatAttrs.zip(this.fields).forall { case ((_, v1), (_, v2)) => v1 == v2 }
    case _ => false
  }
}
