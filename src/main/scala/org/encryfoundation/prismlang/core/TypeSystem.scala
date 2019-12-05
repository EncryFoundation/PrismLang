package org.encryfoundation.prismlang.core

import com.typesafe.scalalogging.StrictLogging
import org.encryfoundation.prismlang.core.Ast.{Ident, TypeIdent}

case class TypeSystem(additionalTypes: Seq[Types.PType]) extends StrictLogging {

  import Types._

  case class TypeSystemException(msg: String) extends Exception(msg)

  lazy val allTypes: List[PType] = regularTypes ++ additionalTypes :+ PFunc(List.empty, Nit)

  lazy val typesMap: Map[String, PType] = allTypes.map(t => t.ident -> t).toMap

  def typeByIdent(id: String): Option[PType] = typesMap.get(id)

  /** Resolves the type from its string representation
    * (including type parameters). */
  def resolveType(ident: TypeIdent): Types.PType = {
    val typeParams: List[Types.PType] = ident.typeParams.map(resolveType)
    typeByIdent(ident.name).map {
      case Types.PCollection(_) =>
        if (typeParams.size == 1) Types.PCollection(typeParams.head)
        else throw TypeSystemException("'Array[T]' takes exactly one type parameter")
      case Types.PTuple(types) =>
        if (typeParams.size == types.size) Types.PTuple(typeParams)
        else throw TypeSystemException(s"'Tuple${types.size}' takes exactly ${types.size} type parameters")
      case otherT: Types.PType =>
        if (typeParams.isEmpty) otherT
        else throw TypeSystemException(s"'$otherT' does not take type parameters")
    }.getOrElse(throw TypeSystemException(s"Type '${ident.name}' is undefined."))
  }

  def resolveArgs(args: List[(Ident, TypeIdent)]): List[(String, Types.PType)] = {
    logger.debug(s"Resolving args: ${args.map(arg => s"(${arg._1}, ${arg._2})").mkString(",")}")

    args.map { case (id, typeId) => id.name -> resolveType(typeId) }
  }
}

object TypeSystem {

  def default: TypeSystem = TypeSystem(Seq.empty)
}
