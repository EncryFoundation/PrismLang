package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.core.{Ast, TypeSystem, Types}

object StructDescriptorInterpreter {

  case class StructDescriptorInterpretationException(msg: String) extends Exception(msg)

  def interpretStruct(struct: Ast.Struct): Types.StructTag = {
    def interpretDescriptor(tpe: Ast.TypeDescriptor): Types.PType = tpe match {
      case Ast.TypeDescriptor.SimpleType(id, tps) =>
        val typeParams: List[Types.PType] = tps.map(interpretDescriptor)
        TypeSystem.default.typeByIdent(id.name).map {
          case Types.PCollection(_) =>
            if (typeParams.size == 1) Types.PCollection(typeParams.head)
            else throw StructDescriptorInterpretationException("'Array[T]' takes exactly one type parameter")
          case Types.POption(_) =>
            if (typeParams.size == 1) Types.POption(typeParams.head)
            else throw StructDescriptorInterpretationException("'Option[T]' takes exactly one type parameter")
          case otherT: Types.PType =>
            if (typeParams.isEmpty) otherT
            else throw StructDescriptorInterpretationException(s"'$otherT' does not take type parameters")
        }.getOrElse(throw StructDescriptorInterpretationException(s"Type `${id.name}` not found"))
      case Ast.TypeDescriptor.ProductType(props) =>
        Types.ArbitraryProduct("$anon_obj", props.map { case (id, tp) => id.name -> interpretDescriptor(tp) })
    }
    Types.StructTag(struct.id.name, interpretDescriptor(struct.typeDescriptor))
  }
}
