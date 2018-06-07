package org.encryfoundation.prismlang.core.wrapped

import org.encryfoundation.prismlang.core.{Ast, Types}

case class PFunction(args: List[(String, Types.PType)],
                     returnType: Types.PType,
                     body: Ast.Expr) extends PWrappedMember
