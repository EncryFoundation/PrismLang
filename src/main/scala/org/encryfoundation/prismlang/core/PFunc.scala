package org.encryfoundation.prismlang.core

case class PFunc(args: List[(String, Types.PType)],
                 returnType: Types.PType,
                 body: Ast.Expr)
