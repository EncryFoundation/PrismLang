package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.core.{Ast, Types}

case class CompiledContract(args: List[(String, Types.PType)], script: Ast.Expr)
