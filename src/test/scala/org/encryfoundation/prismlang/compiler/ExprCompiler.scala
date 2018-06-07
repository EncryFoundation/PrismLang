package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.core.Ast.Expr.Contract
import org.encryfoundation.prismlang.core.TypeSystem

import scala.util.Try

trait ExprCompiler {

  def compile(expr: Expr): Try[Expr] = Try(StaticAnalyser(TypeSystem.default).scan(Transformer.transform(expr)))

  def compileContract(contract: Contract): Try[Contract] =
    StaticAnalyser(TypeSystem.default).scanContract(Transformer.transform(contract).asInstanceOf[Contract])
}
