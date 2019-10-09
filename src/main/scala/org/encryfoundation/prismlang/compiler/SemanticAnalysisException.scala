package org.encryfoundation.prismlang.compiler

case class SemanticAnalysisException(m: String) extends Exception(m)

object SemanticAnalysisException {
  def apply(msg: String): Nothing =  throw new SemanticAnalysisException(msg)
}