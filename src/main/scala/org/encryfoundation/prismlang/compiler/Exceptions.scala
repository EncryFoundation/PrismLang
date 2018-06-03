package org.encryfoundation.prismlang.compiler

class SemanticAnalysisException(m: String) extends Exception(m)

case class NameException(n: String) extends SemanticAnalysisException(s"Name $n is undefined.")

case class AlreadyDefinedException(n: String) extends SemanticAnalysisException(s"Name $n is already defined in scope.")

case class UnexpectedStatementException(s: String) extends SemanticAnalysisException(s"Unexpected statement: $s.")

case object IllegalExprException extends SemanticAnalysisException(s"Unexpected expression.")

case object IllegalOperandException extends SemanticAnalysisException(s"Illegal operand type.")

case object ZeroDivisionException extends SemanticAnalysisException(s"Zero division.")

case object MissedContextException extends SemanticAnalysisException(s"Missed context for AST processing.")

case class WrongNumberOfArgumentsException(fn: String) extends SemanticAnalysisException(s"Wrong number of arguments in $fn.")

case class UnapplicableFunctionException(fn: String, coll: String) extends SemanticAnalysisException(s"$fn is unapplicable to $coll")

case class NotAnObjectException(n: String) extends SemanticAnalysisException(s"$n is not an object.")

case class UnresolvedSymbolException(s: String) extends SemanticAnalysisException(s"Can not resolve symbol $s")

case object Base58DecodeException extends SemanticAnalysisException("Base58 decode error.")

case object TypeException extends SemanticAnalysisException("Missed type.")

case class UnresolvedTypeException(s: String) extends SemanticAnalysisException(s"Type $s not found")

case class TypeMismatchException(t1: String, t2: String) extends SemanticAnalysisException(s"Expected type $t1, got $t2.")

case object NestedCollectionException extends SemanticAnalysisException("Nested collections are disallowed.")
