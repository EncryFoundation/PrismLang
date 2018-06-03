package org.encryfoundation.prismlang.parser

import fastparse.noApi._
import org.encryfoundation.prismlang.core.Ast
import org.encryfoundation.prismlang.parser.WsApi._

object Expressions {

  import org.encryfoundation.prismlang.parser.Lexer._

  val trueExpr: P[Ast.Expr.True.type] = P( kwd("true").rep(min = 1, max = 1).! ).map(_ => Ast.Expr.True)
  val falseExpr: P[Ast.Expr.False.type] = P( kwd("false").rep(min = 1, max = 1).! ).map(_ => Ast.Expr.False)

  val intConstExpr: P[Ast.Expr.IntConst] = P( Lexer.integer ).map(Ast.Expr.IntConst)

  val NAME: P[Ast.Ident] = Lexer.identifier
  val NUMBER: P[Ast.Expr.IntConst] = P( intConstExpr )
  val BOOL: P[Ast.Expr] = P( trueExpr | falseExpr )
  val STRING: P[String] = Lexer.stringliteral
  val BASE58STRING: P[String] = P( "base58" ~/ Lexer.stringliteral )
  val BASE16STRING: P[String] = P( "base16" ~/ Lexer.stringliteral )
}
