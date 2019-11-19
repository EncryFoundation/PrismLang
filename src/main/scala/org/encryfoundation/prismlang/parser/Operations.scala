package org.encryfoundation.prismlang.parser

import fastparse._

object Operations {

  import org.encryfoundation.prismlang.core.Ast
  implicit def whitespace(cfg: P[_]): P[Unit] = Lexer.WS(cfg)

  /** Common operators, mapped from their strings to their type-safe representations */
  def op[T, _: P](s: P0, rhs: T): P[T] = s.!.map(_ => rhs)
  def Lt[_: P]: P[Ast.CompOp.Lt.type] = op("<", Ast.CompOp.Lt)
  def Gt[_: P]: P[Ast.CompOp.Gt.type] = op(">", Ast.CompOp.Gt)
  def Eq[_: P]: P[Ast.CompOp.Eq.type] = op("==", Ast.CompOp.Eq)
  def GtE[_: P]: P[Ast.CompOp.GtE.type] = op(">=", Ast.CompOp.GtE)
  def LtE[_: P]: P[Ast.CompOp.LtE.type] = op("<=", Ast.CompOp.LtE)
  def NotEq[_: P]: P[Ast.CompOp.NotEq.type] = op("<>" | "!=", Ast.CompOp.NotEq)
  def In[_: P]: P[Ast.CompOp.In.type] = op("in", Ast.CompOp.In)
  def NotIn[_: P]: P[Ast.CompOp.NotIn.type] = op("not in", Ast.CompOp.NotIn)
  def compOp[_: P]: P[Ast.CompOp] = P( LtE|GtE|Eq|Gt|Lt|NotEq|In|NotIn )
  def Add[_: P]: P[Ast.Operator.Add.type] = op("+", Ast.Operator.Add)
  def Sub[_: P]: P[Ast.Operator.Sub.type] = op("-", Ast.Operator.Sub)
  def Pow[_: P]: P[Ast.Operator.Pow.type] = op("**", Ast.Operator.Pow)
  def Mult[_: P]: P[Ast.Operator.Mult.type] = op("*", Ast.Operator.Mult)
  def Div[_: P]: P[Ast.Operator.Div.type] = op("/", Ast.Operator.Div)
  def Mod[_: P]: P[Ast.Operator.Mod.type] = op("%", Ast.Operator.Mod)
  def Invert[_: P]: P[Ast.UnaryOp.Invert.type] = op("~", Ast.UnaryOp.Invert)
  def unaryOp[_: P]: P[Ast.UnaryOp] = P ( Invert )

  def Unary[_: P](p: => P[Ast.Expr]): P[Ast.Expr.Unary] =
    (unaryOp ~ p).map { case (op, operand) => Ast.Expr.Unary(op, operand) }

  def Chain[_: P](p: => P[Ast.Expr], op: => P[Ast.Operator]): P[Ast.Expr] =
    P( p ~ (op ~ p).rep).map {
        case (lhs, chunks) =>
          chunks.foldLeft(lhs) { case (lhs, (op, rhs)) =>
            Ast.Expr.Bin(lhs, op, rhs)
          }
      }
}
