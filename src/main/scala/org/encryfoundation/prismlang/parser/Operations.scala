package org.encryfoundation.prismlang.parser

import fastparse.noApi._
import fastparse.{core, noApi}
import org.encryfoundation.prismlang.parser.WsApi._

object Operations {

  import org.encryfoundation.prismlang.core.Ast

  // Common operators, mapped from their
  // strings to their type-safe representations
  def op[T](s: P0, rhs: T): core.Parser[T, Char, String] = s.!.map(_ => rhs)
  val Lt: core.Parser[Ast.CompOp.Lt.type, Char, String] = op("<", Ast.CompOp.Lt)
  val Gt: core.Parser[Ast.CompOp.Gt.type, Char, String] = op(">", Ast.CompOp.Gt)
  val Eq: core.Parser[Ast.CompOp.Eq.type, Char, String] = op("==", Ast.CompOp.Eq)
  val GtE: core.Parser[Ast.CompOp.GtE.type, Char, String] = op(">=", Ast.CompOp.GtE)
  val LtE: core.Parser[Ast.CompOp.LtE.type, Char, String] = op("<=", Ast.CompOp.LtE)
  val NotEq: core.Parser[Ast.CompOp.NotEq.type, Char, String] = op("<>" | "!=", Ast.CompOp.NotEq)
  val In: core.Parser[Ast.CompOp.In.type, Char, String] = op("in", Ast.CompOp.In)
  val NotIn: core.Parser[Ast.CompOp.NotIn.type, Char, String] = op("not" ~ "in", Ast.CompOp.NotIn)
  val Is: core.Parser[Ast.CompOp.Is.type, Char, String] = op("is", Ast.CompOp.Is)
  val IsNot: core.Parser[Ast.CompOp.IsNot.type, Char, String] = op("is" ~ "not", Ast.CompOp.IsNot)
  val comp_op: noApi.Parser[Ast.CompOp] = P( LtE|GtE|Eq|Gt|Lt|NotEq|In|NotIn|IsNot|Is )
  val Add: core.Parser[Ast.Operator.Add.type, Char, String] = op("+", Ast.Operator.Add)
  val Sub: core.Parser[Ast.Operator.Sub.type, Char, String] = op("-", Ast.Operator.Sub)
  val Pow: core.Parser[Ast.Operator.Pow.type, Char, String] = op("**", Ast.Operator.Pow)
  val Mult: core.Parser[Ast.Operator.Mult.type, Char, String] = op("*", Ast.Operator.Mult)
  val Div: core.Parser[Ast.Operator.Div.type, Char, String] = op("/", Ast.Operator.Div)
  val Mod: core.Parser[Ast.Operator.Mod.type, Char, String] = op("%", Ast.Operator.Mod)
  val UAdd: core.Parser[Ast.UnaryOp.UAdd.type, Char, String] = op("+", Ast.UnaryOp.UAdd)
  val USub: core.Parser[Ast.UnaryOp.USub.type, Char, String] = op("-", Ast.UnaryOp.USub)
  val Invert: core.Parser[Ast.UnaryOp.Invert.type, Char, String] = op("~", Ast.UnaryOp.Invert)
  val unary_op: noApi.Parser[Ast.UnaryOp] = P ( UAdd | USub | Invert )

  def Unary(p: P[Ast.Expr]): core.Parser[Ast.Expr.Unary, Char, String] =
    (unary_op ~ p).map { case (op, operand) => Ast.Expr.Unary(op, operand) }

  def Chain(p: P[Ast.Expr], op: P[Ast.Operator]): core.Parser[Ast.Expr, Char, String] =
    P( p ~ (op ~ p).rep ).map {
      case (lhs, chunks) =>
        chunks.foldLeft(lhs) { case (lhs, (op, rhs)) =>
          Ast.Expr.Bin(lhs, op, rhs)
        }
    }
}
