package org.encryfoundation.prismlang.parser

import fastparse.{core, noApi}
import fastparse.noApi._
import org.encryfoundation.prismlang.core.Ast
import org.encryfoundation.prismlang.parser.WsApi._

object Expressions {

  import org.encryfoundation.prismlang.parser.Lexer._
  import org.encryfoundation.prismlang.parser.Operations._

  val trueExpr: P[Ast.Expr.True.type] = P( kwd("true").rep(min = 1, max = 1).! ).map(_ => Ast.Expr.True)
  val falseExpr: P[Ast.Expr.False.type] = P( kwd("false").rep(min = 1, max = 1).! ).map(_ => Ast.Expr.False)

  val intConstExpr: P[Ast.Expr.IntConst] = P( Lexer.integer ).map(Ast.Expr.IntConst)

  val NAME: P[Ast.Ident] = Lexer.identifier
  val NUMBER: P[Ast.Expr.IntConst] = P( intConstExpr )
  val BOOL: P[Ast.Expr] = P( trueExpr | falseExpr )
  val STRING: P[String] = Lexer.stringliteral
  val BASE58STRING: P[String] = P( "base58" ~/ Lexer.stringliteral )
  val BASE16STRING: P[String] = P( "base16" ~/ Lexer.stringliteral )

  val spaces: P[Unit] = P( (Semis.? ~~ "\n").repX(1) )

  // This parser is for tests only.
  def fileInput: P[Seq[Ast.Expr]] = P( Semis.? ~ expr.repX(0, Semis) ~ Semis.? )

  def fileInputContract: P[Ast.Expr.Contract] = P( Semis.? ~ contract ~ Semis.? )

  val block: P[Ast.Expr] = {
    val end: noApi.Parser[Unit] = P( Semis.? ~ "}" )
    P( "{" ~ Semis.? ~ expr.rep(min=0, sep=Semi) ~ end ).map(exps => Ast.Expr.Block(exps.toList))
  }

  def expr: P[Ast.Expr] = P( arithExpr | test | lambdef | funcdef | constdef | ifExpr | block )
  def arithExpr: P[Ast.Expr] = P( Chain(term, Add | Sub) )
  def term: P[Ast.Expr] = P( Chain(factor, Mult | Div | Mod) )

  val test: P[Ast.Expr] = {
    val ternary = P(orTest ~ (kwd("if") ~ orTest ~ kwd("else") ~ test).?).map {
      case (x, None) => x
      case (x, Some((t, neg))) => ??? // if?
    }
    P( ternary )
  }
  val orTest: core.Parser[Ast.Expr, Char, String] = P( andTest.rep(1, kwd("or") | "||") ).map {
    case Seq(x) => x
    case xs => Ast.Expr.Bool(Ast.BooleanOp.Or, xs.toList)
  }
  val andTest: core.Parser[Ast.Expr, Char, String] = P( notTest.rep(1, kwd("and") | "&&") ).map {
    case Seq(x) => x
    case xs => Ast.Expr.Bool(Ast.BooleanOp.And, xs.toList)
  }
  val notTest: P[Ast.Expr] = P( (("not" | "!") ~ notTest).map(Ast.Expr.Unary(Ast.UnaryOp.Not, _)) | comparison )

  val comparison: P[Ast.Expr] = P( arithExpr ~ (comp_op ~ arithExpr).rep ).map {
    case (lhs, Nil) => lhs
    case (lhs, chunks) =>
      val (ops, vals) = chunks.unzip
      Ast.Expr.Compare(lhs, ops.toList, vals.toList)
  }

  /** NUMBER appears here and below in `atom` to give it precedence.
    * This ensures that "-2" will parse as `Num(-2)` rather than
    * as `UnaryOp(USub, Num(2))`. */
  val factor: P[Ast.Expr] = P( NUMBER | Unary(factor) | power )
  val power: P[Ast.Expr] = P( atom ~ trailer.rep ~ (Pow ~ factor).? ).map {
    case (lhs, trailers, rhs) =>
      val left = trailers.foldLeft(lhs)((l, t) => t(l))
      rhs match{
        case None => left
        case Some((op, right)) => Ast.Expr.Bin(left, op, right)
      }
  }
  val atom: P[Ast.Expr] = {
//    val empty_tuple = ("(" ~ ")").map(_ => Ast.EXPR.ESTuple(Nil))
//    val empty_list = ("[" ~ "]").map(_ => Ast.EXPR.ESList(Nil))
//    val empty_dict = ("{" ~ "}").map(_ => Ast.EXPR.ESDictNode(Nil, Nil))
    P(
//      empty_tuple  |
//        empty_list |
//        empty_dict |
//        "(" ~ (tuple | test) ~ ")" |
//        "[" ~ list ~ "]" |
//        "{" ~ dictorsetmaker ~ "}" |
        BASE58STRING.rep(1).map(_.mkString).map(Ast.Expr.Base58Str) |
        STRING.rep(1).map(_.mkString).map(Ast.Expr.Str) |
        NAME.map(Ast.Expr.Name) |
        NUMBER |
        BOOL
    )
  }

  val sliceop: noApi.Parser[Option[Ast.Expr]] = P(":" ~ test.?)
  val exprlist: P[Seq[Ast.Expr]] = P( expr.rep(1, sep = ",") ~ ",".? )
  val testlist: P[Seq[Ast.Expr]] = P( test.rep(1, sep = ",") ~ ",".? )

  val subscript: P[Ast.SliceOp] = {
    val ellipses = P(("." ~ "." ~ ".").map(_ => Ast.SliceOp.Ellipsis))
    val single = P(test.map(Ast.SliceOp.Index))
    val multi = P(test.? ~ ":" ~ test.? ~ sliceop.?).map { case (lower, upper, step) =>
      Ast.SliceOp.Slice(
        lower,
        upper,
        step.map(_.getOrElse(Ast.Expr.Name(Ast.Ident("None"))))
      )
    }
    P( ellipses | multi | single )
  }

  val subscriptlist: core.Parser[Ast.SliceOp, Char, String] = P( subscript.rep(1, ",") ~ ",".? ).map {
    case Seq(x) => x
    case xs => Ast.SliceOp.ExtSlice(xs.toList)
  }

  val trailer: P[Ast.Expr => Ast.Expr] = {
    val call = P("(" ~ arglist ~ ")").map { args => (lhs: Ast.Expr) => Ast.Expr.Call(lhs, args.toList) }
    val slice = P("[" ~ subscriptlist ~ "]").map(args => (lhs: Ast.Expr) => Ast.Expr.Subscript(lhs, args))
    val attr = P("." ~ NAME).map(id => (lhs: Ast.Expr) => Ast.Expr.Attribute(lhs, id))
    P(call | slice | attr)
  }

  val plainArgument: core.Parser[Ast.Expr, Char, String] = P( test )

  val arglist: noApi.Parser[Seq[Ast.Expr]] = {
    val inits = P( (plainArgument ~ !"=").rep(0, ",") )
    P( inits ~ ",".? )
  }

  val typeParams: P[Seq[String]] = P( "[" ~ NAME.rep(sep = ",") ~ "]" ).map(_.map(_.name))

  val typeIdent: P[Ast.TypeIdent] = P( NAME ~ typeParams.? ).map { case (tpeN, tpsOpt) =>
    Ast.TypeIdent(tpeN.name, tpsOpt.map(_.toList).getOrElse(List.empty))
  }

  val typeAnnotation: P[Ast.TypeIdent] = P( ":" ~/ typeIdent )

  val varargslist: P[Seq[(Ast.Ident, Ast.TypeIdent)]] = P( (NAME ~/ typeAnnotation).rep(sep = ",") ~ ",".? )

  val lambdef: P[Ast.Expr.Lambda] = P( kwd("lamb") ~ "(" ~ varargslist ~ ")" ~ "=" ~ expr ).map { case (args, exp) =>
    Ast.Expr.Lambda(args.toList, exp)
  }

  val constdef: P[Ast.Expr.Let] = P( kwd("let") ~ NAME ~ typeAnnotation.? ~ "=" ~ expr ).map { case (name, typeOpt, value) =>
    Ast.Expr.Let(name, value, typeOpt)
  }

  val funcdef: P[Ast.Expr.Def] = P( kwd("def") ~ NAME ~ "(" ~ varargslist ~ ")" ~ typeAnnotation ~ "=" ~ expr )
    .map { case (name, args, retType, body) => Ast.Expr.Def(name, args.toList, body, retType) }

  val ifExpr: P[Ast.Expr.If] = {
    val firstIf = P( kwd("if") ~ "(" ~ test ~ ")" ~/ expr )
    val lastElse = P( Semi.? ~ kwd("else") ~/ expr )
    P( firstIf ~ lastElse ).map { case (tst, body, elseBody) =>
      Ast.Expr.If(tst, body, List(elseBody))
    }
  }

  val contract: P[Ast.Expr.Contract] = P( kwd("contract") ~ "(" ~ varargslist ~ ")" ~ "=" ~ expr ).map { case (args, body) =>
    Ast.Expr.Contract(body, args.toList)
  }
}
