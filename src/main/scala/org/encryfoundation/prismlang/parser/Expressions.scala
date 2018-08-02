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
  val BASE16STRING: P[String] = P( ( "base16" | "hex" ) ~/ Lexer.stringliteral )

  val spaces: P[Unit] = P( (Semis.? ~~ "\n").repX(1) )

  // This parser is for tests only.
  def fileInput: P[Seq[Ast.Expr]] = P( Semis.? ~ expr.repX(0, Semis) ~ Semis.? )

  def module: P[Ast.Module] = P( Semis.? ~ struct.rep(0, Semis) ~ Semis.? ~ contract ~ Semis.? ).map { case (structsN, contractN) =>
    Ast.Module(contractN, structsN.toList)
  }

  val block: P[Ast.Expr] = {
    val end: noApi.Parser[Unit] = P( Semis.? ~ "}" )
    P( "{" ~ Semis.? ~ expr.rep(min=1, sep=Semi) ~ end ).map(exps => Ast.Expr.Block(exps.toList))
  }

  def expr: P[Ast.Expr] = P( test | comparison | arithExpr | lambdef | funcdef | constdef | ifLetExpr | ifExpr | block )
  def arithExpr: P[Ast.Expr] = P( Chain(term, Add | Sub) )
  def term: P[Ast.Expr] = P( Chain(factor, Mult | Div | Mod) )

  def test: P[Ast.Expr] = P( orTest | lambdef )
  def multiline(p: P[Unit]): P[Unit] = P( p ~ "\n".? )
  val orTest: core.Parser[Ast.Expr, Char, String] = P( andTest.rep(1, multiline(kwd("or")) | multiline("||")) ).map {
    case Seq(x) => x
    case xs => Ast.Expr.Bool(Ast.BooleanOp.Or, xs.toList)
  }
  lazy val andTest: core.Parser[Ast.Expr, Char, String] = P( notTest.rep(1, multiline(kwd("and")) | multiline("&&")) ).map {
    case Seq(x) => x
    case xs => Ast.Expr.Bool(Ast.BooleanOp.And, xs.toList)
  }
  val notTest: P[Ast.Expr] = P( (("not" | "!") ~ notTest).map(Ast.Expr.Unary(Ast.UnaryOp.Not, _)) | comparison )

  lazy val comparison: P[Ast.Expr] = P( arithExpr ~ (compOp ~ arithExpr).rep ).map {
    case (lhs, Nil) => lhs
    case (lhs, chunks) =>
      val (ops, vals) = chunks.unzip
      Ast.Expr.Compare(lhs, ops.toList, vals.toList)
  }

  /** NUMBER appears here and below in `atom` to give it precedence.
    * This ensures that "-2" will parse as `Num(-2)` rather than
    * as `UnaryOp(USub, Num(2))`. */
  val factor: P[Ast.Expr] = P( NUMBER | Unary(factor) | power )
  lazy val power: P[Ast.Expr] = P( atom ~ trailer.rep ~ (Pow ~ factor).? ).map {
    case (lhs, trailers, rhs) =>
      val left = trailers.foldLeft(lhs)((l, t) => t(l))
      rhs match{
        case None => left
        case Some((op, right)) => Ast.Expr.Bin(left, op, right)
      }
  }

  val listContents: noApi.Parser[Seq[Ast.Expr]] = P( test.rep(1, "," ~ LineBreak.?) ~ ",".? ~ LineBreak.? )
  val list: core.Parser[Ast.Expr.Collection, Char, String] = P( listContents ).map(exps => Ast.Expr.Collection(exps.toList))
  val tupleContents: core.Parser[Seq[Ast.Expr], Char, String] = P( test ~ "," ~ listContents.?).map { case (head, rest)  => head +: rest.getOrElse(Seq.empty) }
  val tuple: core.Parser[Ast.Expr.Tuple, Char, String] = P( tupleContents ).map(tcs => Ast.Expr.Tuple(tcs.toList))

  val atom: P[Ast.Expr] = {
    P(
        "(" ~ LineBreak.? ~ (tuple | expr) ~ ")" |
        "Array(" ~ LineBreak.? ~ list ~ ")" |
        BASE58STRING.rep(1).map(_.mkString).map(Ast.Expr.Base58Str) |
        BASE16STRING.rep(1).map(_.mkString).map(Ast.Expr.Base16Str) |
        STRING.rep(1).map(_.mkString).map(Ast.Expr.Str) |
        NAME.map(n => Ast.Expr.Name(n)) |
        NUMBER |
        BOOL
    )
  }

  val exprlist: P[Seq[Ast.Expr]] = P( expr.rep(1, sep = ",") ~ ",".? )
  val testlist: P[Seq[Ast.Expr]] = P( test.rep(1, sep = ",") ~ ",".? )

  val subscript: P[Ast.SliceOp] = {
    val single = P(test.map(Ast.SliceOp.Index))
    val multi = P(test.? ~ ":" ~ test.?).map { case (lower, upper) =>
      Ast.SliceOp.Slice(lower, upper)
    }
    P( multi | single )
  }

  val subscriptlist: core.Parser[Ast.SliceOp, Char, String] = P( subscript.rep(1, ",") ~ ",".? ).map {
    case Seq(x) => x
  }

  val trailer: P[Ast.Expr => Ast.Expr] = {
    val call = P("(" ~ arglist ~ ")").map { args => (lhs: Ast.Expr) => Ast.Expr.Call(lhs, args.toList) }
    val slice = P("[" ~ subscriptlist ~ "]").map(args => (lhs: Ast.Expr) => Ast.Expr.Subscript(lhs, args))
    val attr = P("." ~ NAME).map(id => (lhs: Ast.Expr) => Ast.Expr.Attribute(lhs, id))
    P(call | slice | attr)
  }

  val plainArgument: core.Parser[Ast.Expr, Char, String] = P( test )

  val arglist: noApi.Parser[Seq[Ast.Expr]] = P( (plainArgument ~ !"=").rep(0, ",") ~ ",".? )

  def typeParams: P[Seq[Ast.TypeIdent]] = P( "[" ~ typeIdent.rep(sep = ",") ~ "]" )

  def typeIdent: P[Ast.TypeIdent] = P( NAME ~ typeParams.? ).map { case (tpeN, tpsOpt) =>
    Ast.TypeIdent(tpeN.name, tpsOpt.map(_.toList).getOrElse(List.empty))
  }

  val typeAnnotation: P[Ast.TypeIdent] = P( ":" ~/ typeIdent )

  val varargslist: P[Seq[(Ast.Ident, Ast.TypeIdent)]] = P( (NAME ~/ typeAnnotation).rep(sep = ",") ~ ",".? )

  lazy val lambdef: P[Ast.Expr.Lambda] = P( kwd("lamb") ~ "(" ~ varargslist ~ ")" ~ "=" ~ expr ).map { case (args, exp) =>
    Ast.Expr.Lambda(args.toList, exp)
  }

  lazy val constdef: P[Ast.Expr.Let] = P( kwd("let") ~ NAME ~ typeAnnotation.? ~ "=" ~ expr ).map { case (name, typeOpt, value) =>
    Ast.Expr.Let(name, value, typeOpt)
  }

  lazy val funcdef: P[Ast.Expr.Def] = P( kwd("def") ~ NAME ~ "(" ~ varargslist ~ ")" ~ typeAnnotation ~ "=" ~ expr )
    .map { case (name, args, retType, body) => Ast.Expr.Def(name, args.toList, body, retType) }

  val ifExpr: P[Ast.Expr.If] = {
    val firstIf = P( kwd("if") ~ "(" ~ test ~ ")" ~/ expr )
    val lastElse = P( Semi.? ~ kwd("else") ~/ expr )
    P( firstIf ~ lastElse ).map { case (tst, body, elseBody) =>
      Ast.Expr.If(tst, body, elseBody)
    }
  }

  val ifLetExpr: P[Ast.Expr.IfLet] = {
    val let: noApi.Parser[(Ast.Ident, Ast.TypeIdent, Ast.Expr)] = P( kwd("let") ~ NAME ~ typeAnnotation ~ "=" ~ expr )
    val firstIf = P( kwd("if") ~ "(" ~ let ~ ")" ~/ expr )
    val lastElse = P( Semi.? ~ kwd("else") ~/ expr )
    P( firstIf ~ lastElse ).map { case (local, typeId, target, body, elseBody) =>
      Ast.Expr.IfLet(local, typeId, target, body, elseBody)
    }
  }

  val contract: P[Ast.Contract] = P( kwd("contract") ~ "(" ~ varargslist ~ ")" ~ "=" ~ expr ).map { case (args, body) =>
    Ast.Contract(body, args.toList)
  }

  def typeDescriptorParams: P[Seq[Ast.TypeDescriptor]] = P( "[" ~ tpe.rep(1, ",") ~ ",".? ~ "]" )

  def simpleType: P[Ast.TypeDescriptor.SimpleType] = P( NAME ~ typeDescriptorParams.? ).map { case (tpeN, tpsOpt) =>
    Ast.TypeDescriptor.SimpleType(tpeN, tpsOpt.map(_.toList).getOrElse(List.empty))
  }

  def field: P[Ast.TypeDescriptor.Field] = P( NAME ~ ":" ~ tpe )

  def fields: P[Seq[Ast.TypeDescriptor.Field]] = P( Semis.? ~ field.rep(min = 1, Semis) ~ Semis.? )

  def productType: P[Ast.TypeDescriptor.ProductType] = P( "Object" ~ "(" ~ fields ~ ")" ).map(flds => Ast.TypeDescriptor.ProductType(flds.toList))

  def tpe: P[Ast.TypeDescriptor] = P( productType | simpleType )

  def struct: P[Ast.Struct] = P( kwd("struct") ~ NAME ~ ":" ~ tpe ).map { case (id, tp) => Ast.Struct(id, tp) }
}
