package org.encryfoundation.prismlang.parser

import fastparse._
import org.encryfoundation.prismlang.core.Ast
import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.parser.Lexer._
import org.encryfoundation.prismlang.parser.Operations._

object Expressions {

  implicit def whitespace(cfg: P[_]): P[Unit] = Lexer.WS(cfg)

  def trueExpr[_: P]: P[Ast.Expr.True.type] = P( kwd("true").rep(min = 1, max = 1).! ).map(_ => Ast.Expr.True)
  def falseExpr[_: P]: P[Ast.Expr.False.type] = P( kwd("false").rep(min = 1, max = 1).! ).map(_ => Ast.Expr.False)

  def intConstExpr[_: P]: P[Ast.Expr.IntConst] = P( Lexer.integer ).map(Ast.Expr.IntConst)

  def NAME[_: P]: P[Ast.Ident] = Lexer.identifier
  def NUMBER[_: P]: P[Ast.Expr.IntConst] = P( intConstExpr )
  def BOOL[_: P]: P[Ast.Expr] = P( trueExpr | falseExpr )
  def STRING[_: P]: P[String] = Lexer.stringliteral
  def BASE58STRING[_: P]: P[String] = P( "base58" ~/ Lexer.stringliteral )
  def BASE16STRING[_: P]: P[String] = P( ( "base16" | "hex" ) ~/ Lexer.stringliteral )

  def spaces[_: P]: P[Unit] = P( (Semis.? ~~ "\n").repX(1) )

  // This parser is for tests only.
  def fileInput[_: P]: P[Seq[Ast.Expr]] = P( Semis.? ~ expr.repX(0, Semis) ~ Semis.? )

  def module[_: P]: P[Ast.Module] = P( Semis.? ~ struct.rep(0, Semis) ~ Semis.? ~ contract ~ Semis.? ).map { case (structsN, contractN) =>
    Ast.Module(contractN, structsN.toList)
  }

  def block[_: P]: P[Ast.Expr] = {
    def end: P[Unit] = P( Semis.? ~ "}" )
    P( "{" ~ Semis.? ~ expr.rep(min=1, sep=Semi) ~ end ).map(exps => Ast.Expr.Block(exps.toList))
  }

  def expr[_: P]: P[Ast.Expr] = P( test | comparison | arithExpr | lambdef | funcdef | constdef | ifLetExpr | ifExpr | block )
  def arithExpr[_: P]: P[Ast.Expr] = P( Chain(term, Add | Sub) )
  def term[_: P]: P[Ast.Expr] = P( Chain(factor, Mult | Div | Mod) )

  def test[_: P]: P[Ast.Expr] = P( orTest | lambdef )
  def multiline[_: P](p: => P[Unit]): P[Unit] = P( p ~ "\n".? )

  def orTest[_: P]: P[Ast.Expr] =
    P( andTest.rep(1, multiline(kwd("or")) | multiline("||")) ).map {
      case Seq(x) => x
      case xs => Ast.Expr.Bool(Ast.BooleanOp.Or, xs.toList)
    }
  def andTest[_: P]: P[Ast.Expr] = P( notTest.rep(1, multiline(kwd("and")) | multiline("&&") ) ).map {
    case Seq(x) => x
    case xs => Ast.Expr.Bool(Ast.BooleanOp.And, xs.toList)
  }
  def notTest[_: P]: P[Ast.Expr] = P( (("not" | "!") ~ notTest).map(Ast.Expr.Unary(Ast.UnaryOp.Not, _)) | comparison )

  def comparison[_: P]: P[Ast.Expr] = P( arithExpr ~ (compOp ~ arithExpr).rep ).map {
      case (lhs, Nil) => lhs
      case (lhs, chunks) =>
        val (ops, vals) = chunks.unzip
        Ast.Expr.Compare(lhs, ops.toList, vals.toList)
    }

  /** NUMBER appears here and below in `atom` to give it precedence.
    * This ensures that "-2" will parse as `Num(-2)` rather than
    * as `UnaryOp(USub, Num(2))`. */

  def factor[_: P]: P[Ast.Expr] = P(NUMBER | Unary(factor) | power )
  def power[_: P]: P[Ast.Expr] = P( atom ~ trailer.rep ~ (Pow ~ factor).? ).map {
    case (lhs, trailers, rhs) =>
      val left = trailers.foldLeft(lhs)((l, t) => t(l))
      rhs match{
        case None => left
        case Some((op, right)) => Ast.Expr.Bin(left, op, right)
      }
  }

  def listContents[_: P]: P[Seq[Ast.Expr]] = P( test.rep(1, "," ~ LineBreak.?) ~ ",".? ~ LineBreak.? )
  def list[_: P]: P[Ast.Expr.Collection] = P( listContents ).map(exps => Ast.Expr.Collection(exps.toList))
  def tupleContents[_: P]: P[Seq[Ast.Expr]] = P( test ~ "," ~ listContents.?).map { case (head, rest)  => head +: rest.getOrElse(Seq.empty) }
  def tuple[_: P]: P[Ast.Expr.Tuple] = P( tupleContents ).map(tcs => Ast.Expr.Tuple(tcs.toList))

  def atom[_: P]: P[Ast.Expr] =
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

  def exprlist[_: P]: P[Seq[Ast.Expr]] = P( expr.rep(1, sep = ",") ~ ",".? )
  def testlist[_: P]: P[Seq[Ast.Expr]] = P( test.rep(1, sep = ",") ~ ",".? )

  def subscript[_: P]: P[Ast.SliceOp] = {
    val single = P(test.map(Ast.SliceOp.Index))
    val multi = P(test.? ~ ":" ~ test.?).map { case (lower, upper) =>
      Ast.SliceOp.Slice(lower, upper)
    }
    P( multi | single )
  }

  def subscriptlist[_: P]: P[Ast.SliceOp] = P( subscript.rep(1, ",") ~ ",".? ).map {
    case Seq(x) => x
  }

  def trailer[_: P]: P[Ast.Expr => Ast.Expr] = {
    def call = P("(" ~ arglist ~ ")").map { args => (lhs: Ast.Expr) =>
      Ast.Expr.Call(lhs, args.toList)
     }
    def slice = P("[" ~ subscriptlist ~ "]").map(args => (lhs: Ast.Expr) =>
      Ast.Expr.Subscript(lhs, args))
    def attr = P("." ~ NAME).map(id => (lhs: Ast.Expr) => Ast.Expr.Attribute(lhs, id) )
    P(call | slice | attr)
  }

  def plainArgument[_: P]: P[Ast.Expr] = P( test )

  def arglist[_: P]: P[Seq[Ast.Expr]] = P( (plainArgument ~ !"=").rep(0, ",") ~ ",".? )

  def typeParams[_: P]: P[Seq[Ast.TypeIdent]] = P( "[" ~ typeIdent.rep(sep = ",") ~ "]" )

  def typeIdent[_: P]: P[Ast.TypeIdent] = P( NAME ~ typeParams.? ).map { case (tpeN, tpsOpt) =>
    Ast.TypeIdent(tpeN.name, tpsOpt.map(_.toList).getOrElse(List.empty))
  }

  def typeAnnotation[_: P]: P[Ast.TypeIdent] = P( ":" ~/ typeIdent )

  def varargslist[_: P]: P[Seq[(Ast.Ident, Ast.TypeIdent)]] = P( (NAME ~/ typeAnnotation).rep(sep = ",") ~ ",".? )

  def lambdef[_: P]: P[Ast.Expr.Lambda] = P( kwd("lamb") ~ "(" ~ varargslist ~ ")" ~ "=" ~ expr ).map { case (args, exp) =>
    Ast.Expr.Lambda(args.toList, exp)
  }

  def constdef[_: P]: P[Ast.Expr.Let] = P( kwd("let") ~ NAME ~ typeAnnotation.? ~ "=" ~ expr ).map { case (name, typeOpt, value) =>
    Ast.Expr.Let(name, value, typeOpt)
  }

  def funcdef[_: P]: P[Ast.Expr.Def] = P( kwd("def") ~ NAME ~ "(" ~ varargslist ~ ")" ~ typeAnnotation ~ "=" ~ expr )
    .map { case (name, args, retType, body) => Ast.Expr.Def(name, args.toList, body, retType) }

  def ifExpr[_: P]: P[Ast.Expr.If] = {
    def firstIf: P[(Expr, Expr)] = P( kwd("if") ~ "(" ~ test ~ ")" ~ expr)
    def lastElse: P[Expr] = P( Semi.? ~ kwd("else") ~ expr ~ Semi.?)
    val res = P( firstIf ~/ lastElse ).map { case (tst, body, elseBody) =>
      Ast.Expr.If(tst, body, elseBody)
    }
    res
  }

  def ifLetExpr[_: P]: P[Ast.Expr.IfLet] = {
    def let = P( kwd("let") ~ NAME ~ typeAnnotation ~ "=" ~ expr )
    def firstIf = P( kwd("if") ~ "(" ~ let ~ ")" ~/ expr )
    def lastElse = P( Semi.? ~ kwd("else") ~/ expr )
    P( firstIf ~ lastElse ).map { case (local, typeId, target, body, elseBody) =>
      Ast.Expr.IfLet(local, typeId, target, body, elseBody)
    }
  }

  def contract[_: P]: P[Ast.Contract] =
    P( kwd("contract") ~ "(" ~ varargslist ~ ")" ~ "=" ~ expr ).map { case (args, body) =>
      Ast.Contract(body, args.toList)
    }

  def typeDescriptorParams[_: P]: P[Seq[Ast.TypeDescriptor]] = P( "[" ~ tpe.rep(1, ",") ~ ",".? ~ "]" )

  def simpleType[_: P]: P[Ast.TypeDescriptor.SimpleType] = P( NAME ~ typeDescriptorParams.? ).map { case (tpeN, tpsOpt) =>
    Ast.TypeDescriptor.SimpleType(tpeN, tpsOpt.map(_.toList).getOrElse(List.empty))
  }

  def field[_: P]: P[Ast.TypeDescriptor.Field] = P( NAME ~ ":" ~ tpe )

  def fields[_: P]: P[Seq[Ast.TypeDescriptor.Field]] = P( Semis.? ~ field.rep(min = 1, Semis) ~ Semis.? )

  def productType[_: P]: P[Ast.TypeDescriptor.ProductType] = P( "Object" ~ "(" ~ fields ~ ")" ).map(flds => Ast.TypeDescriptor.ProductType(flds.toList))

  def tpe[_: P]: P[Ast.TypeDescriptor] = P( productType | simpleType )

  def struct[_: P]: P[Ast.Struct] = P( kwd("struct") ~ NAME ~ ":" ~ tpe ).map { case (id, tp) => Ast.Struct(id, tp) }
}
