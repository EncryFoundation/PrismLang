package org.encryfoundation.prismlang

import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.core.Ast.Expr.{Base16Str, Base58Str, ByteConst, False, IntConst, Str, True}
import org.encryfoundation.prismlang.integration.Utils
import scorex.utils.{Random => ScorexRandom}

import scala.util.Random

object ValueGenerator extends Utils {

  def genRandomValue(valueTypeName: String): (Expr, Any) = valueTypeName match {
    case "Str" =>
      val s = generateRandomString(Random.nextInt(100))
      (Str(s), s)
    case "IntConst" =>
      val i = Random.nextInt()
      (IntConst(i), i)
    case "ByteConst" =>
      val b = Random.nextInt().toByte
      (ByteConst(b), b)
    case "Base16Str" =>
      val b = ScorexRandom.randomBytes()
      val s = scorex.crypto.encode.Base16.encode(b)
      (Base16Str(s), b.toList)
    case "Base58Str" =>
      val b = ScorexRandom.randomBytes()
      val s = scorex.crypto.encode.Base58.encode(b)
      (Base58Str(s), b.toList)
    case "Bool" =>
      val bool = Random.nextBoolean()
      (if(bool) True else False, bool)
  }

  def genValueTypeList(valueTypes: List[String], count: Int): List[String] =
    (0 until count).foldLeft(List[String]()) { (acc, n) => acc :+ valueTypes(n % valueTypes.size) }

  def genValues(valueTypes: List[String], count: Int): (List[Expr], List[Any]) =
    ValueGenerator.genValueTypeList(valueTypes, count)
    .map(ValueGenerator.genRandomValue)
    .unzip

}
