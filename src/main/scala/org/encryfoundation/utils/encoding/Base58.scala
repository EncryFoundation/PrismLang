package org.encryfoundation.utils.encoding

import scala.annotation.tailrec
import scala.util.{Success, Try}

object Base58 {

  val Alphabet: String = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  private val Base58Size: Int = Alphabet.length

  private def index(char: Char): Option[Int] = char match {
    case c if c <= '9' && c >= '1' => Some(c - '1')
    case c if c <= 'k' && c >= 'a' => Some(c - 'a' + 33)
    case c if c <= 'z' && c >= 'm' => Some(c - 'm' + 44)
    case c if c >= 'A' && c <= 'H' => Some(c - 'A' + 9)
    case c if c >= 'J' && c <= 'N' => Some(c - 'J' + 17)
    case c if c >= 'P' && c <= 'Z' => Some(c - 'P' + 22)
    case _ => None
  }

  def encode(input: Array[Byte]): String =
    if (input.isEmpty) ""
    else {
      val bi: BigInt = BigInt(1, input)
      val s: StringBuilder = new StringBuilder

      @tailrec
      def append(rest: BigInt): Unit = {
        val div: BigInt = rest / Base58Size
        val mod: BigInt = rest % Base58Size
        s.insert(0, Alphabet(mod.intValue))
        if (div > 0) append(div)
      }

      append(bi)

      val zeros: Int = input.indexWhere(_ != 0)
      0 until zeros foreach { _ => s.insert(0, Alphabet(0)) }

      s.toString
    }

  def decode(input: String): Try[Array[Byte]] =
    if (input.isEmpty) Success(Array.empty[Byte])
    else {
      def toBytes: String => Try[Array[Byte]] = (in: String) => Try {
        val size: Int = in.length
        in.zipWithIndex.foldRight(BigInt(0)) { (c, bi) =>
          index(c._1).map { i =>
            bi + (BigInt(i) * BigInt(Base58Size).pow(size - 1 - c._2))
          }.getOrElse(throw InvalidCharacterException(c._1, c._2))
        }.toByteArray.dropWhile(_ == 0)
      }

      val (z: String, in) = input.span(_ == Alphabet.head)
      val zeros: Array[Byte] = z.map(_ => 0: Byte).toArray

      if (in.isEmpty) Success(zeros)
      else toBytes(in).map { bytes => zeros ++ bytes }
    }

  case class InvalidCharacterException(char: Char, index: Int) extends IllegalArgumentException(
      s"An invalid character ($char)) at index $index")
}
