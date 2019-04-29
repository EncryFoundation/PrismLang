package org.encryfoundation.prismlang.compiler

import java.nio.charset.Charset

import org.encryfoundation.prismlang.codec.PCodec
import org.encryfoundation.prismlang.core.{Ast, Types}
import scodec.bits.BitVector
import scorex.crypto.hash.Blake2b256

import scala.util.Try

case class CompiledContract(args: List[(String, Types.PType)], script: Ast.Expr) {
  lazy val bytes: Array[Byte] = CompiledContractSerializer.toBytes(this)
  lazy val hash: CompiledContract.ContractHash = Blake2b256.hash(bytes)

  override def toString: String = script.toString
}

object CompiledContract {
  type ContractHash = Array[Byte]
  def costOf(contract: CompiledContract): Int = CostEstimator
    .default.costOf(contract.script) + contract.args.map(_._2.dataCost).sum
}

object CompiledContractSerializer {

  import scodec.codecs._

  implicit val charset: Charset = Charset.defaultCharset()

  def toBytes(obj: CompiledContract): Array[Byte] = {
    val args: Array[Byte] = obj.args.foldLeft(Array.empty[Byte]) { case (acc, (name, tpe)) =>
      val nameB: Array[Byte] = string.encode(name).require.toByteArray
      val nameLen: Array[Byte] = uint8.encode(nameB.length).require.toByteArray
      val tpeB: Array[Byte] = PCodec.typeCodec.encode(tpe).require.toByteArray
      val tpeLen: Array[Byte] = uint16.encode(tpeB.length).require.toByteArray
      acc ++ nameLen ++ nameB ++ tpeLen ++ tpeB
    }
    uint8.encode(obj.args.size).require.toByteArray ++ args ++ PCodec.exprCodec.encode(obj.script).require.toByteArray
  }

  def parseBytes(bytes: Array[Byte]): Try[CompiledContract] = Try {
    val argsQty: Int = uint8.decode(BitVector(bytes.head)).require.value
    val (args: List[(String, Types.PType)], scriptBytes: Array[Byte]) =
      (1 to argsQty).foldLeft(List.empty[(String, Types.PType)], bytes.tail) { case ((acc, leftBytes), _) =>
        val nameLen: Int = uint8.decode(BitVector(leftBytes.head)).require.value
        val name: String = string.decode(BitVector(leftBytes.tail.take(nameLen))).require.value
        val tpeLen: Int = uint16.decode(BitVector(leftBytes.slice(1 + nameLen, nameLen + 3))).require.value
        val tpe: Types.PType = PCodec.typeCodec.decode(BitVector(leftBytes.slice(nameLen + 3, nameLen + 3 + tpeLen))).require.value
        (acc :+ (name -> tpe)) -> leftBytes.drop(nameLen + tpeLen + 3)
      }
    CompiledContract(args, PCodec.exprCodec.decode(BitVector(scriptBytes)).require.value)
  }
}

