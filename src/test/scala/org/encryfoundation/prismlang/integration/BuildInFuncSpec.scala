package org.encryfoundation.prismlang.integration

import java.text.SimpleDateFormat
import java.util.Date

import org.scalatest.PropSpec
import scorex.crypto.encode.{Base16, Base58}
import scorex.crypto.hash.{Blake2b256, Blake2b512, Digest, Digest32, Keccak256, Keccak512, Sha256}
import scorex.utils.{Random => ScorexRandom}

import scala.util.{Random, Try}

class BuildInFuncSpec extends PropSpec with Utils {

  val bytes: Array[Byte] = ScorexRandom.randomBytes()

  property("unixTime") {
    val sdf = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
    val timeStr = sdf.format(new Date)
    val timestamp = sdf.parse(timeStr).getTime

    val sources =
      s"""
         |  {
         |    unixTime("$timeStr")
         |  }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(sources, compilationSuccess = true, Some(true), Some(timestamp))
  }

  property("blake2b256") {
    checkHashFunc(bytes, "blake2b256", Blake2b256.hash)
  }

  property("blake2b512") {
    checkHashFunc(bytes, "blake2b512", Blake2b512.hash)
  }

  property("keccak256") {
    checkHashFunc(bytes, "keccak256", Keccak256.hash)
  }

  property("keccak512") {
    checkHashFunc(bytes, "keccak512", Keccak512.hash)
  }

  property("sha256") {
    checkHashFunc(bytes, "sha256", Sha256.hash)
  }

  property("Min") {
    checkBinaryFunc("min", Math.min)
  }

  property("Max") {
    checkBinaryFunc("max", Math.max)
  }

  property("base16") {
    checkBaseDecodeFunc(Base16.encode(bytes), "base16", Base16.decode)
  }

  property("base58") {
    checkBaseDecodeFunc(Base58.encode(bytes), "base58", Base58.decode)
  }

  property("encode16") {
    checkBaseEncodeFunc(bytes, "encode16", Base16.encode)
  }

  property("encode58") {
    checkBaseEncodeFunc(bytes, "encode58", Base58.encode)
  }

  def checkHashFunc(bytes: Array[Byte], funcName: String, checkFunc: Array[Byte] => Digest): Unit = {
    val hash = checkFunc(bytes)
    val sources = s"{ $funcName(base16'${Base16.encode(bytes)}') }"

    testCompiledExpressionWithOptionalEvaluation(sources, compilationSuccess = true, Some(true), Some(hash))
  }

  def checkBinaryFunc(funcName: String, checkFunc: (Long, Long) => Long): Unit = {
    val a = Random.nextLong
    val b = Random.nextLong
    val sources = s"{ $funcName($a, $b) }"

    testCompiledExpressionWithOptionalEvaluation(sources, compilationSuccess = true, Some(true), Some(checkFunc(a, b)))
  }

  def checkBaseDecodeFunc(str: String, funcName: String, checkFunc: String => Try[Array[Byte]]): Unit = {
    val sources = s"{ $funcName'$str' }"

    testCompiledExpressionWithOptionalEvaluation(sources, compilationSuccess = true, Some(true), Some(checkFunc(str).get))
  }

  def checkBaseEncodeFunc(bytes: Array[Byte], funcName: String, checkFunc: Array[Byte] => String): Unit = {
    val sources =
      s"""
         {
           let b: Array[Byte] = Array(${bytes.mkString(",")})
           $funcName(b)
         }
      """.stripMargin
    testCompiledExpressionWithOptionalEvaluation(sources, compilationSuccess = true, Some(true), Some(checkFunc(bytes)))
  }

}
