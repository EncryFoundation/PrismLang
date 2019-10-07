package org.encryfoundation.prismlang.integration

import java.text.SimpleDateFormat
import java.util.Date

import org.scalatest.PropSpec
import scorex.crypto.encode.{Base16, Base58}
import scorex.crypto.hash.{Blake2b256, Blake2b512, Digest32, Keccak256, Keccak512, Sha256}
import scorex.utils.Random

class BuildInFuncSpec extends PropSpec with Utils {

  val bytes: Array[Byte] = Random.randomBytes()

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
    checkHash(bytes, "blake2b256", Blake2b256.hash)
  }

  property("blake2b512") {
    checkHash(bytes, "blake2b512", Blake2b512.hash)
  }

  property("keccak256") {
    checkHash(bytes, "keccak256", Keccak256.hash)
  }

  property("keccak512") {
    checkHash(bytes, "keccak512", Keccak512.hash)
  }

  property("sha256") {
    checkHash(bytes, "sha256", Sha256.hash)
  }

  property("base16") {
    checkBase(bytes, "base16", Base16.encode)
  }

  property("base58") {
    checkBase(bytes, "base58", Base58.encode)
  }

  def checkHash(bytes: Array[Byte], hashFuncName: String, hashFunc: Array[Byte] => Array[Byte]): Unit = {
    val hash = hashFunc(bytes)
    val sources = s"{ $hashFuncName(base16'${Base16.encode(bytes)}') }"

    testCompiledExpressionWithOptionalEvaluation(sources, compilationSuccess = true, Some(true), Some(hash))
  }

  def checkBase(bytes: Array[Byte], funcName: String, func: Array[Byte] => String): Unit = {
    val baseStr = func(bytes)
    val sources = s"{ $funcName'$baseStr' }"

    testCompiledExpressionWithOptionalEvaluation(sources, compilationSuccess = true, Some(true), Some(bytes))
  }
}
