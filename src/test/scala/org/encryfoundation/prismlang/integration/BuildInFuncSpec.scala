package org.encryfoundation.prismlang.integration

import java.text.SimpleDateFormat
import java.util.Date

import org.scalatest.PropSpec
import scorex.crypto.encode.Base16
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.utils.Random

class BuildInFuncSpec extends PropSpec with Utils {

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

    val bytes: Array[Byte] = Random.randomBytes()
    val hash: Digest32 = Blake2b256.hash(bytes)

    val sources =
      s"""
         |  {
         |    let b = base16'${Base16.encode(bytes)}'
         |    blake2b256(b)
         |  }
      """.stripMargin

    testCompiledExpressionWithOptionalEvaluation(sources, compilationSuccess = true, Some(true), Some(hash))
  }

}
