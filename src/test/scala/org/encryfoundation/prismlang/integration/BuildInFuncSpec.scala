package org.encryfoundation.prismlang.integration

import java.text.SimpleDateFormat
import java.util.Date

import org.scalatest.PropSpec

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

}
