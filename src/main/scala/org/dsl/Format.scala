package org.dsl

import scala.util.{Try, Success, Failure}
import scala.quoted._
import org.scalafmt.cli._
import scala.util.matching.Regex
import java.io.File
import metaconfig._, Configured._
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.{ScalafmtRunner => SRunner}
import org.scalafmt.util.LoggerOps._

object Format {

  def apply(code: => String) = {

    val codeValue =
      try { code }
      catch {
        case e: Throwable => "/* [INVALID CODE] */"
      }

    val encosedCode =
      s"""|object DummyEnclosure {
          |  ${codeValue}
          |}""".stripMargin

    // NOTE: Very ineffifient way to get rid of DummyEnclosure on large blocks of code
    //       use only for debugging purposes!
    def unEnclose(enclosedCode: String) =
      val lines =
        enclosedCode
          .replaceFirst("^object DummyEnclosure \\{", "")
          .replaceFirst("\\}", "")
          .split("\n")
      val linesTrimmedFirst = if (lines.head == "") lines.drop(1) else lines
      // if there was a \n} on the last line, remove the }
      val linesTrimmedLast =
        if (linesTrimmedFirst.last == "") linesTrimmedFirst.dropRight(1) else linesTrimmedFirst
      // then if all lines had at least one indent i.e. "  " remove that
      if (linesTrimmedLast.forall(line => line.startsWith("  ")))
        linesTrimmedLast.map(line => line.replaceFirst("  ", "")).mkString("\n")
      else
        linesTrimmedLast.mkString("\n")

    extension [T](t: Try[T])
      def toOptionMsg =
        t match
          case Success(v) => Some(v)
          case Failure(e) => None

    val formattedCode =
      for {
        formatted <- Try { /* println("formatting"); */
          formatInternal(encosedCode)
        }.toOptionMsg
      } yield String.valueOf(formatted) /* null safe way of doing .toString in scala) */

    formattedCode.map(code => unEnclose(code)).getOrElse(codeValue)
  }

  val oldConfig: Regex = "--".r
  // Not really needed, just so that the cache can be normally populated
  val filename = "Main.scala"

  def formatInternal(code: String): String = {
    val style = ScalafmtConfig.default
    Scalafmt.format(code, style, Set.empty, "<input>") match {
      case Formatted.Success(formattedCode) =>
        formattedCode
      case Formatted.Failure(e) =>
        println(
          s"""===== Failed to format the code ====
             |$code
             |---
             |Cause: ${e.getMessage}.
             |""".stripMargin
        )
        code
    }
  }
}
