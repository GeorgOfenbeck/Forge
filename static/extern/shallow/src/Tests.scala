package optiql.shallow

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

import org.scalatest._
import java.io._
import scala.collection.mutable.ArrayBuffer

trait ForgeTestRunnerShallow {
  
  def main(): Unit

  val collector: ArrayBuffer[Boolean] = new ArrayBuffer[Boolean](0)

  def collect(s: Boolean) = { collector += s }

  def mkReportRepless(): Unit = { () }
}

trait ForgeSuiteShallow extends Suite {
  val verbose = System.getProperty("tests.verbose", "false").toBoolean

  def runTest(app: ForgeTestRunnerShallow) {
    app.main()

    // ignore the report file and read the collector directly
    if (verbose) println("==== " + app.getClass.getSimpleName + " ====")
    val results = app.collector
    for (i <- 0 until results.length) {
      val b = results(i)
      if (verbose) print("  condition " + i + ": ")
      if (verbose)
        if (b) println("PASSED") else println("FAILED")
      assert(b)
    }
  }
}
