package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import reflect.Manifest;
import org.scala_lang.virtualized.SourceContext
import scala.virtualization.lms.common._

import ppl.tests.scalatest._
import scala.collection.mutable.ArrayBuffer

import LOWERCASE_DSL_NAME.shared.ForgeTestModule

trait TestsOpsExp extends DeliteTestOpsExp

trait ScalaGenTestsOps extends ScalaGenDeliteTest { val IR: TestsOpsExp }
trait CudaGenTestsOps
trait OpenCLGenTestsOps
trait CGenTestsOps

trait ForgeTestRunnerCompiler extends ForgeTestModule with TestsOpsExp with DeliteTestRunner {
  // everything inherited from DeliteTestRunner
}

trait ForgeTestRunnerStandaloneCompiler extends ForgeTestModule with TestsOpsExp with DeliteTestStandaloneRunner

trait ForgeSuiteCompiler extends DeliteSuite {
  def runTest(app: ForgeTestRunnerCompiler) { compileAndTest(app) }
}
