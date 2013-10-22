package ppl.dsl.forge
package templates
package shallow

import java.io.PrintWriter
import core._

trait ShallowGenImports extends ForgeCodeGenBase {
  val IR: ForgeApplicationRunner with ForgeExp
  import IR._

  def emitForgeLibraryImports(stream: PrintWriter) {
    stream.println("import ch.epfl.lamp.autolifter.library._")
    stream.println("import ch.epfl.lamp.autolifter.annotations._")
    stream.println("import ForgeArray._")
    stream.println("import ForgeArrayBuffer._")
    stream.println("import Numeric._")
  }

  def emitScalaMathImports(stream: PrintWriter) {
    stream.println("import scala.math.Ordering.Implicits._")
    stream.println("import scala.math.Numeric.Implicits._")
  }

  // def emitScalaIOImports(stream: PrintWriter) {
  //   stream.println("import java.io.{BufferedWriter, FileWriter, PrintWriter}")
  // }

  def emitScalaReflectImports(stream: PrintWriter) {
    stream.println("import scala.tools.nsc.io._")
    stream.println("import scala.reflect.{Manifest,SourceContext}")
  }

  // def emitScalaImports(stream: PrintWriter) {
  //   emitScalaIOImports(stream)
  //   emitScalaReflectImports(stream)
  // }

  // def emitLMSImports(stream: PrintWriter) {
  //   stream.println("import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}")
  //   stream.println("import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat,CudaGenFat,CGenFat}")
  //   stream.println("import scala.virtualization.lms.util._")
  //   stream.println("import scala.virtualization.lms.internal._")
  // }

  // def emitDSLImports(stream: PrintWriter) {
  //   // stream.println("import " + dsl.toLowerCase() + "._")
  //   stream.println("import " + dsl.toLowerCase() + ".shared._")
  //   stream.println("import " + dsl.toLowerCase() + ".shared.ops._")
  //   if (OpsGrp.exists(t => isTpeClass(t._1))) {
  //     stream.println("import " + dsl.toLowerCase() + ".shared.typeclass._")
  //   }
  //   // stream.println("import " + dsl.toLowerCase() + ".shared.extern._")
  // }

  def emitAllImports(stream: PrintWriter) {
    // emitScalaImports(stream)
    // emitLMSImports(stream)
    // emitDSLImports(stream)
    emitScalaMathImports(stream)
    emitForgeLibraryImports(stream)
  }
}
