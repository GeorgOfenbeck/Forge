package autooptila.shared.ops

import scala.tools.nsc.io._
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}
import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat}
import scala.virtualization.lms.util._
import scala.virtualization.lms.internal._
import autooptila.shared._
import autooptila.shared.ops._
import autooptila.shared.typeclass._

trait LAPACKOps {
  lazy val useLAPACK = System.getProperty("optila.use.lapack", "true").toBoolean
}
trait LAPACKCompilerOps extends LAPACKOps
