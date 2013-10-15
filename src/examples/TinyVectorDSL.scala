package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object TinyVectorDSLRunner extends ForgeApplicationRunner with TinyVectorDSL

/**
 * This DSL uses Forge's alpha support as an identity generator to generate
 * skeleton Forge specifications from existing Scala classes using reflection.
 */
trait TinyVectorDSL extends ForgeApplication {
  def dslName = "TinyVector"

  def specification() = {
  	importScalaOps()
    importAuto[TVector]
  }
}

class TVector {
  def add(v: TVector): TVector = this
  def add(v: Int): TVector = this
}
