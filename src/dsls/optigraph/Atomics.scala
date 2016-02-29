/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: A simple container for an atomic integer array.
Used as a bitmap for BFS (could be optimized further).
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package	dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait AtomicIntArrayOps {
  	this: OptiGraphDSL =>
  	def importAtomicIntArrayOps() {
  		val AtomicIntArray = grp("AtomicIntArray")
    	val AArray = ephemeralTpe("java.util.concurrent.atomic.AtomicIntegerArray")
    	static (AtomicIntArray) ("apply", Nil, MInt :: AArray, effect=mutable) implements codegen($cala, {
      val arg1 = quotedArg(0)
      s"""new java.util.concurrent.atomic.AtomicIntegerArray($arg1)"""
    })
    	direct (AtomicIntArray) ("testAtomic", Nil, (AArray,MInt,MInt) :: MBoolean) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      val arg3 = quotedArg(2)
      s"""get($arg1,$arg2)==$arg3"""
    }
    	direct (AtomicIntArray) ("get", Nil, (AArray,MInt) :: MInt) implements codegen($cala, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1.get($arg2)"""
      })
      direct (AtomicIntArray) ("getAndAdd", Nil, (AArray,MInt,MInt) :: MInt, effect = write(0)) implements codegen($cala, {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  val arg3 = quotedArg(2)
  s"""$arg1.getAndAdd($arg2,$arg3)"""
})
	    direct (AtomicIntArray) ("testAndSetAtomic", Nil, (AArray,MInt,MInt,MInt) :: MBoolean, effect=write(0)) implements codegen($cala, {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  val arg3 = quotedArg(2)
  val arg4 = quotedArg(3)
  s"""$arg1.compareAndSet($arg2,$arg3,$arg4)"""
})
	    direct (AtomicIntArray) ("set", Nil, (AArray,MInt,MInt) :: MUnit, effect=write(0)) implements codegen($cala, {
    val arg1 = quotedArg(0)
    val arg2 = quotedArg(1)
    val arg3 = quotedArg(2)
    s"""$arg1.set($arg2,$arg3)"""
  })
  }
}

trait AtomicDoubleArrayOps {
    this: OptiGraphDSL =>
    def importAtomicDoubleArrayOps() {
      val AtomicDoubleArray = grp("AtomicDoubleArray")
      val AArray = ephemeralTpe("com.google.common.util.concurrent.AtomicDoubleArray")
      static (AtomicDoubleArray) ("apply", Nil, MInt :: AArray, effect=mutable) implements codegen($cala, {
        val arg1 = quotedArg(0)
        s"""new com.google.common.util.concurrent.AtomicDoubleArray($arg1)"""
      })
      direct (AtomicDoubleArray) ("get", Nil, (AArray,MInt) :: MDouble) implements codegen($cala, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1.get($arg2)"""
      })
      direct (AtomicDoubleArray) ("getAndAdd", Nil, (AArray,MInt,MDouble) :: MDouble, effect = write(0)) implements codegen($cala, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        val arg3 = quotedArg(2)
        s"""$arg1.getAndAdd($arg2,$arg3)"""
      })
      direct (AtomicDoubleArray) ("set", Nil, (AArray,MInt,MDouble) :: MUnit, effect=write(0)) implements codegen($cala, {
    val arg1 = quotedArg(0)
    val arg2 = quotedArg(1)
    val arg3 = quotedArg(2)
    s"""$arg1.set($arg2,$arg3)"""
  })
  }
}

trait AtomicBooleanOps {
    this: OptiGraphDSL =>
    def importAtomicBooleanOps() {
      val AtomicBoolean = grp("AtomicBoolean")
      val ABool = ephemeralTpe("java.util.concurrent.atomic.AtomicBoolean")
      static (AtomicBoolean) ("apply", Nil, MBoolean :: ABool, effect=mutable) implements codegen($cala, {
        val arg1 = quotedArg(0)
        s"""new java.util.concurrent.atomic.AtomicBoolean($arg1)"""
      })
      direct (AtomicBoolean) ("get", Nil, ABool :: MBoolean) implements codegen($cala, {
        val arg1 = quotedArg(0)
        s"""$arg1.get()"""
      })
      direct (AtomicBoolean) ("getAndSet", Nil, (ABool,MBoolean) :: MBoolean, effect=write(0)) implements codegen($cala, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1.getAndSet($arg2)"""
      })
      direct (AtomicBoolean) ("set", Nil, (ABool,MBoolean) :: MUnit, effect=write(0)) implements codegen($cala, {
    val arg1 = quotedArg(0)
    val arg2 = quotedArg(1)
    s"""$arg1.set($arg2)"""
  })
  }
}