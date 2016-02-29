package ppl.dsl.forge
package dsls
package optiwrangler

import core.{ForgeApplication,ForgeApplicationRunner}

trait RangeOps {
  this: OptiWranglerDSL =>

  def importRangeOps() {
  	val A = tpePar("A")
  	val Range = tpe("Range")
  	data (Range, "start" -> MInt, "end" -> MInt)
	static (Range) ("apply", Nil, (MInt, MInt) :: Range) implements allocates (Range, {
  val arg1 = quotedArg(0)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

	val RangeOps = withTpe (Range)
	RangeOps {
	  compiler ("startIndex") (Nil :: MInt) implements getter (0, "start")
	  compiler ("endIndex") (Nil :: MInt) implements getter (0, "end")
	  compiler ("length") (Nil :: MInt) implements single {
  val self = quotedArg("self")
  s"""endIndex($self) - startIndex($self)"""
}
	  compiler ("getElement") (MInt :: MInt) implements single {
  val arg1 = quotedArg(1)
  val self = quotedArg("self")
  s"""$arg1 + startIndex($self)"""
}

	  compiler ("range_illegalalloc") (MInt :: MNothing, effect = simple) implements composite {
  s"""fatal("Range cannot be allocated from a parallel op")"""
}
	  compiler ("range_illegalupdate") ((MInt,MInt) :: MNothing, effect = simple) implements composite {
  s"""fatal("Range cannot be updated")"""
}

	  infix ("foreach") ((MInt ==> MUnit) :: MUnit) implements foreach(MInt, 0, {
  val arg1 = quotedArg(1)
  s"""e => $arg1(e)"""
})

	  parallelize as ParallelCollection (MInt, lookupOp("range_illegalalloc"), lookupOp("length"), lookupOp("getElement"), lookupOp("range_illegalupdate"))
	}
  }
}