package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait IndexVectorOps {
  this: OptiLADSL =>

  def importIndexVectorOps() {
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")

    // data fields - we take a tagged union approach to enable range- and sequence- based IndexVectors without subtyping
    data(IndexVector, ("_data", MArray(MInt)), ("_start", MInt), ("_end", MInt), ("_isRow", MBoolean), ("_isRange", MBoolean))

    // static methods
    static (IndexVector) ("apply", Nil, (MInt,MInt) :: IndexVector) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""IndexVector($arg1,$arg2,unit(true))"""
    }
    static (IndexVector) ("apply", Nil, (MInt,MInt,MBoolean) :: IndexVector) implements
      allocates(IndexVector, {
  s"""array_empty_imm[Int](unit(0))"""
}, quotedArg(0), quotedArg(1), quotedArg(2), {
  s"""unit(true)"""
})

    static (IndexVector) ("apply", Nil, DenseVector(MInt) :: IndexVector) implements redirect {
      val arg1 = quotedArg(0)
      s"""IndexVector($arg1,$arg1.isRow)"""
    }
    static (IndexVector) ("apply", Nil, (DenseVector(MInt), MBoolean) :: IndexVector) implements
      allocates(IndexVector, {
  val arg1 = quotedArg(0)
  s"""indexvector_copyarray($arg1)"""
}, {
  s"""unit(0)"""
}, {
  s"""unit(0)"""
}, quotedArg(1), {
  s"""unit(false)"""
})

    static (IndexVector) ("apply", Nil, MethodSignature(List(MArray(MInt), ("isRow", MBoolean, "unit(true)")), IndexVector)) implements
      redirect {
  val arg1 = quotedArg(0)
  s"""indexvector_fromarray($arg1,isRow)"""
}

    direct (IndexVector) ("indexvector_fromarray", Nil, (MArray(MInt),MBoolean) :: IndexVector) implements
      allocates(IndexVector, quotedArg(0), {
  s"""unit(0)"""
}, {
  s"""unit(0)"""
}, quotedArg(1), {
  s"""unit(false)"""
})

    compiler (IndexVector) ("indexvector_copyarray", Nil, DenseVector(MInt) :: MArray(MInt)) implements composite {
        val arg1 = quotedArg(0)
        s"""val d = array_empty[Int]($arg1.length)
$arg1.indices foreach { i => d(i) = $arg1(i) }
d.unsafeImmutable"""
      }

    // this is unsafe because it uses the underlying input array directly instead of copying
    // they should only be used if we know the intermediate reference is dead or immutable (to avoid unsafe aliasing)
    // TODO: for some reason this does not work when we think it should, so we are reverting to the safer copy-always policy.

    // compiler (IndexVector) ("unsafe_dense_to_index", Nil, DenseVector(MInt) :: IndexVector) implements composite ${
    //   indexvector_fromarray(densevector_raw_data($0), $0.isRow)
    // }

    // index helpers
    for (arity <- 2 to 6) {
      val Tup = tpeInst(lookupTpe("Tuple"+arity, stage = compile), (0 until arity).map(i => MInt).toList)

      // unroll during staging to specialize for each arity
      val d = (2 to arity).map(k => "dims._" + k)
      val s1 = d.scanRight("1")((a,b) => a + "*" + b)

      val s2 = s1.zipWithIndex.map(t => "inds._"+(t._2+1) + "*" + t._1)
      val retFlat = s2.mkString(" + ")
      // e.g. for index (i,j,k,l) and dims (a,b,c,d), returns (i*b*c*d + j*c*d + k*d + l)
      direct (IndexVector) ("flatten", Nil, (("inds",Tup),("dims",Tup)) :: MInt) implements redirect {
  s"""$retFlat"""
}

      val s3 = s1.zipWithIndex.map(t => "(i / (" + t._1 + ")) % dims._" + (t._2+1))
      val retTuple = s3.mkString("(",",",")")
      // e.g. for index i and dims (a,b,c,d), returns [i/dcb % a, i/dc % b, i/d % c, i/1 % d]
      direct (IndexVector) ("unflatten", Nil, (("i",MInt),("dims",Tup)) :: Tup) implements redirect {
      s"""$retTuple"""
    }
    }

    val IndexVectorOps = withTpe(IndexVector)
    IndexVectorOps {
      compiler ("indexvector_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("indexvector_end") (Nil :: MInt) implements getter(0, "_end")
      compiler ("indexvector_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_data")
      compiler ("indexvector_is_range") (Nil :: MBoolean) implements getter(0, "_isRange")

      // TODO: the _isRange field should be a compile-time constant. can this be optimized (or does it already) eliminate the conditional in length/apply?

      infix ("length") (Nil :: MInt) implements composite {
          val self = quotedArg("self")
          s"""if (indexvector_is_range($self)) {
  indexvector_end($self) - indexvector_start($self)
}
else {
  array_length(indexvector_raw_data($self))
}"""
        }
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("apply") (MInt :: MInt) implements composite {
          val arg1 = quotedArg(1)
          val self = quotedArg("self")
          s"""fassert($arg1 >= 0 && $arg1 < $self.length, "IndexVector apply out of bounds at index " + $arg1)
if (indexvector_is_range($self)) {
  indexvector_start($self) + $arg1
}
else {
  indexvector_raw_data($self).apply($arg1)
}"""
        }
      infix ("apply") (IndexVector :: IndexVector) implements composite {
          val arg1 = quotedArg(1)
          val self = quotedArg("self")
          s"""val out = $arg1 { i => $self(i) }
IndexVector(out, $self.isRow)"""
        }

      infix ("slice") ((("start",MInt),("end",MInt)) :: IndexVector) implements composite {
          val self = quotedArg("self")
          val start = quotedArg("start")
          val end = quotedArg("end")
          s"""if (indexvector_is_range($self)) {
  fassert($start >= indexvector_start($self) && $end <= indexvector_end($self), "IndexVector slice (" + $start + "," + $end + ") out of bounds (" + indexvector_start($self) + "," + indexvector_end($self) + ")")
  IndexVector($start, $end, $self.isRow)
}
else {
  $self($start::$end)
}"""
        }

      infix ("t") (Nil :: IndexVector) implements allocates(IndexVector, {
  val self = quotedArg("self")
  s"""indexvector_raw_data($self)"""
}, {
  val self = quotedArg("self")
  s"""indexvector_start($self)"""
}, {
  val self = quotedArg("self")
  s"""indexvector_end($self)"""
}, {
  val self = quotedArg("self")
  s"""!(indexvector_isrow($self))"""
}, {
  val self = quotedArg("self")
  s"""indexvector_is_range($self)"""
})

      infix ("Clone") (Nil :: IndexVector, aliasHint = copies(0)) implements composite {
          val self = quotedArg("self")
          s"""if (indexvector_is_range($self)) {
  IndexVector(indexvector_start($self),indexvector_end($self),$self.isRow)
}
else {
  indexvector_fromarray(array_clone(indexvector_raw_data($self)), $self.isRow)
}"""
        }

      infix ("toDense") (Nil :: DenseVector(MInt)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(0)
          s"""if (indexvector_is_range($self)) { $self.map(e => e) }
else {
  
  densevector_fromarray(indexvector_raw_data($arg1), $arg1.isRow)
}"""
        }

      direct ("__equal") (IndexVector :: MBoolean) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.toDense == $arg1"""
      }
      direct ("__equal") (DenseVector(MInt) :: MBoolean) implements composite {
  val arg1 = quotedArg(1)
  val self = quotedArg("self")
  s"""$arg1 == $self"""
}

      // compiler ("indexvector_filter_helper") (IndexVector, MInt ==> MBoolean) :: DenseVector(MInt)) implements filter((MInt,MInt), 0, ${e => $1(e)}, ${e => e})
      // infix ("filter") ((MInt ==> MBoolean) :: IndexVector) implements composite ${
      //   IndexVector(indexvector_filter_helper($self, $1))
      // }

      infix ("filter") ((MInt ==> MBoolean) :: IndexVector) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val data = array_filter($self.toArray, $arg1)
indexvector_fromarray(data, $self.isRow)"""
        }

      // These are required because reduce currently requires a collection of type A to be matched with a signature (A,A) => A.
      // Therefore, we need a method that takes as input a collection of type Int in order to reduce it with the proper zero values.
      val T = tpePar("T")
      compiler ("min_index_of") (DenseVector(T) :: MInt, TOrdering(T), addTpePars = T) implements reduce(MInt, 0, "unit(Int.MaxValue)", {
        val arg1 = quotedArg(1)
        s"""(a,b) => if ($arg1(a) < $arg1(b)) a else b"""
      })
      compiler ("max_index_of") (DenseVector(T) :: MInt, TOrdering(T), addTpePars = T) implements reduce(MInt, 0, "unit(Int.MinValue)", {
  val arg1 = quotedArg(1)
  s"""(a,b) => if ($arg1(a) > $arg1(b)) a else b"""
})

      // parallel, so the conversion can fuse with the consumer
      // is this fast and robust enough to capture parallel operators over index vectors?
      fimplicit ("indexToDense") (Nil :: DenseVector(MInt)) implements composite {
          val self = quotedArg("self")
          s"""if (Settings.verbose > 0) println("(performance warning): automatic conversion from IndexVector to DenseVector")
$self.toDense"""
        }

      // naming is by convention here, a little brittle. would it be better to put this in extern?
      val grpName = if (Config.fastCompile) "$Flat" else "DenseVector"
      fimplicit ("chainIndexToDenseOps") (Nil :: ephemeralTpe(grpName+"DenseVectorOpsCls[Int]", stage = now)) implements composite {
          val self = quotedArg("self")
          s"""repTo${grpName}DenseVectorOpsCls(indexToDense($self))"""
        }
      fimplicit ("chainIndexToDenseIntOps") (Nil :: ephemeralTpe(grpName+"DenseVectorIntOpsCls", stage = now)) implements composite {
          val self = quotedArg("self")
          s"""repTo${grpName}DenseVectorIntOpsCls(indexToDense($self))"""
        }

      compiler ("indexvector_illegalalloc") (MInt :: MNothing) implements composite {
        s"""fatal("IndexVectors cannot be allocated from a parallel op")"""
      }
      compiler ("indexvector_illegalupdate") ((MInt, MInt) :: MNothing) implements composite {
  s"""fatal("IndexVectors cannot be updated")"""
}

      // IndexVectors can't be mapped over, but they can be zipped with or reduced
      parallelize as ParallelCollection(MInt, lookupOp("indexvector_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",5), lookupOp("indexvector_illegalupdate"))
    }

    // allows us to perform operations without converting to a DenseVector first
    addVectorCommonOps(IndexVector,MInt)
  }
}
