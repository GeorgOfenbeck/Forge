package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait SparseVectorOps {
  this: OptiLADSL =>

  // Duplicates an important subset of the sparse interface between SparseVector and SparseVectorView,
  // avoiding an unnecessary conversion when used with views.
  def addSparseVectorCommonOps(v: Rep[DSLType], T: Rep[TypePar]) {
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val SparseVector = lookupTpe("SparseVector")

    val SparseVectorCommonOps = withTpe(v)
    SparseVectorCommonOps {
      for (rhs <- List(DenseVector(T), DenseVectorView(T))) {
        infix ("*") (rhs :: SparseVector(T), TArith(T)) implements composite {
            val self = quotedArg("self")
            val arg1 = quotedArg(1)
            s"""val out = ($self.indices.zip($self.nz) { (i,e) => e*$arg1(i) })

sparsevector_alloc_raw($self.length, $self.isRow, out.toArray, $self.indices.toArray, $self.nnz)"""
          }

        infix ("*:*") (rhs :: T, TArith(T)) implements composite {
            val self = quotedArg("self")
            val arg1 = quotedArg(1)
            s"""($self.indices.zip($self.nz) { (i,e) => e*$arg1(i) }).sum"""
          }
      }

      // This appears to be slower than the sparse dot product defined below that uses zipVectorUnion.
      // infix ("*:*") (SparseVector(T) :: T, TArith(T)) implements composite ${
      //   // The lookup of the rhs sparse vector is log(n)
      //   ($self.indices.zip($self.nz) { (i,e) => e*$1(i) }).sum
      // }

      infix ("*") (T :: SparseVector(T), TArith(T)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val out = $self.nz.map(e => e*$arg1)
sparsevector_alloc_raw($self.length, $self.isRow, out.toArray, $self.indices.toArray, $self.nnz)"""
        }

      infix ("*") (DenseMatrix(T) :: DenseVector(T), TArith(T)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""fassert($self.isRow, "dimension mismatch: vector * matrix")
$arg1.mapColsToVector { col => $self *:* col }"""
        }
    }
  }

  def importSparseVectorOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val A = tpePar("A")
    val B = tpePar("B")

    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val IndexVector = lookupTpe("IndexVector")
    val SparseVector = lookupTpe("SparseVector")
    val SparseVectorView = lookupTpe("SparseVectorView")

    // data fields
    data(SparseVector, ("_length", MInt), ("_isRow", MBoolean), ("_data", MArray(T)), ("_indices", MArray(MInt)), ("_nnz", MInt))

    // static methods
    static (SparseVector) ("apply", T, (MInt, MBoolean) :: SparseVector(T), effect = mutable) implements allocates(SparseVector, {
  val arg1 = quotedArg(0)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(1)
  s"""$arg1"""
}, {
  s"""array_empty[T](unit(32))"""
}, {
  s"""array_empty[Int](unit(32))"""
}, {
  s"""unit(0)"""
})

    static (SparseVector) ("fromElements", T, MethodSignature(List(
        ("length", MInt),
        ("isRow", MBoolean),
        ("nzIndices", IndexVector),
        ("nzElements", DenseVector(T))), SparseVector(T))) implements composite {
  s"""val (sortedIndices, sortedOriginalPositions) = nzIndices.toDense.sortWithIndex
      val sortedElements = nzElements(sortedOriginalPositions)
      sparsevector_alloc_raw(length, isRow, densevector_raw_data(sortedElements), densevector_raw_data(sortedIndices), sortedIndices.length)"""
}

    static (SparseVector) ("fromSortedElements", T, MethodSignature(List(
        ("length", MInt),
        ("isRow", MBoolean),
        ("nzIndices", IndexVector),
        ("nzElements", DenseVector(T))), SparseVector(T))) implements composite {
  s"""sparsevector_alloc_raw(length, isRow, densevector_raw_data(nzElements), densevector_raw_data(nzIndices), nzIndices.length)"""
}

    static (SparseVector) ("fromFunc", T, (MInt, MBoolean, IndexVector, MInt ==> T) :: SparseVector(T)) implements composite {
        val arg1 = quotedArg(2)
        val arg2 = quotedArg(0)
        val arg3 = quotedArg(1)
        val arg4 = quotedArg(3)
        s"""val sorted = $arg1.sort
sparsevector_alloc_raw($arg2, $arg3, densevector_raw_data(sorted.map($arg4)), densevector_raw_data(sorted), sorted.length)"""
      }

    // helper
    compiler (SparseVector) ("sparsevector_alloc_raw", T, (MInt, MBoolean, MArray(T), MArray(MInt), MInt) :: SparseVector(T)) implements
      allocates(SparseVector, {
  val arg1 = quotedArg(0)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(1)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(2)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(3)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(4)
  s"""$arg1"""
})

    static (SparseVector) ("zeros", Nil, MInt :: SparseVector(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      s"""SparseVector[Double]($arg1, unit(true)).unsafeImmutable"""
    }
    static (SparseVector) ("zerosf", Nil, MInt :: SparseVector(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      s"""SparseVector[Float]($arg1, unit(true)).unsafeImmutable"""
    }
    static (SparseVector) ("rand", Nil, (("length", MInt), ("sparsity", MDouble)) :: SparseVector(MDouble)) implements composite {
        val arg1 = quotedArg(0)
        s"""val density = 1.0 - sparsity
val nnz = floor(density*length)
val indices = shuffle(0::length).take(nnz)
SparseVector.fromFunc($arg1, true, indices, i => random[Double])"""
      }
    static (SparseVector) ("randf", Nil, (("length", MInt), ("sparsity", MDouble)) :: SparseVector(MFloat)) implements composite {
        val arg1 = quotedArg(0)
        s"""val density = 1.0 - sparsity
val nnz = floor(density*length)
val indices = shuffle(0::length).take(nnz)
SparseVector.fromFunc($arg1, true, indices, i => random[Float])"""
      }

    compiler (SparseVector) ("bsearch", Nil, (("a",MArray(MInt)),("_start",MInt),("_end",MInt),("pos",MInt)) :: MInt) implements single {
        s"""var start = _start
var end = _end
var mid = (readVar(start)+readVar(end))/2
var found = false
while (!found && (start <= end)) {
  mid = (readVar(start)+readVar(end))/2
  if (pos > a(mid)) {
    start = mid + 1
  }
  else if (pos < a(mid)) {
    end = mid - 1
  }
  else {
    found = true
  }
}

if (found) mid
else {
  
  if (_end < _start) ~(_start)
  else if (pos > a(mid)) ~(mid+1)
  else ~readVar(mid)
}"""
      }

    // defaultValue must obey "zero" semantics:
    //    x+defaultValue = x,              x-defaultValue = -x
    //    x*defaultValue = defaultValue,   defaultValue/x = defaultValue
    compiler (SparseVector) ("defaultValue", T, Nil :: T) implements composite {
        s"""manifest[T] match {
  case Manifest.Boolean => unit(false).asInstanceOf[Rep[T]]
  case Manifest.Int => unit(0).asInstanceOf[Rep[T]]
  case Manifest.Long => unit(0L).asInstanceOf[Rep[T]]
  case Manifest.Float => unit(0f).asInstanceOf[Rep[T]]
  case Manifest.Double => unit(0.0).asInstanceOf[Rep[T]]
  case _ => fatal("no default value found for type: " + manifest[T])
}"""
      }

    /*
     * Sparse sequential operators useful for both vectors and matrices. outIndices/outData are passed as inputs for matrices,
     * which call these methods repeatedly for different rows.
     *
     * sparse-sparse math ops should be at most O(nnz1+nnz2). how should they be parallelized?
     * would it be easier if we represented the sparse vectors as hashmaps? (e.g., and chunk one of the key sets)
     */
    compiler (SparseVector) ("zipUnion", (A,B,R), MethodSignature(List(("nnzInit",MInt),("aIdxInit",MInt),("annz",MInt),("aIndices",MArray(MInt)),("aData",MArray(A)),("bIdxInit",MInt),("bnnz",MInt),("bIndices",MArray(MInt)),("bData",MArray(B)),("outIndices",MArray(MInt)),("outData",MArray(R)),("f",(A,B) ==> R)), MInt), effect = write(9,10)) implements single {
        s"""var nnz = nnzInit
var aIdx = aIdxInit
var bIdx = bIdxInit
while (aIdx < annz || bIdx < bnnz) {
  
  if (aIdx < annz && bIdx < bnnz) {
    
    if (array_apply(aIndices,aIdx) < array_apply(bIndices,bIdx)) {
array_update(outIndices, nnz, array_apply(aIndices,aIdx))
array_update(outData, nnz, f(array_apply(aData,aIdx),defaultValue[B]))
aIdx += 1
    }
    else if (array_apply(aIndices,aIdx) > array_apply(bIndices,bIdx)) {
array_update(outIndices, nnz, array_apply(bIndices,bIdx))
array_update(outData, nnz, f(defaultValue[A], array_apply(bData,bIdx)))
bIdx += 1
    }
    else {
array_update(outIndices, nnz, array_apply(aIndices,aIdx)) 
array_update(outData, nnz, f(array_apply(aData,aIdx),array_apply(bData,bIdx)))
aIdx += 1
bIdx += 1
    }
  }
  else if (aIdx < annz) {
    
    array_update(outIndices, nnz, array_apply(aIndices,aIdx))
    array_update(outData, nnz, f(array_apply(aData,aIdx),defaultValue[B]))
    aIdx += 1
  }
  else if (bIdx < bnnz) {
    
    array_update(outIndices, nnz, array_apply(bIndices,bIdx))
    array_update(outData, nnz, f(defaultValue[A], array_apply(bData,bIdx)))
    bIdx += 1
  }
  else {
    fatal("zipUnion should never reach here")
  }
  nnz += 1
}
nnz"""
      }

    compiler (SparseVector) ("zipIntersect", (A,B,R), MethodSignature(List(("nnzInit",MInt),("aIdxInit",MInt),("annz",MInt),("aIndices",MArray(MInt)),("aData",MArray(A)),("bIdxInit",MInt),("bnnz",MInt),("bIndices",MArray(MInt)),("bData",MArray(B)),("outIndices",MArray(MInt)),("outData",MArray(R)),("f",(A,B) ==> R)), MInt), effect = write(9,10)) implements single {
        s"""var nnz = nnzInit
var aIdx = aIdxInit
var bIdx = bIdxInit
while (aIdx < annz && bIdx < bnnz) {
  
  if (array_apply(aIndices,aIdx) < array_apply(bIndices,bIdx)) {
    aIdx += 1
  }
  else if (array_apply(aIndices,aIdx) > array_apply(bIndices,bIdx)) {
    bIdx += 1
  }
  else {
    array_update(outIndices, nnz, array_apply(aIndices,aIdx)) 
    array_update(outData, nnz, f(array_apply(aData,aIdx),array_apply(bData,bIdx)))
    aIdx += 1
    bIdx += 1
    nnz += 1
  }
}
nnz"""
      }

    val SparseVectorOps = withTpe (SparseVector)
    SparseVectorOps {
      /**
       * Accessors
       */
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("nnz") (Nil :: MInt) implements getter(0, "_nnz")

      infix ("nz") (Nil :: DenseVectorView(T), aliasHint = contains(0)) implements single {
          val self = quotedArg("self")
          s"""DenseVectorView[T](sparsevector_raw_data($self), 0, 1, $self.nnz, $self.isRow)"""
        }

      infix ("indices") (Nil :: IndexVector) implements composite {
          val self = quotedArg("self")
          s"""val rawIndices = sparsevector_raw_indices($self)
val data = array_fromfunction($self.nnz, i => rawIndices(i))
indexvector_fromarray(data, $self.isRow)"""
        }

      compiler ("sparsevector_find_offset") (("pos",MInt) :: MInt) implements composite {
          val self = quotedArg("self")
          s"""val indices = sparsevector_raw_indices($self)
bsearch(indices, 0, $self.nnz-1, pos)"""
        }

      infix ("apply") (MInt :: T) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val data = sparsevector_raw_data($self)
val offRaw = sparsevector_find_offset($self, $arg1)
if (offRaw > -1) {
  var d = data 
  array_apply(readVar(d),offRaw)
}
else defaultValue[T]"""
        }

      infix ("apply") (IndexVector :: SparseVector(T)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val data = sparsevector_raw_data($self)
val offsets = $arg1.map(i => sparsevector_find_offset($self,i))
val logicalIndices = offsets.find(_ > -1)
val physicalIndices = offsets(logicalIndices)


sparsevector_alloc_raw($arg1.length, $self.isRow, densevector_raw_data(physicalIndices.mutable.map(i => array_apply(data,i))), densevector_raw_data(logicalIndices), logicalIndices.length)"""
        }

      infix ("isEmpty") (Nil :: MBoolean) implements composite {
        val self = quotedArg("self")
        s"""$self.nnz == 0"""
      }
      infix ("first") (Nil :: T) implements composite {
        val self = quotedArg("self")
        s"""$self(0)"""
      }
      infix ("firstnz") (Nil :: T) implements composite {
        val self = quotedArg("self")
        s"""$self.nz.apply(0)"""
      }
      infix ("last") (Nil :: T) implements composite {
        val self = quotedArg("self")
        s"""$self($self.length-1)"""
      }
      infix ("lastnz") (Nil :: T) implements composite {
        val self = quotedArg("self")
        s"""$self.nz.apply($self.nnz-1)"""
      }
      infix ("drop") (MInt :: SparseVector(T)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.slice($arg1, $self.length)"""
      }
      infix ("take") (MInt :: SparseVector(T)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.slice(0, $arg1)"""
}

      infix ("slice") ((("start",MInt),("end",MInt)) :: SparseVector(T)) implements redirect {
  val self = quotedArg("self")
  s"""$self(start::end)"""
}

      infix ("contains") (T :: MBoolean) implements composite {
          val arg1 = quotedArg(1)
          val self = quotedArg("self")
          s"""if ($arg1 == defaultValue[T] && $self.nnz < $self.length) true
else densevector_alloc_raw($self.nnz, true, sparsevector_raw_data($self)).contains($arg1)"""
        }

      infix ("distinct") (Nil :: DenseVector(T)) implements composite {
          val self = quotedArg("self")
          s"""val data = densevector_alloc_raw($self.nnz, true, sparsevector_raw_data($self))
val out = DenseVector[T](0, $self.isRow)

if ($self.nnz < $self.length) out <<= defaultValue[T]

for (i <- 0 until $self.nnz) {
  
  if (!out.contains(data(i))) out <<= data(i)
}
out.unsafeImmutable"""
        }

      /**
       * Miscellaneous
       */
      infix ("t") (Nil :: SparseVector(T)) implements allocates(SparseVector, {
        val arg1 = quotedArg(0)
        s"""sparsevector_length($arg1)"""
      }, {
        val arg1 = quotedArg(0)
        s"""!(sparsevector_isrow($arg1))"""
      }, {
        val arg1 = quotedArg(0)
        s"""array_soft_clone(sparsevector_raw_data($arg1))"""
      }, {
        val arg1 = quotedArg(0)
        s"""array_soft_clone(sparsevector_raw_indices($arg1))"""
      }, {
        val arg1 = quotedArg(0)
        s"""sparsevector_nnz($arg1)"""
      })
      infix ("mt") (Nil :: MUnit, effect = write(0)) implements composite {
          val arg1 = quotedArg(0)
          s"""sparsevector_set_isrow($arg1, !$arg1.isRow)"""
        }

      // infix ("toMat") (Nil :: SparseMatrix(T)) implements composite ${
      //   if ($self.isRow) {
      //     SparseMatrix($self)
      //   }
      //   else {
      //     SparseMatrix[T](0,0) <<| $self
      //   }
      // }

      infix ("Clone") (Nil :: SparseVector(T), aliasHint = copies(0)) implements allocates(SparseVector, {
        val arg1 = quotedArg(0)
        s"""sparsevector_length($arg1)"""
      }, {
        val arg1 = quotedArg(0)
        s"""sparsevector_isrow($arg1)"""
      }, {
        val arg1 = quotedArg(0)
        s"""array_clone(sparsevector_raw_data($arg1))"""
      }, {
        val arg1 = quotedArg(0)
        s"""array_clone(sparsevector_raw_indices($arg1))"""
      }, {
        val arg1 = quotedArg(0)
        s"""sparsevector_nnz($arg1)"""
      })
      infix ("mutable") (Nil :: SparseVector(T), effect = mutable, aliasHint = copies(0)) implements allocates(SparseVector, {
  val arg1 = quotedArg(0)
  s"""sparsevector_length($arg1)"""
}, {
  val arg1 = quotedArg(0)
  s"""sparsevector_isrow($arg1)"""
}, {
  val arg1 = quotedArg(0)
  s"""array_clone(sparsevector_raw_data($arg1))"""
}, {
  val arg1 = quotedArg(0)
  s"""array_clone(sparsevector_raw_indices($arg1))"""
}, {
  val arg1 = quotedArg(0)
  s"""sparsevector_nnz($arg1)"""
})

      infix ("toDense") (Nil :: DenseVector(T)) implements composite {
          val self = quotedArg("self")
          s"""val out = DenseVector[T]($self.length, $self.isRow)
val indices = densevector_alloc_raw($self.nnz, true, sparsevector_raw_indices($self))
val data = sparsevector_raw_data($self)
(0::indices.length) foreach { i => out(indices(i)) = data(i) } 
out.unsafeImmutable"""
        }

      infix ("makeString") (Nil :: MString, TStringable(T)) implements single {
          val self = quotedArg("self")
          s"""val indices = sparsevector_raw_indices($self)
val data = sparsevector_raw_data($self)
var s = ""

if ($self == null) {
  s = "null"
}
else if ($self.nnz == 0) {
  s = "[ ]"
}
else if ($self.isRow) {
  for (i <- 0 until $self.nnz-1) {
    s = s + "(" + array_apply(indices,i) + ", " + array_apply(data,i).makeStr + "), "
  }
  s = s + "(" + array_apply(indices,$self.nnz-1) + ", " + array_apply(data,$self.nnz-1).makeStr + ") "
}
else {
  for (i <- 0 until $self.nnz-1) {
    s = s + "(" + array_apply(indices,i) + ", " + array_apply(data,i).makeStr + ")\\n"
  }
  s = s + "(" + array_apply(indices,$self.nnz-1) + ", " + array_apply(data,$self.nnz-1).makeStr + ")"
}
s"""
        }
      infix ("toString") (Nil :: MString) implements single {
          val self = quotedArg("self")
          s"""val indices = sparsevector_raw_indices($self)
val data = sparsevector_raw_data($self)
var s = ""

if ($self == null) {
  s = "null"
}
else if ($self.nnz == 0) {
  s = "[ ]"
}
else if ($self.isRow) {
  for (i <- 0 until $self.nnz-1) {
    s = s + "(" + array_apply(indices,i) + ", " + optila_fmt_str(array_apply(data,i)) + "), "
  }
  s = s + "(" + array_apply(indices,$self.nnz-1) + ", " + optila_fmt_str(array_apply(data,$self.nnz-1)) + ") "
}
else {
  for (i <- 0 until $self.nnz-1) {
    s = s + "(" + array_apply(indices,i) + ", " + optila_fmt_str(array_apply(data,i)) + ")\\n"
  }
  s = s + "(" + array_apply(indices,$self.nnz-1) + ", " + optila_fmt_str(array_apply(data,$self.nnz-1)) + ")"
}
s"""
        }

      infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite {
  val self = quotedArg("self")
  s"""println($self.makeStr + "\\n")"""
} // $self.toString doesn't work in Delite


      /**
       * Data operations
       */
      compiler ("sparsevector_raw_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("sparsevector_raw_indices") (Nil :: MArray(MInt)) implements getter(0, "_indices")
      compiler ("sparsevector_set_length") (MInt :: MUnit, effect = write(0)) implements setter(0, "_length", {
        val arg1 = quotedArg(1)
        s"""$arg1"""
      })
      compiler ("sparsevector_set_isrow") (MBoolean :: MUnit, effect = write(0)) implements setter(0, "_isRow", {
        val arg1 = quotedArg(1)
        s"""$arg1"""
      })
      compiler ("sparsevector_set_raw_data") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", {
        val arg1 = quotedArg(1)
        s"""$arg1"""
      })
      compiler ("sparsevector_set_raw_indices") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_indices", {
        val arg1 = quotedArg(1)
        s"""$arg1"""
      })
      compiler ("sparsevector_set_nnz") (MInt :: MUnit, effect = write(0)) implements setter(0, "_nnz", {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

      infix ("update") ((("pos",MInt),("e",T)) :: MUnit, effect = write(0)) implements composite {
          val self = quotedArg("self")
          s"""val offRaw = sparsevector_find_offset($self, pos)
if (offRaw > -1) array_update(sparsevector_raw_data($self), offRaw, e)
else {
  if (e != defaultValue[T]) {
    val off = ~offRaw
    sparsevector_insertspace($self, off, 1)
    array_update(sparsevector_raw_indices($self), off, pos)
    array_update(sparsevector_raw_data($self), off, e)
  }
}"""
        }

      // must be sequential (updates are not disjoint in the underlying arrays)
      infix ("update") ((("indices",IndexVector),("e",T)) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          s"""for (i <- 0 until indices.length) {
  fassert(indices(i) >= 0 && indices(i) < $self.length, "index out of bounds: bulk vector update")
  $self(indices(i)) = e
}"""
        }

      infix ("update") ((("indices",IndexVector),("v",SparseVector(T))) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          s"""fassert(indices.length == v.length, "dimension mismatch: bulk vector update")

for (i <- 0 until indices.length) {
  fassert(indices(i) >= 0 && indices(i) < $self.length, "index out of bounds: bulk vector update")
  $self(indices(i)) = v(i)
}"""
        }

      infix ("<<") (T :: SparseVector(T)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val out = $self.mutable
out <<= $arg1
out.unsafeImmutable"""
        }

      infix("<<") (SparseVector(T) :: SparseVector(T)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val outIndices = array_empty[Int]($self.nnz + $arg1.nnz)
val outData = array_empty[T]($self.nnz + $arg1.nnz)

for (i <- 0 until $self.nnz) {
  array_update(outIndices, i, array_apply(sparsevector_raw_indices($self), i))
  array_update(outData, i, array_apply(sparsevector_raw_data($self), i))
}
for (i <- 0 until $arg1.nnz) {
  array_update(outIndices, i+$self.nnz, array_apply(sparsevector_raw_indices($arg1), i)+$self.length)
  array_update(outData, i+$self.nnz, array_apply(sparsevector_raw_data($arg1), i))
}

sparsevector_alloc_raw($self.length+$arg1.length, $self.isRow, outData, outIndices, array_length(outIndices))"""
        }

      infix ("<<=") (T :: MUnit, effect = write(0)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""sparsevector_insert_at_off($self, $self.nnz, $self.length, $arg1)"""
      }
      infix ("<<=") (SparseVector(T) :: MUnit, effect = write(0)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.insertAll($self.length,$arg1)"""
}

      compiler ("sparsevector_insert_at_off") ((("off",MInt),("pos",MInt),("e",T)) :: MUnit, effect = write(0)) implements composite {
          val self = quotedArg("self")
          s"""sparsevector_insertspace($self, off, 1)
val data = sparsevector_raw_data($self)
val indices = sparsevector_raw_indices($self)
array_update(indices, off, pos)
array_update(data, off, e)
for (i <- off+1 until $self.nnz) {
  array_update(indices, i, indices(i) + 1)
}

sparsevector_set_length($self, $self.length + 1)"""
        }

      infix ("insert") ((MInt,T) :: MUnit, effect = write(0)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""val offRaw = sparsevector_find_offset($self, $arg1)
val off = if (offRaw > -1) offRaw else ~offRaw
sparsevector_insert_at_off($self, off, $arg1, $arg2)"""
        }

      infix ("insertAll") ((("pos",MInt),("xs",SparseVector(T))) :: MUnit, effect = write(0)) implements composite {
          val self = quotedArg("self")
          s"""val offRaw = sparsevector_find_offset($self, pos)
val off = if (offRaw > -1) offRaw else ~offRaw

sparsevector_insertspace($self, off, xs.nnz)
val data = sparsevector_raw_data($self)
val indices = sparsevector_raw_indices($self)
for (i <- 0 until xs.nnz) {
  array_update(indices, i+off, array_apply(sparsevector_raw_indices(xs),i)+pos)
  array_update(data, i+off, array_apply(sparsevector_raw_data(xs), i))
}

for (i <- off+xs.nnz until $self.nnz) {
  array_update(indices, i, array_apply(indices, i) + xs.length)
}

sparsevector_set_length($self, $self.length + xs.length)"""
        }

      infix ("remove") (MInt :: MUnit, effect = write(0)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.removeAll($arg1, 1)"""
}

      infix ("removeAll") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements composite {
          val self = quotedArg("self")
          s"""val data = sparsevector_raw_data($self)
val indices = sparsevector_raw_indices($self)
val startRaw = sparsevector_find_offset($self, pos)
val start = if (startRaw > -1) startRaw else ~startRaw
val endRaw = sparsevector_find_offset($self, pos+len)
val end = if (endRaw > -1) endRaw else ~endRaw
val remaining = $self.nnz - end
array_copy(data, end, data, start, remaining)
array_copy(indices, end, indices, start, remaining)
sparsevector_set_length($self, $self.length - len)
sparsevector_set_nnz($self, start+remaining)"""
        }

      infix ("copyFrom") ((("pos",MInt),("xs",SparseVector(T))) :: MUnit, effect = write(0)) implements composite {
          val self = quotedArg("self")
          s"""for (i <- 0 until xs.length) {
  val e = xs(i)
  
  if (e != defaultValue[T]) {
    $self(pos+i) = e
  }
  
  
  else if ($self(pos+i) != defaultValue[T]) {
    $self(pos+i) = defaultValue[T]
  }
}"""
        }

      infix ("trim") (Nil :: MUnit, effect = write(0)) implements composite {
          val self = quotedArg("self")
          s"""val data = sparsevector_raw_data($self)
val indices = sparsevector_raw_indices($self)
if ($self.nnz < array_length(data)) {
  val outData = array_empty[T]($self.nnz)
  val outIndices = array_empty[Int]($self.nnz)
  array_copy(data, 0, outData, 0, $self.nnz)
  array_copy(indices, 0, outIndices, 0, $self.nnz)
  sparsevector_set_raw_data($self, outData.unsafeImmutable)
  sparsevector_set_raw_indices($self, outIndices.unsafeImmutable)
}"""
        }

      infix ("clear") (Nil :: MUnit, effect = write(0)) implements composite {
          val self = quotedArg("self")
          s"""sparsevector_set_length($self, 0)
sparsevector_set_nnz($self, 0)
sparsevector_set_raw_data($self, (array_empty[T](0)).unsafeImmutable)
sparsevector_set_raw_indices($self, (array_empty[Int](0)).unsafeImmutable)"""
        }

      compiler ("sparsevector_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          s"""sparsevector_ensureextra($self,len)
val data = sparsevector_raw_data($self)
val indices = sparsevector_raw_indices($self)
array_copy(data, pos, data, pos + len, $self.nnz - pos)
array_copy(indices, pos, indices, pos + len, $self.nnz - pos)
sparsevector_set_nnz($self, $self.nnz + len)"""
        }

      compiler ("sparsevector_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          s"""val data = sparsevector_raw_data($self)
if (array_length(data) - $self.nnz < extra) {
  sparsevector_realloc($self, $self.nnz + extra)
}"""
        }

      compiler ("sparsevector_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          s"""val data = sparsevector_raw_data($self)
val indices = sparsevector_raw_indices($self)
var n = max(4, array_length(data) * 2)
while (n < minLen) n = n*2
val newData = array_empty[T](n)
val newIndices = array_empty[Int](n)
array_copy(data, 0, newData, 0, $self.nnz)
array_copy(indices, 0, newIndices, 0, $self.nnz)
sparsevector_set_raw_data($self, newData.unsafeImmutable)
sparsevector_set_raw_indices($self, newIndices.unsafeImmutable)"""
        }


      /**
       * Math
       */

      // toDense will materialize in parallel, and then the dense operations will also be in parallel

      compiler ("zipVectorUnion") ((SparseVector(B), (T,B) ==> R) :: SparseVector(R), addTpePars = (B,R)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""val outIndices = array_empty[Int]($self.nnz+$arg1.nnz) 
val outData = array_empty[R]($self.nnz+$arg1.nnz)

val nnz = zipUnion(0, 0, $self.nnz, sparsevector_raw_indices($self), sparsevector_raw_data($self), 0, $arg1.nnz, sparsevector_raw_indices($arg1), sparsevector_raw_data($arg1), outIndices, outData, $arg2)
sparsevector_alloc_raw($self.length, $self.isRow, outData.unsafeImmutable, outIndices.unsafeImmutable, nnz)"""
        }

      compiler ("zipVectorIntersect") ((SparseVector(B), (T,B) ==> R) :: SparseVector(R), addTpePars = (B,R)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""val outIndices = array_empty[Int]($self.nnz) 
val outData = array_empty[R]($self.nnz)

val nnz = zipIntersect(0, 0, $self.nnz, sparsevector_raw_indices($self), sparsevector_raw_data($self), 0, $arg1.nnz, sparsevector_raw_indices($arg1), sparsevector_raw_data($arg1), outIndices, outData, $arg2)
sparsevector_alloc_raw($self.length, $self.isRow, outData.unsafeImmutable, outIndices.unsafeImmutable, nnz)"""
        }

      infix ("+") (SparseVector(T) :: SparseVector(T), TArith(T)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""zipVectorUnion[T,T,T]($self, $arg1, (a,b) => a+b)"""
      }
      infix ("+") (DenseVector(T) :: DenseVector(T), TArith(T)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.toDense + $arg1"""
      }
      infix ("+") (T :: DenseVector(T), TArith(T)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.toDense + $arg1"""
}

      infix ("-") (SparseVector(T) :: SparseVector(T), TArith(T)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""zipVectorUnion[T,T,T]($self, $arg1, (a,b) => a-b)"""
      }
      infix ("-") (DenseVector(T) :: DenseVector(T), TArith(T)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.toDense - $arg1"""
      }
      infix ("-") (T :: DenseVector(T), TArith(T)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.toDense - $arg1"""
}

      infix ("*") (SparseVector(T) :: SparseVector(T), TArith(T)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""zipVectorIntersect[T,T,T]($self, $arg1, (a,b) => a*b)"""
}

      // infix ("*") (SparseMatrix(T) :: SparseVector(T), TArith(T) implements composite ${
      // }

      infix ("*:*") (SparseVector(T) :: T, TArith(T)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""sum($self*$arg1)"""
}

      // infix ("**") (SparseVector(T) :: SparseMatrix(T), TArith(T)) implements composite ${
      //   fassert(!$self.isRow && $1.isRow, "dimension mismatch: vector outer product")
      //   val out = DenseMatrix[\$TT]($self.length, $1.length)
      //   for (i <- 0 until $self.length ){
      //     for (j <- 0 until $1.length ){
      //       out(i,j) = $self(i)*$1(j)
      //     }
      //   }
      //   out.unsafeImmutable
      // }

      infix ("**") (DenseVector(T) :: DenseMatrix(T), TArith(T)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.toDense ** $arg1"""
}

      infix ("/") (SparseVector(T) :: SparseVector(T), TArith(T)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""zipVectorIntersect[T,T,T]($self, $arg1, (a,b) => a/b)"""
      } // ignores x / 0 errors...
      infix ("/") (DenseVector(T) :: DenseVector(T), TArith(T)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.toDense / $arg1"""
      }
      infix ("/") (T :: SparseVector(T), TArith(T)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.mapnz(e => e/$arg1)"""
}

      infix ("abs") (Nil :: SparseVector(T), TArith(T)) implements composite {
        val self = quotedArg("self")
        s"""$self.mapnz { e => e.abs }"""
      }
      infix ("sum") (Nil :: T, TArith(T)) implements composite {
        val self = quotedArg("self")
        s"""$self.nz.sum"""
      }
      infix ("mean") (Nil :: MDouble, ("conv",T ==> MDouble)) implements composite {
  val self = quotedArg("self")
  s"""$self.mapnz(conv).sum / $self.length"""
}

      /**
       * Ordering
       */

      infix ("min") (Nil :: T, (TOrdering(T), THasMinMax(T))) implements composite {
          val self = quotedArg("self")
          s"""val min = $self.nz.min
if (min > defaultValue[T]) defaultValue[T] else min"""
        }

      infix ("max") (Nil :: T, (TOrdering(T), THasMinMax(T))) implements composite {
          val self = quotedArg("self")
          s"""val max = $self.nz.max
if (max < defaultValue[T]) defaultValue[T] else max"""
        }

      direct ("__equal") (DenseVector(T) :: MBoolean) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.toDense == $arg1"""
}

      direct ("__equal") (SparseVectorView(T) :: MBoolean) implements composite {
  val arg1 = quotedArg(1)
  val self = quotedArg("self")
  s"""$arg1 == $self"""
}

      direct ("__equal") (SparseVector(T) :: MBoolean) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""if ($self.length != $arg1.length || $self.nnz != $arg1.nnz || $self.isRow != $arg1.isRow) false
else {
  val dataEqual = densevector_alloc_raw($self.nnz, true, sparsevector_raw_data($self)) == densevector_alloc_raw($arg1.nnz, true, sparsevector_raw_data($arg1))
  val indexEqual = densevector_alloc_raw($self.nnz, true, sparsevector_raw_indices($self)) == densevector_alloc_raw($arg1.nnz, true, sparsevector_raw_indices($arg1))
  dataEqual && indexEqual
}"""
        }


      /**
       * Bulk
       */

      infix ("mapnz") ((T ==> R) :: SparseVector(R), addTpePars = R) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val out = $self.nz.map($arg1)
sparsevector_alloc_raw($self.length, $self.isRow, densevector_raw_data(out), sparsevector_raw_indices($self), $self.nnz)"""
        }

      infix ("reducenz") (((T,T) ==> T) :: T, TArith(T)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.nz.reduce($arg1)"""
}

      infix ("filternz") ((T ==> MBoolean) :: SparseVector(T)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val data = sparsevector_raw_data($self)
val selected = (0::$self.nnz).filter(i => $arg1(array_apply(data,i)))
val indices = densevector_raw_data(selected)
val removed = $self.nnz - selected.length
sparsevector_alloc_raw($self.length-removed, $self.isRow, array_map[Int,T](indices, i => array_apply(data, i)), indices, selected.length)"""
        }

      infix ("foreachnz") ((T ==> MUnit) :: MUnit) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.nz.foreach { $arg1 }"""
}

      infix ("findnz") ((T ==> MBoolean) :: IndexVector) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val data = sparsevector_raw_data($self)
val indices = sparsevector_raw_indices($self)
val selected = (0::$self.nnz).filter(i => $arg1(array_apply(data, i)))
IndexVector(selected.map(i => array_apply(indices, i)))"""
        }

      infix ("countnz") ((T ==> MBoolean) :: MInt) implements composite {
      val self = quotedArg("self")
      val arg1 = quotedArg(1)
      s"""$self.nz.count{$arg1}"""
    }
    }

    addSparseVectorCommonOps(SparseVector, T)

    // should we add primitive sparse op combinations? the extra combinations kill our compile times, but the trade-off is the loss of flexible syntax with sparse math.
  }
}
