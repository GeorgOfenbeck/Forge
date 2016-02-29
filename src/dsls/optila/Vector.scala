package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait VectorOps {
  this: OptiLADSL =>

  /**
   * Code generation can be an alternative to subtyping for achieving code re-use:
   *
   * This interface represents a convenient set of vector accessor functions.
   * They require v to define length, isRow, slice and apply, and v must be a ParallelCollection (which is checked at Forge stage-time)
   *
   * The trade-off here is compile-time vs. run-time: we are generating more code, but do not require an implicit conversion (which
   * previously copied into a DenseVector) to expose the common API. Note that in the Delite case the conversion should fuse anyway,
   * resulting in no run-time overhead. However, in the library-case, it will certainly hurt.
   */
  def addVectorCommonOps(v: Rep[DSLType], T: Rep[DSLType]) {
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val SparseVector = lookupTpe("SparseVector")
    val SparseVectorView = lookupTpe("SparseVectorView")
    val Tuple2 = lookupTpe("Tup2")
    val B = tpePar("B")
    val R = tpePar("R")

    // have to be careful about the type argument name we use in single and composite since T is being passed in
    // We splice this name into blocks using the escaped \$ to inform the preprocessor that the value already exists.
    val TT = T.name

    // we can also perform bulk operations generically, returning a DenseVector result for each operation
    // Arith is only required if T is actually a tpePar here, so we need to be careful.
    if (!isTpePar(T)) compiler (v) ("zeroT", Nil, Nil :: T) implements composite {
  s"""0.asInstanceOf[$TT]"""
}

    // We need to be a little careful using empty for zero here. With nested vectors, the zero vector
    // will not be the correct dimension. This implementation relies on the fact that DeliteOps will not
    // attempt to reduce with the zero element unless the collection is empty, in which case the behavior
    // is still not correct (but should probably throw an exception instead of silently returning []).
    val AZ = if (isTpePar(T)) (List(TArith(asTpePar(T))), "implicitly[Arith[T]].empty") else (Nil, "zeroT")
    val A = AZ._1; val Z = AZ._2; // can't use capital letters with tuple return pattern matching
    val O = if (isTpePar(T)) List(TOrdering(asTpePar(T))) else Nil
    val S = if (isTpePar(T)) List(TStringable(asTpePar(T))) else Nil
    val V = if (isTpePar(T)) v(T) else v

    val VectorCommonOps = withTpe(v)
    VectorCommonOps {
      /**
       * Conversions
       */
      infix ("toBoolean") (Nil :: DenseVector(MBoolean), ("conv",T ==> MBoolean)) implements composite {
        s"""self.map(conv)"""
      }
      infix ("toDouble") (Nil :: DenseVector(MDouble), ("conv",T ==> MDouble)) implements composite {
        s"""self.map(conv)"""
      }
      infix ("toFloat") (Nil :: DenseVector(MFloat), ("conv",T ==> MFloat)) implements composite {
        s"""self.map(conv)"""
      }
      infix ("toInt") (Nil :: DenseVector(MInt), ("conv",T ==> MInt)) implements composite {
  s"""self.map(conv)"""
}

      /**
       * Accessors
       */
      infix ("indices") (Nil :: IndexVector) implements composite {
        val self = quotedArg("self")
        s"""IndexVector(unit(0), $self.length, $self.isRow)"""
      }
      infix ("isEmpty") (Nil :: MBoolean) implements composite {
        val self = quotedArg("self")
        s"""$self.length == 0"""
      }
      infix ("first") (Nil :: T) implements composite {
        val self = quotedArg("self")
        s"""$self(0)"""
      }
      infix ("last") (Nil :: T) implements composite {
        val self = quotedArg("self")
        s"""$self($self.length - 1)"""
      }
      infix ("drop") (MInt :: V) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.slice($arg1, $self.length)"""
      }
      infix ("take") (MInt :: V) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.slice(0, $arg1)"""
      }
      infix ("contains") (T :: MBoolean) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""var found = false
var i = 0
while (i < $self.length && !found) {
  if ($self(i) == $arg1) {
    found = true
  }
  i += 1
}
found"""
        }

      infix ("histogram") (Nil :: MHashMap(T,MInt)) implements composite {
          val self = quotedArg("self")
          s"""$self.groupByReduce(e => e, v => 1, (a: Rep[Int],b: Rep[Int]) => a+b)"""
        }

      infix ("distinct") (Nil :: DenseVector(T)) implements composite {
          val self = quotedArg("self")
          s"""val elements = $self.histogram
densevector_fromarray(elements.keys(), true)"""
        }

      infix ("mutable") (Nil :: DenseVector(T), effect = mutable, aliasHint = copies(0)) implements composite {
          val self = quotedArg("self")
          s"""val out = DenseVector[$TT]($self.length, $self.isRow)


for (i <- 0 until out.length) {
  out(i) = $self(i)
}
out"""
        }

      infix ("replicate") ((MInt,MInt) :: DenseMatrix(T)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""if ($self.isRow) {
  val out = DenseMatrix[$TT]($arg1, $arg2*$self.length)
  for (col <- 0 until $arg2*$self.length){
    val colToJ = col % $self.length
    for (rI <- 0 until $arg1) {
      out(rI, col) = $self(colToJ)
    }
  }
  out.unsafeImmutable
}
else {
  val out = DenseMatrix[$TT]($arg1*$self.length, $arg2)
  for (row <- 0 until $arg1*$self.length){
    val rowToI = row % $self.length
    for (cI <- 0 until $arg2) {
      out(row, cI) = $self(rowToI)
    }
  }
  out.unsafeImmutable
}"""
        }

      // we need two versions so that we can override toString in the lib, and use makeStr in Delite. sad.
      infix ("makeString") (Nil :: MString, S) implements single {
          val self = quotedArg("self")
          s"""var s = ""
if ($self.length == 0) {
  s = "[ ]"
}
else if ($self.isRow) {
  for (i <- 0 until $self.length - 1) {
    s = s + optila_padspace($self(i).makeStr)
  }
  s = s + optila_padspace($self($self.length-1).makeStr)
}
else {
  for (i <- 0 until $self.length - 1) {
    s = s + optila_padspace($self(i).makeStr) + "\\n"
  }
  s = s + optila_padspace($self($self.length-1).makeStr)
}
s"""
        }

      infix ("toString") (Nil :: MString) implements single {
          val self = quotedArg("self")
          s"""var s = ""
if ($self.length == 0) {
  s = "[ ]"
}
else if ($self.isRow) {
  for (i <- 0 until $self.length - 1) {
    s = s + optila_padspace(optila_fmt_str($self(i)))
  }
  s = s + optila_padspace(optila_fmt_str($self($self.length-1)))
}
else {
  for (i <- 0 until $self.length - 1) {
    s = s + optila_padspace(optila_fmt_str($self(i))) + "\\n"
  }
  s = s + optila_padspace(optila_fmt_str($self($self.length-1)))
}
s"""
        }

      infix ("pprint") (Nil :: MUnit, S, effect = simple) implements composite {
  val self = quotedArg("self")
  s"""println($self.makeStr + "\\n")"""
} // $self.toString doesn't work in Delite

      infix ("makeStrWithDelim") (("delim",MString) :: MString, S) implements composite {
          val self = quotedArg("self")
          s"""array_mkstring($self.toArray, delim)"""
        }

      /**
       * Math
       */

      // allow arbitrary rhs arguments as well.. are we getting carried away?
      for (rhs <- List(DenseVector(T),DenseVectorView(T))) {
        infix ("+") (rhs :: DenseVector(T), A) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self.zip($arg1) { (a,b) => a+b }"""
        }
        infix ("-") (rhs :: DenseVector(T), A) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self.zip($arg1) { (a,b) => a-b }"""
        }
        infix ("*") (rhs :: DenseVector(T), A) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self.zip($arg1) { (a,b) => a*b }"""
        }
        infix ("*:*") (rhs :: T, A) implements composite {
            val self = quotedArg("self")
            val arg1 = quotedArg(1)
            s"""fassert($self.length == $arg1.length, "dimension mismatch: vector dot product")
sum($self*$arg1)"""
          }
        infix ("**") (rhs :: DenseMatrix(T), A) implements composite {
            val self = quotedArg("self")
            val arg1 = quotedArg(1)
            s"""fassert(!$self.isRow && $arg1.isRow, "dimension mismatch: vector outer product")
val out = DenseMatrix[$TT]($self.length, $arg1.length)
for (i <- 0 until $self.length ){
  for (j <- 0 until $arg1.length ){
    out(i,j) = $self(i)*$arg1(j)
  }
}
out.unsafeImmutable"""
          }
        infix ("/") (rhs :: DenseVector(T), A) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.zip($arg1) { (a,b) => a/b }"""
      }
      }

      for (rhs <- List(SparseVector(T),SparseVectorView(T))) {
        infix ("+") (rhs :: DenseVector(T), A) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self + $arg1.toDense"""
        }
        infix ("-") (rhs :: DenseVector(T), A) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self - $arg1.toDense"""
        }
        infix ("*") (rhs :: SparseVector(T), A) implements composite {
          val arg1 = quotedArg(1)
          val self = quotedArg("self")
          s"""$arg1 * $self.toDense"""
        }
        infix ("*:*") (rhs :: T, A) implements composite {
          val arg1 = quotedArg(1)
          val self = quotedArg("self")
          s"""$arg1 *:* $self.toDense"""
        }
        infix ("**") (rhs :: DenseMatrix(T), A) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self ** $arg1.toDense"""
        }
        infix ("/") (rhs :: DenseVector(T), A) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self / $arg1.toDense"""
      }
      }

      for (rhs <- List(DenseVector(B),DenseVectorView(B))) {
        // infix ("+") (rhs :: DenseVector(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a+b })
        // infix ("-") (rhs :: DenseVector(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a-b })
        // infix ("*") (rhs :: DenseVector(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a*b })
        // infix ("/") (rhs :: DenseVector(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a/b })
        infix ("zip") (CurriedMethodSignature(List(List(rhs), List((T,B) ==> R)), DenseVector(R)), addTpePars = (B,R)) implements zip((T,B,R), (0,1), {
        val arg1 = quotedArg(2)
        s"""(a,b) => $arg1(a,b)"""
      })
      }

      infix ("+") (T :: DenseVector(T), A) implements composite {
        val arg1 = quotedArg(1)
        s"""self.map(e => e+$arg1)"""
      }
      infix ("-") (T :: DenseVector(T), A) implements composite {
        val arg1 = quotedArg(1)
        s"""self.map(e => e-$arg1)"""
      }
      infix ("*") (T :: DenseVector(T), A) implements composite {
        val arg1 = quotedArg(1)
        s"""self.map(e => e*$arg1)"""
      }
      infix ("*") (DenseMatrix(T) :: DenseVector(T), A) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""fassert($self.isRow, "dimension mismatch: vector * matrix")
$arg1.mapColsToVector { col => $self *:* col }"""
        }
      infix ("/") (T :: DenseVector(T), A) implements composite {
  val arg1 = quotedArg(1)
  s"""self.map(e => e/$arg1)"""
}

      infix ("abs") (Nil :: DenseVector(T), A) implements composite {
        s"""self.map(e => e.abs)"""
      }
      infix ("exp") (Nil :: DenseVector(T), A) implements composite {
        s"""self.map(e => e.exp)"""
      }
      infix ("log") (Nil :: DenseVector(T), A) implements composite {
        s"""self.map(e => e.log)"""
      }
      infix ("sum") (Nil :: T, A) implements composite {
        s"""self.reduce((a,b) => a+b )"""
      }
      infix ("prod") (Nil :: T, A) implements reduce(T, 0, {
        s"""unit(1.asInstanceOf[$TT])"""
      }, {
        s"""(a,b) => a*b"""
      })
      infix ("mean") (Nil :: MDouble, ("conv",T ==> MDouble)) implements composite {
  val self = quotedArg("self")
  s"""$self.map(conv).sum / $self.length"""
}


      /**
       * Ordering
       */
      val H = if (isTpePar(T)) List(THasMinMax(asTpePar(T))) else Nil
      val MinT = if (isTpePar(T)) "implicitly[HasMinMax[T]].min" else "implicitly[HasMinMax["+TT+"]].min"
      val MaxT = if (isTpePar(T)) "implicitly[HasMinMax[T]].max" else "implicitly[HasMinMax["+TT+"]].max"

      infix ("min") (Nil :: T, O ::: H) implements reduce(T, 0, MaxT, {
        s"""(a,b) => if (a < b) a else b"""
      })
      infix ("max") (Nil :: T, O ::: H) implements reduce(T, 0, MinT, {
  s"""(a,b) => if (a > b) a else b"""
})

      infix ("minIndex") (Nil :: MInt, O) implements composite {
        val self = quotedArg("self")
        s"""min_index_of($self.indices, $self)"""
      }
      infix ("maxIndex") (Nil :: MInt, O) implements composite {
  val self = quotedArg("self")
  s"""max_index_of($self.indices, $self)"""
}


      /**
       * Bulk
       */
      infix ("map") ((T ==> R) :: DenseVector(R), addTpePars = R) implements map((T,R), 0, {
        val arg1 = quotedArg(1)
        s"""e => $arg1(e)"""
      })
      infix ("reduce") (((T,T) ==> T) :: T, A) implements reduce(T, 0, Z, {
        val arg1 = quotedArg(1)
        s"""(a,b) => $arg1(a,b)"""
      })
      infix ("foreach") ((T ==> MUnit) :: MUnit) implements foreach(T, 0, {
        val arg1 = quotedArg(1)
        s"""e => $arg1(e)"""
      })
      infix ("forall") ((T ==> MBoolean) :: MBoolean) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""reduce_and($self.map($arg1))"""
      }
      infix ("find") ((T ==> MBoolean) :: IndexVector) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.indices.filter(i => $arg1($self(i)))"""
}

      val filterMap = v.name.toLowerCase + "_densevector_filter_map"
      compiler (filterMap) (((T ==> MBoolean), (T ==> R)) :: DenseVector(R), addTpePars = R) implements filter((T,R), 0, {
        val arg1 = quotedArg(1)
        s"""e => $arg1(e)"""
      }, {
        val arg1 = quotedArg(2)
        s"""e => $arg1(e)"""
      })
      infix ("count") ((T ==> MBoolean) :: MInt) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val x = $filterMap($self, $arg1, (e: Rep[$TT]) => 1)
if (x.length > 0) sum(x)
else 0"""
        }

      val partitionReturn = if (v.name == "IndexVector") IndexVector else DenseVector(T)
      infix ("partition") (("pred",(T ==> MBoolean)) :: Tuple2(partitionReturn,partitionReturn)) implements composite {
          val self = quotedArg("self")
          s"""val assignments = $self.map(pred).mutable
val partT = $self(assignments.find(e => e))
val partF = $self(assignments.find(e => !e))
pack((partT, partF))"""
        }

      infix ("flatMap") ((T ==> DenseVector(R)) :: DenseVector(R), addTpePars = R) implements flatMap((T,R), 0, {
  val arg1 = quotedArg(1)
  s"""e => $arg1(e)"""
})

      // TODO: implement with a DeliteOp
      infix ("scanLeft") (CurriedMethodSignature(List(List(("zero", R)), List((R,T) ==> R)), DenseVector(R)), addTpePars = R) implements composite {
          val self = quotedArg("self")
          val zero = quotedArg("zero")
          val arg1 = quotedArg(2)
          s"""val out = DenseVector[R]($self.length+1, $self.isRow)
out(0) = $zero
var i = 1
while (i < out.length) {
  out(i) = $arg1(out(i-1), $self(i-1))
  i += 1
}
out.unsafeImmutable"""
        }

      // TODO: implement with a DeliteOp
      infix ("scanRight") (CurriedMethodSignature(List(List(("zero", R)), List((T,R) ==> R)), DenseVector(R)), addTpePars = R) implements composite {
          val self = quotedArg("self")
          val zero = quotedArg("zero")
          val arg1 = quotedArg(2)
          s"""val out = DenseVector[R]($self.length+1, $self.isRow)
out(out.length-1) = $zero
var i = $self.length-1
while (i >= 0) {
  out(i) = $arg1($self(i), out(i+1))
  i -= 1
}
out.unsafeImmutable"""
        }

      infix ("prefixSum") (Nil :: DenseVector(T), A) implements composite {
  val self = quotedArg("self")
  s"""($self.scanLeft($Z)((a,b) => a+b)).drop(1)"""
}

      // TODO
      // infix ("groupBy") (((T ==> R)) :: DenseVector(DenseVector(T)))

      infix ("intersect") (DenseVector(T) :: DenseVector(T)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val hash = if ($self.length > $arg1.length) $arg1.histogram else $self.histogram
val other = if ($self.length > $arg1.length) $self.distinct else $arg1.distinct
other.filter(e => hash.contains(e))"""
        }

      /**
       * Data exchange
       */

      infix ("toArray") (Nil :: MArray(T)) implements composite {
          val self = quotedArg("self")
          s"""array_fromfunction($self.length, i => $self(i))"""
        }
    }
  }
}
