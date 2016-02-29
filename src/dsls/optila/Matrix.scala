package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait MatrixOps {
  this: OptiLADSL =>

  /**
   * This interface represents a convenient set of matrix accessor functions.
   * They require m to define numRows, numCols, size, getRow, getCol, slice and apply,
   * and m must be a ParallelCollection (which is checked at Forge stage-time).
   */
  def addMatrixCommonOps(m: Rep[DSLType], T: Rep[DSLType]) {
    val IndexVector = lookupTpe("IndexVector")
    val IndexWildcard = lookupTpe("IndexWildcard", stage = compile)
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseMatrixView = lookupTpe("DenseMatrixView")
    val SparseMatrix = lookupTpe("SparseMatrix")
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
    if (!isTpePar(T)) compiler (m) ("zeroT", Nil, Nil :: T) implements composite {
  s"""0.asInstanceOf[$TT]"""
}

    val AZ = if (isTpePar(T)) (List(TArith(asTpePar(T))), "implicitly[Arith[T]].empty") else (Nil, "zeroT")
    val A = AZ._1; val Z = AZ._2; // can't use capital letters with tuple return pattern matching
    val O = if (isTpePar(T)) List(TOrdering(asTpePar(T))) else Nil
    val S = if (isTpePar(T)) List(TStringable(asTpePar(T))) else Nil
    val M = if (isTpePar(T)) m(T) else m

    val MatrixCommonOps = withTpe(m)
    MatrixCommonOps {
      /**
       * Conversions
       */
      infix ("toBoolean") (Nil :: DenseMatrix(MBoolean), ("conv",T ==> MBoolean)) implements composite {
        val self = quotedArg("self")
        s"""$self.map(conv)"""
      }
      infix ("toDouble") (Nil :: DenseMatrix(MDouble), ("conv",T ==> MDouble)) implements composite {
        val self = quotedArg("self")
        s"""$self.map(conv)"""
      }
      infix ("toFloat") (Nil :: DenseMatrix(MFloat), ("conv",T ==> MFloat)) implements composite {
        val self = quotedArg("self")
        s"""$self.map(conv)"""
      }
      infix ("toInt") (Nil :: DenseMatrix(MInt), ("conv",T ==> MInt)) implements composite {
  val self = quotedArg("self")
  s"""$self.map(conv)"""
}

      /**
       * Accessors
       */
      infix ("apply") (MInt :: DenseVectorView(T)) implements redirect {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.getRow($arg1)"""
}

      // orientation of IndexVector in apply does not matter - use getCols or 2d apply to slice cols. This is so we can use n::m syntax
      // to slice rows, while still retaining our convention of row vectors being the default (e.g. for matrix construction).
      // TODO: if the IndexVector is continuous, we should slice (and return a view) instead of copy.
      infix ("apply") (IndexVector :: DenseMatrix(T)) implements redirect {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.getRows($arg1)"""
      }
      infix ("apply") ((IndexVector, IndexWildcard) :: DenseMatrix(T)) implements redirect {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.getRows($arg1)"""
      }
      infix ("apply") ((("rows", IndexVector), ("cols", IndexVector)) :: DenseMatrix(T)) implements composite {
          val self = quotedArg("self")
          s"""(rows, cols) { (i,j) => $self(i,j) }"""
        }
      infix ("apply") ((IndexWildcard, IndexVector) :: DenseMatrix(T)) implements redirect {
  val self = quotedArg("self")
  val arg1 = quotedArg(2)
  s"""$self.getCols($arg1)"""
}

      infix ("indices") (Nil :: IndexVector) implements composite {
        val self = quotedArg("self")
        s"""IndexVector(0, $self.size)"""
      }
      infix ("rowIndices") (Nil :: IndexVector) implements composite {
        val self = quotedArg("self")
        s"""IndexVector(0, $self.numRows, false)"""
      }
      infix ("colIndices") (Nil :: IndexVector) implements composite {
  val self = quotedArg("self")
  s"""IndexVector(0, $self.numCols)"""
}

      infix ("getRows") (IndexVector :: DenseMatrix(T)) implements composite {
          val arg1 = quotedArg(1)
          val self = quotedArg("self")
          s"""if ($arg1.length == 0) densematrix_fromarray[T](array_empty_imm[T](0), 0, $self.numCols)
else {
  var z = $self 
  ($arg1, *) { i => z(i) }
}"""
        }
      infix ("getCols") (IndexVector :: DenseMatrix(T)) implements composite {
          val arg1 = quotedArg(1)
          val self = quotedArg("self")
          s"""if ($arg1.length == 0) densematrix_fromarray[T](array_empty_imm[T](0), $self.numRows, 0)
else {
  var z = $self 
  (*, $arg1) { j => z.getCol(j) }
}"""
        }

      infix ("sliceRows") ((("start",MInt),("end",MInt)) :: DenseMatrixView(T)) implements composite {
        val self = quotedArg("self")
        s"""$self.slice(start, end, 0, $self.numCols)"""
      }
      infix ("sliceCols") ((("start",MInt),("end",MInt)) :: DenseMatrixView(T)) implements composite {
  val self = quotedArg("self")
  s"""$self.slice(0, $self.numRows, start, end)"""
}

      /**
       * Miscellaneous
       */
      // $self.toString doesn't work in Delite, since there is no 'self' instance
      infix ("pprint") (Nil :: MUnit, S, effect = simple) implements composite {
  val self = quotedArg("self")
  s"""println($self.makeStr + "\\n")"""
}

      infix ("makeDimsStr") (Nil :: MString) implements single {
          val self = quotedArg("self")
          s"""$self.numRows + " x " + $self.numCols"""
        }

      infix ("makeString") (Nil :: MString, S) implements single {
          val self = quotedArg("self")
          s"""var s = ""
if ($self == null) {
  s = "null"
}
else if ($self.numRows == 0) {
  s = "[ ]"
}
else {
  for (i <- 0 until $self.numRows-1) {
    s = s + $self(i).makeStr + "\\n"
  }
  s = s + $self($self.numRows-1).makeStr
}
s"""
        }

      infix ("toString") (Nil :: MString) implements single {
          val self = quotedArg("self")
          s"""var s = ""
if ($self == null) {
  s = "null"
}
else if ($self.numRows == 0) {
  s = "[ ]"
}
else {
  for (i <- 0 until $self.numRows-1) {
    s = s + densevectorview_tostring($self(i)) + "\\n"
  }
  s = s + densevectorview_tostring($self($self.numRows-1))
}
s"""
        }

      infix ("t") (Nil :: DenseMatrix(T)) implements composite {
  val self = quotedArg("self")
  s"""(0::$self.numCols, 0::$self.numRows) { (i,j) => $self(j, i) }"""
}

      infix ("mutable") (Nil :: DenseMatrix(T), effect = mutable, aliasHint = copies(0)) implements composite {
           val self = quotedArg("self")
           s"""val out = DenseMatrix[$TT]($self.numRows, $self.numCols)
for (i <- 0 until $self.numRows) {
  for (j <- 0 until $self.numCols) {
    out(i,j) = $self(i,j)
  }
}
out"""
         }

     infix ("replicate") ((MInt,MInt) :: DenseMatrix(T)) implements composite {
         val arg1 = quotedArg(1)
         val self = quotedArg("self")
         val arg2 = quotedArg(2)
         s"""val out = DenseMatrix[$TT]($arg1*$self.numRows, $arg2*$self.numCols)
for (ii <- 0 until $arg1) {
  for (i <- 0 until $self.numRows) {
    for (jj <- 0 until $arg2) {
      for (j <- 0 until $self.numCols) {
 out(ii*$self.numRows+i, jj*$self.numCols+j) = $self(i,j)
      }
    }
  }
}
out.unsafeImmutable"""
       }


     /**
      * Math
      */
     // TODO: inverse
     infix ("+") (DenseMatrix(T) :: DenseMatrix(T), A) implements composite {
       val self = quotedArg("self")
       val arg1 = quotedArg(1)
       s"""$self.zip($arg1) { (a,b) => a+b }"""
     }
     infix ("+") (T :: DenseMatrix(T), A) implements composite {
       val self = quotedArg("self")
       val arg1 = quotedArg(1)
       s"""$self.map(e => e+$arg1)"""
     }
     // infix ("+") (DenseMatrix(B) :: DenseMatrix(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a+b })

     infix ("-") (DenseMatrix(T) :: DenseMatrix(T), A) implements composite {
       val self = quotedArg("self")
       val arg1 = quotedArg(1)
       s"""$self.zip($arg1) { (a,b) => a-b }"""
     }
     infix ("-") (T :: DenseMatrix(T), A) implements composite {
       val self = quotedArg("self")
       val arg1 = quotedArg(1)
       s"""$self.map(e => e-$arg1)"""
     }
     // infix ("-") (DenseMatrix(B) :: DenseMatrix(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a-b })

     infix ("*:*") (DenseMatrix(T) :: DenseMatrix(T), A) implements composite {
       val self = quotedArg("self")
       val arg1 = quotedArg(1)
       s"""$self.zip($arg1) { (a,b) => a*b }"""
     }
     // infix ("*:*") (DenseMatrix(B) :: DenseMatrix(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a*b })
     infix ("*") (T :: DenseMatrix(T), A) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.map(e => e*$arg1)"""
}

     infix ("*") (DenseMatrix(T) :: DenseMatrix(T), A) implements composite {
         val self = quotedArg("self")
         val arg1 = quotedArg(1)
         s"""fassert($self.numCols == $arg1.numRows, "dimension mismatch: matrix multiply (lhs: " + $self.makeDimsStr + ", rhs: " + $arg1.makeDimsStr + ")")

if ($self.numRows == 0) DenseMatrix[T]()
else {
  var z = $self
  (0::z.numRows, *) { i =>
    $arg1.mapColsToVector { c => z(i) *:* c }
  }
}"""
       }

     infix ("*") (SparseMatrix(T) :: DenseMatrix(T), A) implements composite {
         val self = quotedArg("self")
         val arg1 = quotedArg(1)
         s"""fassert($self.numCols == $arg1.numRows, "dimension mismatch: matrix multiply (lhs: " + $self.makeDimsStr + ", rhs: " + $arg1.makeDimsStr + ")")

($arg1.t*$self.t).t"""
       }

     for (rhs <- List(DenseVector(T), SparseVector(T))) {
       infix ("*") (rhs :: DenseVector(T), A) implements composite {
           val self = quotedArg("self")
           val arg1 = quotedArg(1)
           s"""fassert($self.numCols == $arg1.length && !$arg1.isRow, "dimension mismatch: matrix * vector")
val out = (0::$self.numRows) { i => $self(i) *:* $arg1 }
out.t"""
         }
     }

     infix ("/") (DenseMatrix(T) :: DenseMatrix(T), A) implements composite {
       val self = quotedArg("self")
       val arg1 = quotedArg(1)
       s"""$self.zip($arg1) { (a,b) => a/b }"""
     }
     infix ("/") (T :: DenseMatrix(T), A) implements composite {
       val self = quotedArg("self")
       val arg1 = quotedArg(1)
       s"""$self.map(e => e/$arg1)"""
     }
     // infix ("/") (DenseMatrix(B) :: DenseMatrix(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a/b })

     // Dense-sparse point-wise math
     infix ("+") (SparseMatrix(T) :: DenseMatrix(T), A) implements composite {
       val self = quotedArg("self")
       val arg1 = quotedArg(1)
       s"""$self + $arg1.toDense"""
     }
     infix ("-") (SparseMatrix(T) :: DenseMatrix(T), A) implements composite {
       val self = quotedArg("self")
       val arg1 = quotedArg(1)
       s"""$self - $arg1.toDense"""
     }
     infix ("*:*") (SparseMatrix(T) :: DenseMatrix(T), A) implements composite {
       val self = quotedArg("self")
       val arg1 = quotedArg(1)
       s"""$self *:* $arg1.toDense"""
     }
     infix ("/") (SparseMatrix(T) :: DenseMatrix(T), A) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self / $arg1.toDense"""
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
       s"""$self.map(conv).sum / $self.size"""
     }
     infix ("abs") (Nil :: DenseMatrix(T), A) implements composite {
       val self = quotedArg("self")
       s"""$self.map(e => e.abs)"""
     }
     infix ("exp") (Nil :: DenseMatrix(T), A) implements composite {
       val self = quotedArg("self")
       s"""$self.map(e => e.exp)"""
     }
     infix ("log") (Nil :: DenseMatrix(T), A) implements composite {
  val self = quotedArg("self")
  s"""$self.map(e => e.log)"""
}

     infix ("sumRows") (Nil :: DenseVector(T), A) implements composite {
       val self = quotedArg("self")
       s"""$self.mapRowsToVector { row => sum(row) }"""
     }
     infix ("sumCols") (Nil :: DenseVector(T), A) implements composite {
  val self = quotedArg("self")
  s"""$self.mapColsToVector { col => sum(col) }"""
}


     /**
      * Ordering
      */
     val H = if (isTpePar(T)) List(THasMinMax(asTpePar(T))) else Nil
     val MinT = if (isTpePar(T)) "implicitly[HasMinMax[T]].min" else "implicitly[HasMinMax["+TT+"]].min"
     val MaxT = if (isTpePar(T)) "implicitly[HasMinMax[T]].max" else "implicitly[HasMinMax["+TT+"]].max"

     infix ("minRows") (Nil :: DenseVector(T), O ::: H) implements composite {
       val self = quotedArg("self")
       s"""$self.mapRowsToVector { row => min(row) }"""
     }
     infix ("minCols") (Nil :: DenseVector(T), O ::: H) implements composite {
       val self = quotedArg("self")
       s"""$self.mapColsToVector { col => min(col) }"""
     }
     infix ("maxRows") (Nil :: DenseVector(T), O ::: H) implements composite {
       val self = quotedArg("self")
       s"""$self.mapRowsToVector { row => max(row) }"""
     }
     infix ("maxCols") (Nil :: DenseVector(T), O ::: H) implements composite {
  val self = quotedArg("self")
  s"""$self.mapColsToVector { col => max(col) }"""
}

     infix ("min") (Nil :: T, O ::: H) implements reduce(T, 0, MaxT, {
       s"""(a,b) => if (a < b) a else b"""
     })
     infix ("max") (Nil :: T, O ::: H) implements reduce(T, 0, MinT, {
  s"""(a,b) => if (a > b) a else b"""
})

     // TODO: switch to reduce when TupleReduce is generalized
     infix ("minIndex") (Nil :: Tuple2(MInt,MInt), O) implements composite {
         val self = quotedArg("self")
         s"""var min = $self(0,0)
var minRow = 0
var minCol = 0
for (i <- 0 until $self.numRows) {
  for (j <- 0 until $self.numCols) {
    if ($self(i,j) < min) {
      min = $self(i,j)
      minRow = i
      minCol = j
    }
  }
}
pack((minRow,minCol))"""
       }

     infix ("maxIndex") (Nil :: Tuple2(MInt,MInt), O) implements composite {
         val self = quotedArg("self")
         s"""var max = $self(0,0)
var maxRow = 0
var maxCol = 0
for (i <- 0 until $self.numRows) {
  for (j <- 0 until $self.numCols) {
    if ($self(i,j) > max) {
      max = $self(i,j)
      maxRow = i
      maxCol = j
    }
  }
}
pack((maxRow,maxCol))"""
       }

     /**
      * Bulk
      */
      infix ("map") ((T ==> R) :: DenseMatrix(R), addTpePars = R) implements map((T,R), 0, {
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
      infix ("zip") (CurriedMethodSignature(List(List(DenseMatrix(B)), List((T,B) ==> R)), DenseMatrix(R)), addTpePars = (B,R)) implements zip((T,B,R), (0,1), {
        val arg1 = quotedArg(2)
        s"""(a,b) => $arg1(a,b)"""
      })
      infix ("count") ((T ==> MBoolean) :: MInt) implements mapReduce((T,MInt), 0, {
  s"""e => 1"""
}, {
  s"""0"""
}, {
  s"""(a,b) => a+b"""
}, Some({
  val arg1 = quotedArg(1)
  s"""e => $arg1(e)"""
}))

      infix ("mapRowsToVector") ((DenseVectorView(T) ==> R) :: DenseVector(R), addTpePars = R) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self.rowIndices.map(i => $arg1($self(i)))"""
        }
      infix ("mapColsToVector") ((DenseVectorView(T) ==> R) :: DenseVector(R), addTpePars = R) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self.colIndices.map(i => $arg1($self.getCol(i)))"""
        }

      infix ("findRows") ((DenseVectorView(T) ==> MBoolean) :: IndexVector) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self.rowIndices.filter(i => $arg1($self(i)))"""
        }
      infix ("findCols") ((DenseVectorView(T) ==> MBoolean) :: IndexVector) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self.colIndices.filter(i => $arg1($self.getCol(i)))"""
        }

      infix ("filterRows") ((DenseVectorView(T) ==> MBoolean) :: DenseMatrix(T)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self($self.findRows($arg1))"""
        }
      infix ("filterCols") ((DenseVectorView(T) ==> MBoolean) :: DenseMatrix(T)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self.getCols($self.findCols($arg1))"""
        }

      infix ("foreachRow") ((DenseVectorView(T) ==> MUnit) :: MUnit, effect = simple) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self.rowIndices foreach { i => $arg1($self(i)) }"""
        }
      infix ("foreachCol") ((DenseVectorView(T) ==> MUnit) :: MUnit, effect = simple) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""$self.colIndices foreach { i => $arg1($self.getCol(i)) }"""
        }

      infix ("mapRows") ((DenseVectorView(T) ==> DenseVector(R)) :: DenseMatrix(R), addTpePars = R) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""($self.rowIndices, *) { i =>
  $arg1($self(i))
}"""
        }
      infix ("mapCols") ((DenseVectorView(T) ==> DenseVector(R)) :: DenseMatrix(R), addTpePars = R) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""(*, $self.colIndices) { j =>
  $arg1($self.getCol(j))
}"""
        }

      // in order to express this with the current Delite ops, we have to convert the matrix to a vector of vectors,
      // which is unfortunate. A vector of vectorviews would be somewhat better, but since Delite reduce requires
      // (A,A) => A, we cannot yet express that operation in parallel with converting each vectors.
      // however, the map and reduce here should fuse, eliminating the overhead in the Delite version.
      infix ("reduceRows") (((DenseVector(T),DenseVector(T)) ==> DenseVector(T)) :: DenseVector(T), A) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val vv = $self.rowIndices.map(i => $self(i).toDense)
vv.reduce((a,b) => $arg1(a,b))"""
        }

      infix ("reduceCols") (((DenseVector(T),DenseVector(T)) ==> DenseVector(T)) :: DenseVector(T), A) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val vv = $self.colIndices.map(i => $self.getCol(i).toDense)
vv.reduce((a,b) => $arg1(a,b))"""
        }

    }
  }
}

