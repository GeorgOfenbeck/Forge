package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait DenseMatrixOps {
  this: OptiLADSL =>

  def importDenseMatrixOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")

    val DenseVector = lookupTpe("DenseVector")
    val IndexVector = lookupTpe("IndexVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseMatrixView = lookupTpe("DenseMatrixView")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val Tuple2 = lookupTpe("Tup2")

    // data fields
    data(DenseMatrix, ("_numRows", MInt), ("_numCols", MInt), ("_data", MArray(T)))

    // static methods
    static (DenseMatrix) ("apply", T, (MInt, MInt) :: DenseMatrix(T), effect = mutable) implements allocates(DenseMatrix, {
      val arg1 = quotedArg(0)
      s"""$arg1"""
    }, {
      val arg1 = quotedArg(1)
      s"""$arg1"""
    }, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""array_empty[T]($arg1*$arg2)"""
    })
    static (DenseMatrix) ("apply", T, (MArray(T), MInt, MInt) :: DenseMatrix(T)) implements composite {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  val arg3 = quotedArg(2)
  s"""densematrix_fromarray($arg1, $arg2, $arg3)"""
}

    // matrix from vector of vectors
    for (v <- List(DenseVector(T),DenseVectorView(T))) {
      static (DenseMatrix) ("apply", T, (DenseVector(v)) :: DenseMatrix(T)) implements composite {
          val arg1 = quotedArg(0)
          s"""if ($arg1.length == 0) densematrix_fromarray[T](array_empty_imm[T](0), 0, 0)
else {
  var z = $arg1 
  (0::z.length, *) { i => z(i) }
}"""
        }
    }

    // matrix from variable number of vectors (rows)
    static (DenseMatrix) ("apply", T, varArgs(DenseVector(T)) :: DenseMatrix(T)) implements composite {
        val arg1 = quotedArg(0)
        s"""val out = DenseMatrix[T](0, 0)

for (i: Int <- scala.collection.immutable.Range(0,$arg1.length)) {
  out <<= $arg1(i)
}
out.unsafeImmutable"""
      }

    // block matrix constructor
    // we can't reuse "apply", because it is ambiguous with the row constructor above
    static (DenseMatrix) ("block", T, varArgs(DenseVector(DenseMatrix(T))) :: DenseMatrix(T)) implements single {
        val arg1 = quotedArg(0)
        s"""var totalNumRows = 0
var totalNumCols = 0
val seq = array_fromseq($arg1)
for (i <- 0 until array_length(seq)) {
  val subMatRow = array_apply(seq,i)
  val numRows = subMatRow(0).numRows
  var numCols = subMatRow(0).numCols
  for (j <- 1 until subMatRow.length) {
    fassert(subMatRow(j).numRows == numRows, "dimension mismatch in block matrix constructor: " + subMatRow(j).numRows + " != " + numRows)
    numCols += subMatRow(j).numCols
  }
  totalNumRows += numRows
  if (i == 0) {
    totalNumCols = numCols
  }
  else {
    fassert(numCols == totalNumCols, "dimension mismatch in block matrix constructor: row " + i + " has wrong number of cols " + numCols + " (expected " + totalNumCols + ")")
  }
  ()
}


val out = DenseMatrix[T](totalNumRows, totalNumCols)
var rowIdx = 0
var colIdx = 0
for (i <- 0 until array_length(seq)) {
  val subMatRow = array_apply(seq,i)
  colIdx = 0
  for (j <- 0 until subMatRow.length) {
    for (k <- 0 until subMatRow(j).numRows) {
for (l <- 0 until subMatRow(j).numCols) {
  out(rowIdx+k, colIdx+l) = subMatRow(j).apply(k,l)
}
    }
    colIdx += subMatRow(j).numCols
  }
  rowIdx += subMatRow(0).numRows
}

out.unsafeImmutable"""
      }

    static (DenseMatrix) ("diag", T withBound TArith, (MInt, DenseVector(T)) :: DenseMatrix(T)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""densematrix_fromfunc($arg1, $arg1, (i,j) =>
if (i == j) $arg2(i)
else implicitly[Arith[T]].empty
    )"""
      }
    static (DenseMatrix) ("identity", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""densematrix_fromfunc($arg1, $arg2, (i,j) =>
if (i == j) 1.0
else 0.0
    )"""
      }
    static (DenseMatrix) ("identity", Nil, MInt :: DenseMatrix(MDouble)) implements redirect {
  val arg1 = quotedArg(0)
  s"""DenseMatrix.identity($arg1,$arg1)"""
}

    // helpers
    direct (DenseMatrix) ("densematrix_fromarray", T, (MArray(T), MInt, MInt) :: DenseMatrix(T)) implements allocates(DenseMatrix, {
      val arg1 = quotedArg(1)
      s"""$arg1"""
    }, {
      val arg1 = quotedArg(2)
      s"""$arg1"""
    }, {
      val arg1 = quotedArg(0)
      s"""$arg1"""
    })
    direct (DenseMatrix) ("densematrix_fromfunc", T, (MInt, MInt, (MInt,MInt) ==> T) :: DenseMatrix(T)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        val arg3 = quotedArg(2)
        s"""(0::$arg1, 0::$arg2) { (i,j) => $arg3(i,j) }"""
      }
    compiler (DenseMatrix) ("matrix_shapeindex", Nil, (("idx",MLong),("numCols",MLong)) :: Tuple2(MInt,MInt)) implements composite {
        s"""val rowIndex = idx / numCols
val colIndex = idx % numCols
pack(rowIndex.toInt,colIndex.toInt)"""
      }

    val K = tpePar("K")
    val V = tpePar("V")

    compiler (DenseMatrix) ("densematrix_grouprowsby_helper", (T,K,V), (IndexVector, DenseMatrix(T), DenseVectorView(T) ==> K, DenseVectorView(T) ==> V) :: MHashMap(K, MArrayBuffer(V))) implements groupBy((MInt,K,V), 0, {
      val arg1 = quotedArg(2)
      val arg2 = quotedArg(1)
      s"""i => $arg1($arg2.getRow(i))"""
    }, {
      val arg1 = quotedArg(3)
      val arg2 = quotedArg(1)
      s"""i => $arg1($arg2.getRow(i))"""
    })
    compiler (DenseMatrix) ("densematrix_groupcolsby_helper", (T,K,V), (IndexVector, DenseMatrix(T), DenseVectorView(T) ==> K, DenseVectorView(T) ==> V) :: MHashMap(K, MArrayBuffer(V))) implements groupBy((MInt,K,V), 0, {
  val arg1 = quotedArg(2)
  val arg2 = quotedArg(1)
  s"""i => $arg1($arg2.getCol(i))"""
}, {
  val arg1 = quotedArg(3)
  val arg2 = quotedArg(1)
  s"""i => $arg1($arg2.getCol(i))"""
})

    static (DenseMatrix) ("zeros", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_fromfunc($arg1, $arg2, (i,j) => 0.0 )"""
    }
    static (DenseMatrix) ("zerosf", Nil, (MInt,MInt) :: DenseMatrix(MFloat)) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_fromfunc($arg1, $arg2, (i,j) => 0f )"""
    }
    static (DenseMatrix) ("ones", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_fromfunc($arg1, $arg2, (i,j) => 1.0 )"""
    }
    static (DenseMatrix) ("onesf", Nil, (MInt,MInt) :: DenseMatrix(MFloat)) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_fromfunc($arg1, $arg2, (i,j) => 1f )"""
    }
    static (DenseMatrix) ("rand", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_fromfunc($arg1, $arg2, (i,j) => random[Double] )"""
    }
    static (DenseMatrix) ("randf", Nil, (MInt,MInt) :: DenseMatrix(MFloat)) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_fromfunc($arg1, $arg2, (i,j) => random[Float] )"""
    }
    static (DenseMatrix) ("randn", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_fromfunc($arg1, $arg2, (i,j) => randomGaussian )"""
    }
    static (DenseMatrix) ("randnf", Nil, (MInt,MInt) :: DenseMatrix(MFloat)) implements composite {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""densematrix_fromfunc($arg1, $arg2, (i,j) => randomGaussian.toFloat )"""
}

    // these are provided for convenience for MATLAB-ians
    direct (DenseMatrix) ("diag", T, DenseMatrix(T) :: DenseVector(T)) implements redirect {
      val arg1 = quotedArg(0)
      s"""$arg1.diag"""
    }
    direct (DenseMatrix) ("triu", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements redirect {
      val arg1 = quotedArg(0)
      s"""$arg1.triu"""
    }
    direct (DenseMatrix) ("tril", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements redirect {
  val arg1 = quotedArg(0)
  s"""$arg1.tril"""
}

    // a non-type-safe way of passing the metadata required to allocate a DenseMatrix in a parallel op
    // ideally we would encode this is as a type class, but it's not clear we would get an instance of this type class in dc_alloc
    val CR = tpePar("CR")
    compiler (DenseMatrix) ("densematrix_dc_alloc", (R,CR), (CR,MInt) :: DenseMatrix(R)) implements composite {
        val arg1 = quotedArg(0)
        s"""val simpleName = manifest[CR].erasure.getSimpleName
val (numRows, numCols) = simpleName match {
  case s if s.startsWith("DenseMatrixView") =>
    (densematrixview_numrows($arg1.asInstanceOf[Rep[DenseMatrixView[Any]]]), densematrixview_numcols($arg1.asInstanceOf[Rep[DenseMatrixView[Any]]]))
  case s if s.startsWith("DenseMatrix") =>
    (densematrix_numrows($arg1.asInstanceOf[Rep[DenseMatrix[Any]]]), densematrix_numcols($arg1.asInstanceOf[Rep[DenseMatrix[Any]]]))
}
DenseMatrix[R](numRows, numCols)"""
      }

    val DenseMatrixOps = withTpe (DenseMatrix)
    DenseMatrixOps {
      /**
       * Conversions
       */
      // This workaround is required for 2.11 for some reason (the matrix conversion implicit fails to
      // resolve for vector * matrix).
      mustInfixList :::= List("toFloat", "toDouble")

      // But on the other hand, infix is not working for these in 2.11.
      noInfixList :::= List("slice", "vview", "getRow", "getCol")

      infix ("flattenToVector") (Nil :: DenseVector(T)) implements composite {
          val self = quotedArg("self")
          s"""(0::$self.size) { i => densematrix_raw_apply($self, i) }"""
        }

      /**
       * Accessors
       */
      infix ("numRows") (Nil :: MInt) implements getter(0, "_numRows")
      infix ("numCols") (Nil :: MInt) implements getter(0, "_numCols")
      infix ("size") (Nil :: MInt) implements composite {
  val self = quotedArg("self")
  s"""$self.numRows*$self.numCols"""
}

      compiler ("densematrix_index") ((MInt,MInt) :: MInt) implements composite {
        val arg1 = quotedArg(1)
        val self = quotedArg("self")
        val arg2 = quotedArg(2)
        s"""$arg1*$self.numCols + $arg2"""
      }
      infix ("apply") ((MInt,MInt) :: T) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  val arg2 = quotedArg(2)
  s"""array_apply(densematrix_raw_data($self), densematrix_index($self,$arg1,$arg2))"""
}

      // vview, mview are "contains" because the view points-to the matrix; dereferencing the view returns the matrix.
      // it is not "extracts", because it is not returning any matrix elements.
      infix ("vview") ((("start", MInt), ("stride", MInt), ("length", MInt), ("isRow", MBoolean)) :: DenseVectorView(T), aliasHint = contains(0)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          val arg3 = quotedArg(3)
          val arg4 = quotedArg(4)
          s"""DenseVectorView[T](densematrix_raw_data($self), $arg1, $arg2, $arg3, $arg4)"""
        }
      label(lookupOp("vview"), "densematrix_vectorview")

      infix ("mview") ((("startRow", MInt), ("endRow", MInt), ("startCol", MInt), ("endCol", MInt)) :: DenseMatrixView(T), aliasHint = contains(0)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          val arg3 = quotedArg(3)
          val arg4 = quotedArg(4)
          s"""DenseMatrixView[T](densematrix_raw_data($self), $arg1, $arg2, $arg3, $arg4, $self.numRows, $self.numCols)"""
        }

      infix ("getRow") (MInt :: DenseVectorView(T)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.vview($arg1*$self.numCols, 1, $self.numCols, true)"""
      }
      infix ("getCol") (MInt :: DenseVectorView(T)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.vview($arg1, $self.numCols, $self.numRows, false)"""
}

      infix ("slice") ((("startRow",MInt),("endRow",MInt),("startCol",MInt),("endCol",MInt)) :: DenseMatrixView(T)) implements composite {
          val self = quotedArg("self")
          s"""$self.mview(startRow, endRow, startCol, endCol)"""
        }

      // TODO: generalize the following (and the static diag above) to kth diagonal
      // infix ("diag") (MethodSignature(List(("x",DenseMatrix(T)),("k",MInt,"unit(0)")), DenseVector(T)) implements composite ${
      infix ("diag") (Nil :: DenseVector(T)) implements composite {
          val self = quotedArg("self")
          s"""val dim = min($self.numRows, $self.numCols)
val indices = (0::dim) { i => i + i*$self.numCols }
indices.t map { i => densematrix_raw_apply($self,i) }"""
        }
      infix ("triu") (Nil :: DenseMatrix(T), TArith(T)) implements composite {
          val self = quotedArg("self")
          s"""(0::$self.numRows, 0::$self.numCols) { (i,j) =>
  if (i <= j) $self(i,j) else implicitly[Arith[T]].empty
}"""
        }
      infix ("tril") (Nil :: DenseMatrix(T), TArith(T)) implements composite {
          val self = quotedArg("self")
          s"""(0::$self.numRows, 0::$self.numCols) { (i,j) =>
  if (i >= j) $self(i,j) else implicitly[Arith[T]].empty
}"""
        }


      /**
       * Miscellaneous
       */
       infix ("Clone") (Nil :: DenseMatrix(T), aliasHint = copies(0)) implements composite {
           val self = quotedArg("self")
           s"""densematrix_fromarray(array_clone(densematrix_raw_data($self)), $self.numRows, $self.numCols)"""
         }

       infix ("toSparse") (Nil :: SparseMatrix(T)) implements composite {
            val self = quotedArg("self")
            s"""val sparseElements = $self.indices.map { i =>
  pack((densematrix_raw_apply($self, i), i / $self.numCols, i % $self.numCols))
}

SparseMatrix.fromElements(
  $self.numRows,
  $self.numCols,
  sparseElements.map(_._1),
  sparseElements.map(_._2),
  sparseElements.map(_._3)
)"""
          }


      /**
       * Data operations
       */
      compiler ("densematrix_raw_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("densematrix_set_raw_data") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", {
        val arg1 = quotedArg(1)
        s"""$arg1"""
      })
      compiler ("densematrix_set_numrows") (MInt :: MUnit, effect = write(0)) implements setter(0, "_numRows", {
        val arg1 = quotedArg(1)
        s"""$arg1"""
      })
      compiler ("densematrix_set_numcols") (MInt :: MUnit, effect = write(0)) implements setter(0, "_numCols", {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

      infix ("toArray") (Nil :: MArray(T)) implements composite {
  val self = quotedArg("self")
  s"""densematrix_raw_data($self.map(e => e))"""
}

      infix ("update") ((MInt,MInt,T) :: MUnit, effect = write(0)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  val arg2 = quotedArg(2)
  val arg3 = quotedArg(3)
  s"""array_update(densematrix_raw_data($self), densematrix_index($self,$arg1,$arg2), $arg3)"""
}

      for (rhs <- List(DenseVector(T),DenseVectorView(T))) {
        infix ("update") ((MInt,rhs) :: MUnit, effect = write(0)) implements composite {
            val self = quotedArg("self")
            val arg1 = quotedArg(1)
            val arg2 = quotedArg(2)
            s"""$self.updateRow($arg1, $arg2)"""
          }
        infix ("updateRow") ((MInt,rhs) :: MUnit, effect=write(0)) implements composite {
            val arg1 = quotedArg(2)
            val self = quotedArg("self")
            val arg2 = quotedArg(1)
            s"""(0::$arg1.length) foreach { j => $self($arg2,j) = $arg1(j) }"""
          }
        infix ("updateCol") ((MInt,rhs) :: MUnit, effect=write(0)) implements composite {
            val arg1 = quotedArg(2)
            val self = quotedArg("self")
            val arg2 = quotedArg(1)
            s"""(0::$arg1.length) foreach { i => $self(i,$arg2) = $arg1(i) }"""
          }
      }

      infix ("update") ((IndexVector,DenseMatrix(T)) :: MUnit, effect=write(0)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""$self.updateRows($arg1, $arg2)"""
        }

      infix ("updateRows") ((IndexVector,DenseMatrix(T)) :: MUnit, effect=write(0)) implements composite {
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          val self = quotedArg("self")
          s"""fassert($arg1.length == $arg2.numRows && $self.numCols == $arg2.numCols, "dimension mismatch in updateRows")
(0::$arg1.length) foreach { (i: Rep[Int]) =>
  val row: Rep[Int] = $arg1(i)
  $self.updateRow(row, $arg2(i))
}"""
        }

      infix ("updateCols") ((IndexVector,DenseMatrix(T)) :: MUnit, effect=write(0)) implements composite {
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          val self = quotedArg("self")
          s"""fassert($arg1.length == $arg2.numCols && $self.numRows == $arg2.numRows, "dimension mismatch in updateCols")
(0::$arg1.length) foreach { (i: Rep[Int]) =>
  val col: Rep[Int] = $arg1(i)
  $self.updateCol(col, $arg2.getCol(i))
}"""
        }

      infix ("<<") (DenseVector(T) :: DenseMatrix(T)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val out = DenseMatrix[T](0, 0)
out <<= $self
out <<= $arg1
out.unsafeImmutable"""
        }
      infix ("<<") (DenseMatrix(T) :: DenseMatrix(T)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val out = DenseMatrix[T](0, 0)
out <<= $self
out <<= $arg1
out.unsafeImmutable"""
        }
      infix ("<<|") (DenseVector(T) :: DenseMatrix(T)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val out = DenseMatrix[T](0, 0)
out.insertAllCols(0, $self)
out.insertCol($self.numCols, $arg1)
out.unsafeImmutable"""
        }
      infix ("<<|") (DenseMatrix(T) :: DenseMatrix(T)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val out = DenseMatrix[T](0, 0)
out.insertAllCols(0, $self)
out.insertAllCols($self.numCols, $arg1)
out.unsafeImmutable"""
        }
      infix ("<<=") (DenseVector(T) :: MUnit, effect = write(0)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.insertRow($self.numRows, $arg1)"""
      }
      infix ("<<=") (DenseMatrix(T) :: MUnit, effect = write(0)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.insertAllRows($self.numRows, $arg1)"""
      }
      infix ("<<|=") (DenseVector(T) :: MUnit, effect = write(0)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.insertCol($self.numCols, $arg1)"""
      }
      infix ("<<|=") (DenseMatrix(T) :: MUnit, effect = write(0)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.insertAllCols($self.numCols, $arg1)"""
}

      infix ("insertRow") ((("pos",MInt),("y",DenseVector(T))) :: MUnit, effect=write(0)) implements single {
          val pos = quotedArg("pos")
          val self = quotedArg("self")
          val y = quotedArg("y")
          s"""val idx = $pos*$self.numCols
if ($self.size == 0) densematrix_set_numcols($self, $y.length)
densematrix_insertspace($self, idx, $self.numCols)
val data = densematrix_raw_data($self)
for (i <- idx until idx+$self.numCols){
  array_update(data,i,$y(i-idx))
}
densematrix_set_numrows($self, $self.numRows+1)"""
        }
      infix ("insertAllRows") ((("pos",MInt),("xs",DenseMatrix(T))) :: MUnit, effect=write(0)) implements single {
          val pos = quotedArg("pos")
          val self = quotedArg("self")
          val xs = quotedArg("xs")
          s"""val idx = $pos*$self.numCols
if ($self.size == 0) densematrix_set_numcols($self, $xs.numCols)
val sz = $self.numCols*xs.numRows
densematrix_insertspace($self, idx, sz)
val data = densematrix_raw_data($self)
for (i <- idx until idx+sz){
  array_update(data,i,densematrix_raw_apply($xs, i-idx))
}
densematrix_set_numrows($self, $self.numRows+$xs.numRows)"""
        }
      infix ("insertCol") ((("pos",MInt),("y",DenseVector(T))) :: MUnit, effect=write(0)) implements single {
          val self = quotedArg("self")
          val y = quotedArg("y")
          val pos = quotedArg("pos")
          s"""val newCols = $self.numCols+1
if ($self.size == 0) densematrix_set_numrows($self, $y.length)
val outData = array_empty[T]($self.numRows*newCols)
for (i <- 0 until $self.numRows){
  var col = 0
  for (j <- 0 until newCols) {
    if (j == $pos){
      outData(i*newCols+j) = $y(i)
    }
    else{
      outData(i*newCols+j) = $self(i,col)
      col += 1
    }
  }
}
densematrix_set_raw_data($self, outData.unsafeImmutable)
densematrix_set_numcols($self, newCols)"""
        }
      infix ("insertAllCols") ((("pos",MInt),("xs",DenseMatrix(T))) :: MUnit, effect=write(0)) implements single {
          val self = quotedArg("self")
          val xs = quotedArg("xs")
          val arg1 = quotedArg(1)
          val pos = quotedArg("pos")
          s"""val newCols = $self.numCols+$xs.numCols
if ($self.size == 0) densematrix_set_numrows($self, $xs.numRows)
val outData = array_empty[T]($self.numRows*newCols)
for (i <- 0 until $self.numRows){
  var col = 0
  for (j <- 0 until newCols){
    if (j < $arg1 || j >= $pos+$xs.numCols){
      outData(i*newCols+j) = $self(i,col)
      col += 1
    }
    else{
      outData(i*newCols+j) = $xs(i,j-$pos)
    }
  }
}
densematrix_set_raw_data($self, outData.unsafeImmutable)
densematrix_set_numcols($self, newCols)"""
        }

      infix ("trim") (Nil :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          s"""val data = densematrix_raw_data($self)
if ($self.size < array_length(data)) {
  val d = array_empty[T]($self.size)
  array_copy(data, 0, d, 0, $self.size)
  densematrix_set_raw_data($self, d.unsafeImmutable)
}"""
        }

      infix ("removeRow") (("pos",MInt) :: MUnit, effect = write(0)) implements composite {
        val self = quotedArg("self")
        val pos = quotedArg("pos")
        s"""$self.removeRows($pos, 1)"""
      }
      infix ("removeCol") (("pos",MInt) :: MUnit, effect = write(0)) implements composite {
        val self = quotedArg("self")
        val pos = quotedArg("pos")
        s"""$self.removeCols($pos, 1)"""
      }
      infix ("removeRows") ((("pos",MInt),("num",MInt)) :: MUnit, effect=write(0)) implements composite {
          val pos = quotedArg("pos")
          val self = quotedArg("self")
          val num = quotedArg("num")
          s"""val idx = $pos*$self.numCols
val len = $num*$self.numCols
val data = densematrix_raw_data($self)
array_copy(data, idx + len, data, idx, $self.size - (idx + len))
densematrix_set_numrows($self, $self.numRows - $num)"""
        }
      infix ("removeCols") ((("pos",MInt),("num",MInt)) :: MUnit, effect=write(0)) implements composite {
          val self = quotedArg("self")
          val num = quotedArg("num")
          val pos = quotedArg("pos")
          s"""val newCols = $self.numCols-$num
val outData = array_empty[T]($self.numRows*newCols)
for (i <- 0 until $self.numRows){
  var col = 0
  for (j <- 0 until $self.numCols){
    if (j < $pos || j >= $pos+$num){
      outData(i*newCols+col) = $self(i,j)
      col += 1
    }
  }
}
densematrix_set_raw_data($self, outData.unsafeImmutable)
densematrix_set_numcols($self, newCols)"""
        }

      compiler ("densematrix_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single {
          val pos = quotedArg("pos")
          val self = quotedArg("self")
          val len = quotedArg("len")
          s"""fassert($pos >= 0 && $pos <= $self.size, "densematrix_insertspace: index out of bounds")
densematrix_ensureextra($self,$len)
val d = densematrix_raw_data($self)
array_copy(d, $pos, d, $pos + $len, $self.size - $pos)"""
        }
      compiler ("densematrix_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val extra = quotedArg("extra")
          s"""val data = densematrix_raw_data($self)
if (array_length(data) - $self.size < $extra) {
  densematrix_realloc($self, $self.size+$extra)
}"""
        }
      compiler ("densematrix_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          s"""val data = densematrix_raw_data($self)
var n = max(4, array_length(data) * 2)
while (n < minLen) n = n*2
val d = array_empty[T](n)
array_copy(data, 0, d, 0, $self.size)
densematrix_set_raw_data($self, d.unsafeImmutable)"""
        }


      /**
       * Math
       */

       infix ("+=") (DenseMatrix(T) :: MUnit, TArith(T), effect = write(0)) implements composite {
           val self = quotedArg("self")
           val arg1 = quotedArg(1)
           s"""$self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)+densematrix_raw_apply($arg1,i)) }"""
         }
       infix ("+=") (T :: MUnit, TArith(T), effect = write(0)) implements composite {
           val self = quotedArg("self")
           val arg1 = quotedArg(1)
           s"""$self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)+$arg1) }"""
         }

       infix ("-=") (DenseMatrix(T) :: MUnit, TArith(T), effect = write(0)) implements composite {
           val self = quotedArg("self")
           val arg1 = quotedArg(1)
           s"""$self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)-densematrix_raw_apply($arg1,i)) }"""
         }
       infix ("-=") (T :: MUnit, TArith(T), effect = write(0)) implements composite {
           val self = quotedArg("self")
           val arg1 = quotedArg(1)
           s"""$self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)-$arg1) }"""
         }

       infix ("*=") (DenseMatrix(T) :: MUnit, TArith(T), effect = write(0)) implements composite {
           val self = quotedArg("self")
           val arg1 = quotedArg(1)
           s"""$self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)*densematrix_raw_apply($arg1,i)) }"""
         }
       infix ("*=") (T :: MUnit, TArith(T), effect = write(0)) implements composite {
           val self = quotedArg("self")
           val arg1 = quotedArg(1)
           s"""$self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)*$arg1) }"""
         }

       infix ("/=") (DenseMatrix(T) :: MUnit, TArith(T), effect = write(0)) implements composite {
           val self = quotedArg("self")
           val arg1 = quotedArg(1)
           s"""$self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)/densematrix_raw_apply($arg1,i)) }"""
         }
       infix ("/=") (T :: MUnit, TArith(T), effect = write(0)) implements composite {
           val self = quotedArg("self")
           val arg1 = quotedArg(1)
           s"""$self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)/$arg1) }"""
         }


       /**
        * Ordering
        */

       infix ("sortRowsBy") ((DenseVectorView(T) ==> B) :: DenseMatrix(T), TOrdering(B), addTpePars = B) implements composite {
           val self = quotedArg("self")
           val arg1 = quotedArg(1)
           s"""val sortedIndicesRaw = farray_from_sarray(densevector_sortindex_helper(0, $self.numRows, densevector_raw_data($self.mapRowsToVector($arg1))))
val sortedIndices = IndexVector(densevector_fromarray(sortedIndicesRaw,true))
$self(sortedIndices)"""
         }

       infix ("sortColsBy") ((DenseVectorView(T) ==> B) :: DenseMatrix(T), TOrdering(B), addTpePars = B) implements composite {
           val self = quotedArg("self")
           val arg1 = quotedArg(1)
           s"""val sortedIndicesRaw = farray_from_sarray(densevector_sortindex_helper(0, $self.numCols, densevector_raw_data($self.mapColsToVector($arg1))))
val sortedIndices = IndexVector(densevector_fromarray(sortedIndicesRaw,true))
$self.getCols(sortedIndices)"""
         }

       infix (":>") (DenseMatrix(T) :: DenseMatrix(MBoolean), TOrdering(T)) implements zip((T,T,MBoolean), (0,1), {
         s"""(a,b) => a > b"""
       })
       infix (":<") (DenseMatrix(T) :: DenseMatrix(MBoolean), TOrdering(T)) implements zip((T,T,MBoolean), (0,1), {
  s"""(a,b) => a < b"""
})

       for (rhs <- List(DenseMatrix(T),DenseMatrixView(T))) {
         direct ("__equal") (rhs :: MBoolean) implements composite {
            val self = quotedArg("self")
            val arg1 = quotedArg(1)
            s"""if ($self.numRows != $arg1.numRows || $self.numCols != $arg1.numCols) false
  else {
    val c = sum($self.zip($arg1) { (a,b) => if (a == b) 0 else 1})
    c == 0
  }"""
          }
       }

       direct ("__equal") (SparseMatrix(T) :: MBoolean) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self == $arg1.toDense"""
}


       /**
        * Bulk
        */
       infix ("groupRowsBy") ((DenseVectorView(T) ==> K) :: MHashMap(K, DenseMatrix(T)), addTpePars = K) implements composite {
           val self = quotedArg("self")
           val arg1 = quotedArg(1)
           s"""val grps = densematrix_grouprowsby_helper($self.rowIndices, $self, $arg1, (row: Rep[DenseVectorView[T]]) => row)
val vals = fhashmap_values(grps)
val submats = vals.map(buf => (0::array_buffer_length(buf), *) { i => array_buffer_apply(buf,i) })
fhashmap_from_arrays(fhashmap_keys(grps), submats)"""
         }

       infix ("groupColsBy") ((DenseVectorView(T) ==> K) :: MHashMap(K, DenseMatrix(T)), addTpePars = K) implements composite {
           val self = quotedArg("self")
           val arg1 = quotedArg(1)
           s"""val grps = densematrix_groupcolsby_helper($self.colIndices, $self, $arg1, (col: Rep[DenseVectorView[T]]) => col)
val vals = fhashmap_values(grps)
val submats = vals.map(buf => (*, 0::array_buffer_length(buf)) { j => array_buffer_apply(buf,j) })
fhashmap_from_arrays(fhashmap_keys(grps), submats)"""
         }

      /**
       * Required for parallel collection
       */
      direct ("densematrix_raw_apply") (MInt :: T) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_apply(densematrix_raw_data($self), $arg1)"""
      }
      direct ("densematrix_raw_update") ((MInt,T) :: MUnit, effect = write(0)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  val arg2 = quotedArg(2)
  s"""array_update(densematrix_raw_data($self), $arg1, $arg2)"""
}

      parallelize as ParallelCollection(T, lookupOp("densematrix_dc_alloc"), lookupOp("size"), lookupOp("densematrix_raw_apply"), lookupOp("densematrix_raw_update"))
    }

    // Bulk of matrix operations is imported
    addMatrixCommonOps(DenseMatrix,T)

    // label lets you give a precise name to a particular variant of an overloaded op (typically so that you can refer to it in external code)
    // we use the following labels to optionally override the ops by calling BLAS
    label(lookupOverloaded("DenseMatrix","*",1), "densematrix_matmult")
    label(lookupOverloaded("DenseMatrix","*",2), "densematrix_sparse_matmult")
    label(lookupOverloaded("DenseMatrix","*",3), "densematrix_matvecmult")
    label(lookupOverloaded("DenseMatrix","*",4), "densematrix_sparse_matvecmult")

    // Add DenseMatrix to Arith
    val Arith = lookupGrp("Arith").asInstanceOf[Rep[DSLTypeClass]]
    val DenseMatrixArith = tpeClassInst("ArithDenseMatrix", T withBound TArith, Arith(DenseMatrix(T)))
    infix (DenseMatrixArith) ("zero", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements composite {
      val arg1 = quotedArg(0)
      s"""(unit(0)::$arg1.numRows,unit(0)::$arg1.numCols) { (i,j) => implicitly[Arith[T]].empty }"""
    }
    infix (DenseMatrixArith) ("empty", T withBound TArith, Nil :: DenseMatrix(T)) implements composite {
      s"""densematrix_fromarray[T](array_empty_imm[T](unit(0)),unit(0),unit(0))"""
    }
    infix (DenseMatrixArith) ("+", T withBound TArith, (DenseMatrix(T),DenseMatrix(T)) :: DenseMatrix(T)) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl($arg1,$arg2)"""
    }
    infix (DenseMatrixArith) ("-", T withBound TArith, (DenseMatrix(T),DenseMatrix(T)) :: DenseMatrix(T)) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub($arg1,$arg2)"""
    }
    infix (DenseMatrixArith) ("*", T withBound TArith, (DenseMatrix(T),DenseMatrix(T)) :: DenseMatrix(T)) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mulclnmul($arg1,$arg2)"""
    }
    infix (DenseMatrixArith) ("/", T withBound TArith, (DenseMatrix(T),DenseMatrix(T)) :: DenseMatrix(T)) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div($arg1,$arg2)"""
    }
    infix (DenseMatrixArith) ("abs", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements composite {
      val arg1 = quotedArg(0)
      s"""densematrix_abs($arg1)"""
    }
    infix (DenseMatrixArith) ("exp", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements composite {
      val arg1 = quotedArg(0)
      s"""densematrix_exp($arg1)"""
    }
    infix (DenseMatrixArith) ("log", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements composite {
  val arg1 = quotedArg(0)
  s"""densematrix_log($arg1)"""
}

    importDenseMatrixPrimitiveOps()
  }

  /**
   * Special cases for DenseMatrix primitive arithmetic. This is annoying, so let's hide it at the bottom.
   */
  def importDenseMatrixPrimitiveOps() {
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseVector = lookupTpe("DenseVector")

    // the conversions here will be costly unless things fuse. alternatively, we could convert element by element.

    infix (DenseMatrix) ("+", Nil, (MInt,DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_pl[Int]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (MInt,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_pl[Float]($arg1,$arg2.toFloat)"""
    }
    infix (DenseMatrix) ("+", Nil, (MInt,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_pl[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("+", Nil, (MFloat,DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_pl[Float]($arg1.toFloat,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (MFloat,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_pl[Float]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (MFloat,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_pl[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("+", Nil, (MDouble,DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_pl[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (MDouble,DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_pl[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (MDouble,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_pl[Double]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),MInt) :: DenseMatrix(MInt)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Int]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),MFloat) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Float]($arg1.toFloat,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),MDouble) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),MInt) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Float]($arg1,$arg2.toFloat)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),MFloat) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Float]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),MDouble) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),MInt) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),MFloat) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),MDouble) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Double]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Int]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Float]($arg1.toFloat,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Float]($arg1,$arg2.toFloat)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Float]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_pl[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""densematrix_pl[Double]($arg1,$arg2)"""
}

    infix (DenseMatrix) ("-", Nil, (MInt,DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_map[Int,Int]($arg1, e => forge_int_minus($arg2,e))"""
    }
    infix (DenseMatrix) ("-", Nil, (MInt,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_map[Float,Float]($arg1, e => forge_float_minus($arg2.toFloat,e))"""
    }
    infix (DenseMatrix) ("-", Nil, (MInt,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_map[Double,Double]($arg1, e => forge_double_minus($arg2.toDouble,e))"""
    }
    infix (DenseMatrix) ("-", Nil, (MFloat,DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_map[Int,Float]($arg1, e => forge_float_minus($arg2,e.toFloat))"""
    }
    infix (DenseMatrix) ("-", Nil, (MFloat,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_map[Float,Float]($arg1, e => forge_float_minus($arg2,e))"""
    }
    infix (DenseMatrix) ("-", Nil, (MFloat,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_map[Double,Double]($arg1, e => forge_double_minus($arg2.toDouble,e))"""
    }
    infix (DenseMatrix) ("-", Nil, (MDouble,DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_map[Int,Double]($arg1, e => forge_double_minus($arg2,e.toDouble))"""
    }
    infix (DenseMatrix) ("-", Nil, (MDouble,DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_map[Float,Double]($arg1, e => forge_double_minus($arg2,e.toDouble))"""
    }
    infix (DenseMatrix) ("-", Nil, (MDouble,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_map[Double,Double]($arg1, e => forge_double_minus($arg2,e))"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),MInt) :: DenseMatrix(MInt)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Int]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),MFloat) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Float]($arg1.toFloat,$arg2)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),MDouble) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),MInt) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Float]($arg1,$arg2.toFloat)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),MFloat) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Float]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),MDouble) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),MInt) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),MFloat) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),MDouble) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Double]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Int]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Float]($arg1.toFloat,$arg2)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Float]($arg1,$arg2.toFloat)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Float]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_sub[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""densematrix_sub[Double]($arg1,$arg2)"""
}

    infix (DenseMatrix) ("unary_-", Nil, (DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect {
      val arg1 = quotedArg(0)
      s"""densematrix_mul[Int]($arg1,unit(-1))"""
    }
    infix (DenseMatrix) ("unary_-", Nil, (DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      s"""densematrix_mul[Float]($arg1,unit(-1f))"""
    }
    infix (DenseMatrix) ("unary_-", Nil, (DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      s"""densematrix_mul[Double]($arg1,unit(-1.0))"""
    }
    infix (DenseMatrix) ("*", Nil, (MInt,DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_mul[Int]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (MInt,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_mul[Float]($arg1,$arg2.toFloat)"""
    }
    infix (DenseMatrix) ("*", Nil, (MInt,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_mul[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("*", Nil, (MFloat,DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_mul[Float]($arg1.toFloat,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (MFloat,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_mul[Float]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (MFloat,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_mul[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("*", Nil, (MDouble,DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_mul[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (MDouble,DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_mul[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (MDouble,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(1)
      val arg2 = quotedArg(0)
      s"""densematrix_mul[Double]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),MInt) :: DenseMatrix(MInt)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mul[Int]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),MFloat) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mul[Float]($arg1.toFloat,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),MDouble) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mul[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),MInt) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mul[Float]($arg1,$arg2.toFloat)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),MFloat) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mul[Float]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),MDouble) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mul[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),MInt) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mul[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),MFloat) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mul[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),MDouble) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mul[Double]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matmult[Int]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matmult[Float]($arg1.toFloat,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matmult[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matmult[Float]($arg1,$arg2.toFloat)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matmult[Float]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matmult[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matmult[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matmult[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matmult[Double]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matvecmult[Int]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matvecmult[Float]($arg1.toFloat,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matvecmult[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matvecmult[Float]($arg1,$arg2.toFloat)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matvecmult[Float]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matvecmult[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matvecmult[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matvecmult[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_matvecmult[Double]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mulclnmul[Int]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mulclnmul[Float]($arg1.toFloat,$arg2)"""
    }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mulclnmul[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mulclnmul[Float]($arg1,$arg2.toFloat)"""
    }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mulclnmul[Float]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mulclnmul[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mulclnmul[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_mulclnmul[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""densematrix_mulclnmul[Double]($arg1,$arg2)"""
}

    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),MInt) :: DenseMatrix(MInt)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Int]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),MFloat) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Float]($arg1.toFloat,$arg2)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),MDouble) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),MInt) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Float]($arg1,$arg2.toFloat)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),MFloat) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Float]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),MDouble) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),MInt) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),MFloat) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),MDouble) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Double]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Int]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Float]($arg1.toFloat,$arg2)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Float]($arg1,$arg2.toFloat)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Float]($arg1,$arg2)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Double]($arg1.toDouble,$arg2)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""densematrix_div[Double]($arg1,$arg2.toDouble)"""
    }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect {
    val arg1 = quotedArg(0)
    val arg2 = quotedArg(1)
    s"""densematrix_div[Double]($arg1,$arg2)"""
  }
  }
}
