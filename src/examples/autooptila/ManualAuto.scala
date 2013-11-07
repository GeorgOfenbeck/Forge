package ppl.dsl.forge
package examples
package autooptila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait ManualOps {
  this: AutoOptiLADSL with ppl.dsl.forge.core.ForgeOpsExp =>

  def importManualOps() {
    importIndexVectorManual()
    importDenseVectorViewManual()
    importDenseVectorManual()
    importDenseMatrixManual()
  }

  def importIndexVectorManual() {
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")

    val IndexVectorOps = withTpe (IndexVector)
    IndexVectorOps {
      // parallel, so the conversion can fuse with the consumer
      // is this fast and robust enough to capture parallel operators over index vectors?
      fimplicit ("indexToDense") (Nil :: DenseVector(MInt)) implements composite ${
        Console.println("(performance warning): automatic conversion from IndexVector to DenseVector")
        $self.toDense
      }

      // naming is by convention here, a little brittle. would it be better to put this in extern?
      val grpName = if (Config.fastCompile) "$Flat" else "DenseVector"
      fimplicit ("chainIndexToDenseOps") (Nil :: ephemeralTpe(grpName+"DenseVectorOpsCls[Int]", stage = now)) implements composite ${
        repTo\${grpName}DenseVectorOpsCls(indexToDense($self))
      }

      // IndexVectors can't be mapped over, but they can be zipped with or reduced
      parallelize as ParallelCollection(MInt, lookupOp("indexvector_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",4), lookupOp("indexvector_illegalupdate"))
    }
  }

  def importDenseVectorViewManual() {
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorViewOps = withTpe (DenseVectorView)
    val T = tpePar("T")
    DenseVectorViewOps {
      fimplicit ("viewToDense") (Nil :: DenseVector(T)) implements composite ${
        Console.println("(performance warning): automatic conversion from DenseVectorView to DenseVector")
        // Console.println("  at " + quotePos(fresh[Nothing].withPos(List(implicitly[SourceContext]))))
        $self.toDense
      }
      val grpName = if (Config.fastCompile) "$Flat" else "DenseVector"
      fimplicit ("chainViewToDenseOps") (Nil :: ephemeralTpe(grpName+"DenseVectorOpsCls[T]", stage = now)) implements composite ${
        repTo\${grpName}DenseVectorOpsCls(viewToDense($self))
      }

      parallelize as ParallelCollection(T, lookupOp("densevectorview_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("densevectorview_illegalupdate"))
    }
  }

  def importDenseMatrixManual() {
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseVector = lookupTpe("DenseVector")
    val T = tpePar("T")

    static (DenseMatrix) ("apply", T, varArgs(DenseVector(T)) :: DenseMatrix(T)) implements single ${
      val out = DenseMatrix[T](0, 0)
      // don't lift the range over the current stage Seq[DenseVector[T]]
      for (i: Int <- scala.collection.immutable.Range(0,__arg0.length)) {
        out <<= $0(i)
      }
      out.unsafeImmutable
    }

    // TODO: generalize this (and the static diag above) to kth diagonal
    // direct (DenseMatrix) ("diag", T, MethodSignature(List(("x",DenseMatrix(T)),("k",MInt,"0")), DenseVector(T)) implements composite ${
    direct (DenseMatrix) ("diag", T, DenseMatrix(T) :: DenseVector(T)) implements composite ${
      val indices = (0::$0.numRows) { i => i + i*$0.numCols }
      indices.t map { i => densematrix_raw_apply($0,i) }
    }

    val DenseMatrixOps = withTpe (DenseMatrix)

    DenseMatrixOps {
      direct ("densematrix_raw_apply") (MInt :: T) implements composite ${ array_apply(densematrix_raw_data($self), $1) }
      direct ("densematrix_raw_update") ((MInt,T) :: MUnit, effect = write(0)) implements composite ${ array_update(densematrix_raw_data($self), $1, $2) }

      parallelize as ParallelCollection(T, lookupOp("densematrix_raw_alloc"), lookupOp("size"), lookupOp("densematrix_raw_apply"), lookupOp("densematrix_raw_update"))
    }

    // label lets you give a precise name to a particular variant of an overloaded op (typically so that you can refer to it in external code)
    // we use the following labels to optionally override the ops by calling BLAS
    label(lookupOverloaded("DenseMatrix","*",1), "densematrix_matmult")
    label(lookupOverloaded("DenseMatrix","*",2), "densematrix_matvecmult")

    // Add DenseMatrix to Arith
    val Arith = lookupGrp("Arith").asInstanceOf[Rep[DSLTypeClass]]
    val DenseMatrixArith = tpeClassInst("ArithDenseMatrix", T withBound TArith, Arith(DenseMatrix(T)))
    infix (DenseMatrixArith) ("zero", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements composite ${ DenseMatrix[T]($0.numRows,$0.numCols).unsafeImmutable }
    infix (DenseMatrixArith) ("empty", T withBound TArith, Nil :: DenseMatrix(T)) implements composite ${ DenseMatrix[T](unit(0),unit(0)).unsafeImmutable }
    infix (DenseMatrixArith) ("+", T withBound TArith, (DenseMatrix(T),DenseMatrix(T)) :: DenseMatrix(T)) implements composite ${ densematrix_pl($0,$1) }
    infix (DenseMatrixArith) ("-", T withBound TArith, (DenseMatrix(T),DenseMatrix(T)) :: DenseMatrix(T)) implements composite ${ densematrix_sub($0,$1) }
    infix (DenseMatrixArith) ("*", T withBound TArith, (DenseMatrix(T),DenseMatrix(T)) :: DenseMatrix(T)) implements composite ${ densematrix_mulclnmul($0,$1) }
    infix (DenseMatrixArith) ("/", T withBound TArith, (DenseMatrix(T),DenseMatrix(T)) :: DenseMatrix(T)) implements composite ${ densematrix_div($0,$1) }
    infix (DenseMatrixArith) ("abs", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements composite ${ densematrix_abs($0) }
    infix (DenseMatrixArith) ("exp", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements composite ${ densematrix_exp($0) }
    infix (DenseMatrixArith) ("log", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements composite ${ densematrix_log($0) }

    importDenseMatrixPrimitiveOps()
  }

  def importDenseVectorManual() {
    val DenseVector = lookupTpe("DenseVector")
    val IndexVector = lookupTpe("IndexVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")

    // operations on literal sequences are made available via tuple conversions to DenseVector
    // non-literal sequences don't work and require explicit conversion to DenseVector first, due to the implicit ambiguity problem below
    for (arity <- (2 until 23)) {
      val pars = (1 to arity).map(i => T).toList
      val elems = (1 to arity).map(i => "unit(t._"+i+")").mkString(",")
      val TT = tpe("Tuple" + arity, pars, stage = compile)
      fimplicit (DenseVector) ("tupleToDense" + arity, T, (("t",TT) :: DenseVector(T))) implements redirect ${ DenseVector[T](\$elems) }

      // problem: makes implicit conversions to TupNs ambiguous due to type erasure (Rep[_])
      // val elems2 = (0 until arity).map(i => "t._" + (i+1)).mkString(",")
      // val TT2 = tpeInst(tpe("Tuple" + arity, pars, stage = compile), pars)
      // fimplicit (DenseVector) ("repTupleToDense" + arity, T, (("t",TT2) :: DenseVector(T))) implements redirect ${ DenseVector[T](\$elems2) }
    }


    static (DenseVector) ("flatten", T, ("pieces",DenseVector(DenseVector(T))) :: DenseVector(T)) implements single ${
      if ($pieces.length == 0){
        DenseVector[T](0, $pieces.isRow).unsafeImmutable
      }
      else {
        val sizes = $pieces map { e => e.length }
        val (total,begins) = t2(densevector_precumulate[Int](sizes, 0, (_: Rep[Int]) + (_: Rep[Int])))
        val result = DenseVector[T](total, $pieces.isRow)
        for (i <- 0 until $pieces.length) {
          result.copyFrom(begins(i), $pieces(i))
        }
        result.unsafeImmutable
      }
    }

    // a non-type-safe way of passing the metadata required to allocate a DenseVector in a parallel op
    // ideally we would encode this is as a type class, but it's not clear we would get an instance of this type class in dc_alloc
    val CR = tpePar("CR")
    
    compiler (DenseVector) ("densevector_raw_alloc", (R,CR), (CR,MInt) :: DenseVector(R)) implements composite ${
      val simpleName = manifest[CR].erasure.getSimpleName
      val isRow = simpleName match {
        case s if s.startsWith("IndexVector") => indexvector_isrow($0.asInstanceOf[Rep[IndexVector]])
        case s if s.startsWith("DenseVectorView") => densevectorview_isrow($0.asInstanceOf[Rep[DenseVectorView[Any]]])
        case s if s.startsWith("DenseVector") => densevector_isrow($0.asInstanceOf[Rep[DenseVector[Any]]])
      }
      DenseVector[R]($1, isRow)
    }
    
    compiler (DenseVector) ("densevector_sortindex_helper", T, (MInt,MInt,MArray(T)) :: MArray(MInt), TOrdering(T)) implements codegen($cala, ${
      ($0 until $1: scala.Range).toArray.sortWith((a,b) => $2(a) < $2(b))
    })

    compiler (DenseVector) ("densevector_groupby_helper", (T,R), (MArray(T), (T ==> R)) :: MArray(MArray(T))) implements codegen($cala, ${
      $0.groupBy(e => $b[1](e)).values.toArray
    })

    val DenseVectorOps = withTpe (DenseVector)

    DenseVectorOps {
      parallelize as ParallelCollectionBuffer(T, lookupOp("densevector_raw_alloc"), lookupOp("length"), lookupOverloaded("apply",2), lookupOp("update"), lookupOp("densevector_set_length"), lookupOp("densevector_appendable"), lookupOp("densevector_append"), lookupOp("densevector_copy"))
    }


    // Add DenseVector to Arith
    val Arith = lookupGrp("Arith").asInstanceOf[Rep[DSLTypeClass]]
    val DenseVectorArith = tpeClassInst("ArithDenseVector", T withBound TArith, Arith(DenseVector(T)))
    infix (DenseVectorArith) ("zero", T withBound TArith, DenseVector(T) :: DenseVector(T)) implements composite ${ DenseVector[T]($0.length,$0.isRow).unsafeImmutable }
    infix (DenseVectorArith) ("empty", T withBound TArith, Nil :: DenseVector(T)) implements composite ${ DenseVector[T](unit(0),unit(true)).unsafeImmutable }
    infix (DenseVectorArith) ("+", T withBound TArith, (DenseVector(T),DenseVector(T)) :: DenseVector(T)) implements composite ${ densevector_pl($0,$1) }
    infix (DenseVectorArith) ("-", T withBound TArith, (DenseVector(T),DenseVector(T)) :: DenseVector(T)) implements composite ${ densevector_sub($0,$1) }
    infix (DenseVectorArith) ("*", T withBound TArith, (DenseVector(T),DenseVector(T)) :: DenseVector(T)) implements composite ${ densevector_mul($0,$1) }
    infix (DenseVectorArith) ("/", T withBound TArith, (DenseVector(T),DenseVector(T)) :: DenseVector(T)) implements composite ${ densevector_div($0,$1) }
    infix (DenseVectorArith) ("abs", T withBound TArith, DenseVector(T) :: DenseVector(T)) implements composite ${ densevector_abs($0) }
    infix (DenseVectorArith) ("exp", T withBound TArith, DenseVector(T) :: DenseVector(T)) implements composite ${ densevector_exp($0) }
    infix (DenseVectorArith) ("log", T withBound TArith, DenseVector(T) :: DenseVector(T)) implements composite ${ densevector_log($0) }

    importDenseVectorPrimitiveOps()


  }

  /**
   * Special cases for DenseMatrix primitive arithmetic. This is annoying, so let's hide it at the bottom.
   */
  def importDenseMatrixPrimitiveOps() {
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseVector = lookupTpe("DenseVector")

    // the conversions here will be costly unless things fuse. alternatively, we could convert element by element.

    infix (DenseMatrix) ("+", Nil, (MInt,DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_pl[Int]($1,$0) }
    infix (DenseMatrix) ("+", Nil, (MInt,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($1,$0.toFloat) }
    infix (DenseMatrix) ("+", Nil, (MInt,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($1,$0.toDouble) }
    infix (DenseMatrix) ("+", Nil, (MFloat,DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($1.toFloat,$0) }
    infix (DenseMatrix) ("+", Nil, (MFloat,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($1,$0) }
    infix (DenseMatrix) ("+", Nil, (MFloat,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($1,$0.toDouble) }
    infix (DenseMatrix) ("+", Nil, (MDouble,DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($1.toDouble,$0) }
    infix (DenseMatrix) ("+", Nil, (MDouble,DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($1.toDouble,$0) }
    infix (DenseMatrix) ("+", Nil, (MDouble,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($1,$0) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),MInt) :: DenseMatrix(MInt)) implements redirect ${ densematrix_pl[Int]($0,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),MInt) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($0,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),MInt) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),MFloat) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_pl[Int]($0,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($0,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0,$1) }

    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),MInt) :: DenseMatrix(MInt)) implements redirect ${ densematrix_sub[Int]($0,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_sub[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),MInt) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_sub[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_sub[Float]($0,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),MInt) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),MFloat) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_sub[Int]($0,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_sub[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_sub[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_sub[Float]($0,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0,$1) }

    infix (DenseMatrix) ("unary_-", Nil, (DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_mul[Int]($0,unit(-1)) }
    infix (DenseMatrix) ("unary_-", Nil, (DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($0,unit(-1f)) }
    infix (DenseMatrix) ("unary_-", Nil, (DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($0,unit(-1.0)) }
    infix (DenseMatrix) ("*", Nil, (MInt,DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_mul[Int]($1,$0) }
    infix (DenseMatrix) ("*", Nil, (MInt,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($1,$0.toFloat) }
    infix (DenseMatrix) ("*", Nil, (MInt,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($1,$0.toDouble) }
    infix (DenseMatrix) ("*", Nil, (MFloat,DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($1.toFloat,$0) }
    infix (DenseMatrix) ("*", Nil, (MFloat,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($1,$0) }
    infix (DenseMatrix) ("*", Nil, (MFloat,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($1,$0.toDouble) }
    infix (DenseMatrix) ("*", Nil, (MDouble,DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($1.toDouble,$0) }
    infix (DenseMatrix) ("*", Nil, (MDouble,DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($1.toDouble,$0) }
    infix (DenseMatrix) ("*", Nil, (MDouble,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($1,$0) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),MInt) :: DenseMatrix(MInt)) implements redirect ${ densematrix_mul[Int]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),MInt) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),MInt) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),MFloat) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_matmult[Int]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_matmult[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_matmult[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_matmult[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_matmult[Float]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_matmult[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_matmult[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_matmult[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_matmult[Double]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densematrix_matvecmult[Int]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densematrix_matvecmult[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densematrix_matvecmult[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densematrix_matvecmult[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densematrix_matvecmult[Float]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densematrix_matvecmult[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densematrix_matvecmult[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densematrix_matvecmult[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densematrix_matvecmult[Double]($0,$1) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_mulclnmul[Int]($0,$1) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mulclnmul[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mulclnmul[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mulclnmul[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mulclnmul[Float]($0,$1) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mulclnmul[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mulclnmul[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mulclnmul[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mulclnmul[Double]($0,$1) }

    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),MInt) :: DenseMatrix(MInt)) implements redirect ${ densematrix_div[Int]($0,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_div[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),MInt) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_div[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_div[Float]($0,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),MInt) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),MFloat) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_div[Int]($0,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_div[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_div[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_div[Float]($0,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0,$1) }
  }

  /**
   * Special cases for DenseVector primitive arithmetic. This is annoying, so let's hide it at the bottom.
   */
  def importDenseVectorPrimitiveOps() {
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")

    // the conversions here will be costly unless things fuse. alternatively, we could convert element by element.
    // TODO: unfortunately, these have priority over operators defined in VectorCommonOps, so they can sometimes force conversions.
    infix (DenseVector) ("+", Nil, (MInt,DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_pl[Int]($1,$0) }
    infix (DenseVector) ("+", Nil, (MInt,DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($1,$0.toFloat) }
    infix (DenseVector) ("+", Nil, (MInt,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($1,$0.toDouble) }
    infix (DenseVector) ("+", Nil, (MFloat,DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($1.toFloat,$0) }
    infix (DenseVector) ("+", Nil, (MFloat,DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($1,$0) }
    infix (DenseVector) ("+", Nil, (MFloat,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($1,$0.toDouble) }
    infix (DenseVector) ("+", Nil, (MDouble,DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($1.toDouble,$0) }
    infix (DenseVector) ("+", Nil, (MDouble,DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($1.toDouble,$0) }
    infix (DenseVector) ("+", Nil, (MDouble,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($1,$0) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),MInt) :: DenseVector(MInt)) implements redirect ${ densevector_pl[Int]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($0.toFloat,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),MInt) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($0,$1.toFloat) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),MInt) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0,$1.toDouble) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),MFloat) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_pl[Int]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($0.toFloat,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($0,$1.toFloat) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0,$1.toDouble) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0,$1.toDouble) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0,$1) }

    infix (DenseVector) ("-", Nil, (DenseVector(MInt),MInt) :: DenseVector(MInt)) implements redirect ${ densevector_sub[Int]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_sub[Float]($0.toFloat,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),MInt) :: DenseVector(MFloat)) implements redirect ${ densevector_sub[Float]($0,$1.toFloat) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_sub[Float]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),MInt) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0,$1.toDouble) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),MFloat) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_sub[Int]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_sub[Float]($0.toFloat,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_sub[Float]($0,$1.toFloat) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_sub[Float]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0,$1.toDouble) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0,$1.toDouble) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0,$1) }

    infix (DenseVector) ("unary_-", Nil, (DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_mul[Int]($0,unit(-1)) }
    infix (DenseVector) ("unary_-", Nil, (DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,unit(-1f)) }
    infix (DenseVector) ("unary_-", Nil, (DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,unit(-1.0)) }
    infix (DenseVector) ("*", Nil, (MInt,DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_mul[Int]($1,$0) }
    infix (DenseVector) ("*", Nil, (MInt,DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($1,$0.toFloat) }
    infix (DenseVector) ("*", Nil, (MInt,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($1,$0.toDouble) }
    infix (DenseVector) ("*", Nil, (MFloat,DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($1.toFloat,$0) }
    infix (DenseVector) ("*", Nil, (MFloat,DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($1,$0) }
    infix (DenseVector) ("*", Nil, (MFloat,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($1,$0.toDouble) }
    infix (DenseVector) ("*", Nil, (MDouble,DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($1.toDouble,$0) }
    infix (DenseVector) ("*", Nil, (MDouble,DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($1.toDouble,$0) }
    infix (DenseVector) ("*", Nil, (MDouble,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($1,$0) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),MInt) :: DenseVector(MInt)) implements redirect ${ densevector_mul[Int]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0.toFloat,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),MInt) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,$1.toFloat) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),MInt) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1.toDouble) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),MFloat) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_mul[Int]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0.toFloat,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,$1.toFloat) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1.toDouble) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1.toDouble) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseMatrix(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_mul[Int]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseMatrix(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0.toFloat,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseMatrix(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseMatrix(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,$1.toFloat) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseMatrix(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseMatrix(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseMatrix(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1.toDouble) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseMatrix(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1.toDouble) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseMatrix(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1) }

    infix (DenseVector) ("/", Nil, (DenseVector(MInt),MInt) :: DenseVector(MInt)) implements redirect ${ densevector_div[Int]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_div[Float]($0.toFloat,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),MInt) :: DenseVector(MFloat)) implements redirect ${ densevector_div[Float]($0,$1.toFloat) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_div[Float]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),MInt) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0,$1.toDouble) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),MFloat) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_div[Int]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_div[Float]($0.toFloat,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_div[Float]($0,$1.toFloat) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_div[Float]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0,$1.toDouble) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0,$1.toDouble) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0,$1) }
  }
}