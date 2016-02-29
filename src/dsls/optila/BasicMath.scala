package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait BasicMathOps {
  this: OptiLADSL =>

  def importBasicMathOps() {
    val Math = grp("BasicMath")
    val Prim = lookupGrp("Primitive")

	  val T = tpePar("T")

    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseMatrixView = lookupTpe("DenseMatrixView")
    val SparseVector = lookupTpe("SparseVector")
    val SparseMatrix = lookupTpe("SparseMatrix")

    // -- aliases for Math._
    direct (Math) ("abs", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.abs($arg1)"""
    }
    direct (Math) ("exp", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.exp($arg1)"""
    }
    direct (Math) ("log", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.log($arg1)"""
    }
    direct (Math) ("log10", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.log10($arg1)"""
    }
    direct (Math) ("square", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""$arg1*$arg1"""
    }
    direct (Math) ("sqrt", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.sqrt($arg1)"""
    }
    direct (Math) ("ceil", Nil, MDouble :: MInt) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.ceil($arg1).toInt"""
    }
    direct (Math) ("floor", Nil, MDouble :: MInt) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.floor($arg1).toInt"""
    }
    direct (Math) ("round", Nil, MDouble :: MInt) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.round($arg1).toInt"""
    }
    direct (Math) ("sin", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.sin($arg1)"""
    }
    direct (Math) ("sinh", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.sinh($arg1)"""
    }
    direct (Math) ("asin", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.asin($arg1)"""
    }
    direct (Math) ("cos", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.cos($arg1)"""
    }
    direct (Math) ("cosh", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.cosh($arg1)"""
    }
    direct (Math) ("acos", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.acos($arg1)"""
    }
    direct (Math) ("tan", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.tan($arg1)"""
    }
    direct (Math) ("tanh", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.tanh($arg1)"""
    }
    direct (Math) ("atan", Nil, MDouble :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      s"""Math.atan($arg1)"""
    }
    direct (Math) ("atan2", Nil, (MDouble,MDouble) :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""Math.atan2($arg1,$arg2)"""
    }
    direct (Math) ("pow", Nil, (MDouble,MDouble) :: MDouble) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""Math.pow($arg1,$arg2)"""
    }
    infix (Math)  ("~^", Nil, (MDouble,MInt) :: MDouble) implements redirect {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""Math.pow($arg1,$arg2)"""
} // wrong precedence without ~

    val max = direct (Math) ("max", T withBound TNumeric, (T,T) :: T) implements codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      val tpeT = quotedTpe("T", lookupOp(Math,"max"))
      s"""implicitly[Numeric[$tpeT]].max($arg1,$arg2)"""
    })
    val min = direct (Math) ("min", T withBound TNumeric, (T,T) :: T) implements codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      val tpeT = quotedTpe("T", lookupOp(Math,"min"))
      s"""implicitly[Numeric[$tpeT]].min($arg1,$arg2)"""
    })
    for (g <- List(cuda, cpp)) {
    	impl (max) (codegen(g, "(" + quotedArg(0) + ">" + quotedArg(1) + ")?" + quotedArg(0) + ":" + quotedArg(1)))
    	impl (min) (codegen(g, "(" + quotedArg(0) + "<" + quotedArg(1) + ")?" + quotedArg(0) + ":" + quotedArg(1)))
  	}


    // -- distance

    // don't kick in when polymorphic, for unknown reasons
    // fimplicit (DenseVector) ("dist", T, (DenseVector(T),DenseVector(T)) :: MDouble, ("conv", T ==> MDouble)) implements composite ${ sum(abs($0.toDouble - $1.toDouble)) }
    // fimplicit (DenseMatrix) ("dist", T, (DenseMatrix(T),DenseMatrix(T)) :: MDouble, ("conv", T ==> MDouble)) implements composite ${ sum(abs($0.toDouble - $1.toDouble)) }
    fimplicit (Prim) ("dist", Nil, (MInt,MInt) :: MDouble) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""abs($arg1-$arg2)"""
    }
    fimplicit (Prim) ("dist", Nil, (MDouble,MDouble) :: MDouble) implements composite {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""abs($arg1-$arg2)"""
}

    // metrics: default is ABS
    val DMetric = tpe("DistanceMetric", stage = compile)
    identifier (DMetric) ("ABS")
    identifier (DMetric) ("SQUARE")
    identifier (DMetric) ("EUC")

    for (TP <- List(DenseVector,DenseVectorView,DenseMatrix,SparseVector)) {
      fimplicit (TP) ("dist", Nil, (TP(MDouble),TP(MDouble)) :: MDouble) implements redirect {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""dist($arg1,$arg2,ABS)"""
}

      direct (TP) ("dist", Nil, (TP(MDouble),TP(MDouble),DMetric) :: MDouble) implements composite {
          val arg1 = quotedArg(2)
          val arg2 = quotedArg(0)
          val arg3 = quotedArg(1)
          s"""$arg1 match {
  case ABS => sum(abs($arg2 - $arg3))
  case SQUARE => sum(square($arg2 - $arg3))
  case EUC => sqrt(sum(square($arg2 - $arg3)))
}"""
        }
    }


    // -- norms

    // norm ids: default is L2
    val NormId = tpe("NormId", stage = compile)
    identifier (NormId) ("L1")
    identifier (NormId) ("L2")
    identifier (NormId) ("FRO")

    for (TP <- List(DenseVector,DenseVectorView)) {
      direct (TP) ("norm", Nil, TP(MDouble) :: MDouble) implements redirect {
  val arg1 = quotedArg(0)
  s"""norm($arg1,L2)"""
}

      direct (TP) ("norm", Nil, (TP(MDouble),NormId) :: MDouble) implements composite {
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(0)
          s"""$arg1 match {
  case L1 => sum(abs($arg2))
  case L2 => sqrt(sum(square($arg2)))
  case FRO => norm($arg2, L2)
}"""
        }
    }

    for (TP <- List(DenseMatrix,DenseMatrixView)) {
      direct (TP) ("norm", Nil, TP(MDouble) :: MDouble) implements redirect {
  val arg1 = quotedArg(0)
  s"""norm($arg1,L2)"""
}

      direct (TP) ("norm", Nil, (TP(MDouble),NormId) :: MDouble) implements composite {
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(0)
          s"""$arg1 match {
  case L1 => max($arg2.mapColsToVector(c => norm(c, L1)))
  case L2 => fatal("not implemented")
  case FRO => sqrt(sum($arg2.mapColsToVector(c => sum(square(c)))))
}"""
        }
    }


    // -- normalization

    val NormalizeMethod = tpe("NormalizeMethod", stage = compile)
    identifier (NormalizeMethod) ("Std")
    identifier (NormalizeMethod) ("Unity")

    direct (Math) ("normalize", Nil, DenseVector(MDouble) :: DenseVector(MDouble)) implements redirect {
  val arg1 = quotedArg(0)
  s"""normalize($arg1, Std)"""
}

    direct (Math) ("normalize", Nil, (DenseVector(MDouble), NormalizeMethod) :: DenseVector(MDouble)) implements composite {
        val arg1 = quotedArg(1)
        val arg2 = quotedArg(0)
        s"""$arg1 match {
  case Std =>
    
    normalizeStdUsing($arg2, mean($arg2), stddev($arg2))

  case Unity =>
    
    normalizeUnityUsing($arg2, min($arg2), max($arg2))
}"""
      }

    direct (Math) ("normalize", Nil, DenseMatrix(MDouble) :: DenseMatrix(MDouble)) implements redirect {
  val arg1 = quotedArg(0)
  s"""normalize($arg1, Std)"""
}

    direct (Math) ("normalize", Nil, (DenseMatrix(MDouble), NormalizeMethod) :: DenseMatrix(MDouble)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 mapCols { c => normalize(c, $arg2) }"""
      }

    // -- These are factored out so that users can call them independently (i.e. with saved values)

    direct (Math) ("normalizeUnityScalarUsing", Nil, (("e",MDouble), ("minVal", MDouble), ("maxVal", MDouble)) :: MDouble) implements composite {
        s"""(((e - minVal) / (maxVal - minVal)) * 2.0) - 1.0"""
      }

    direct (Math) ("normalizeUnityUsing", Nil, (("v",DenseVector(MDouble)), ("minVal", MDouble), ("maxVal", MDouble)) :: DenseVector(MDouble)) implements composite {
        s"""v map { e => normalizeUnityScalarUsing(e, minVal, maxVal) }"""
      }

    direct (Math) ("normalizeStdScalarUsing", Nil, (("e",MDouble), ("avg", MDouble), ("stddev", MDouble)) :: MDouble) implements composite {
        s"""(e - avg) / stddev"""
      }

    direct (Math) ("normalizeStdUsing", Nil, (("v",DenseVector(MDouble)), ("avg", MDouble), ("stddev", MDouble)) :: DenseVector(MDouble)) implements composite {
        s"""v map { e => normalizeStdScalarUsing(e, avg, stddev) }"""
      }

    // -- other math ops
    direct (Math) ("sigmoid", Nil, MDouble :: MDouble) implements composite {
  val arg1 = quotedArg(0)
  s"""1.0 / (1.0 + exp(-$arg1))"""
}


    // -- pdfs
    direct (Math) ("normpdf", Nil, (("x",MDouble),("mu",MDouble),("sigma",MDouble)) :: MDouble) implements composite {
        s"""(1.0 / (sigma * sqrt(2.0*Pi))) * exp(-((x-mu)*(x-mu)) / (2.0*sigma*sigma))"""
      }

    direct (Math) ("normpdf", Nil, (("x",DenseVector(MDouble)),("mu",DenseVector(MDouble)),("sigma",DenseVector(MDouble))) :: DenseVector(MDouble)) implements composite {
        s"""(0::x.length) { i => normpdf(x(i), mu(i), sigma(i)) }"""
      }

    direct (Math) ("normpdf", Nil, (("x",DenseMatrix(MDouble)),("mu",DenseMatrix(MDouble)),("sigma",DenseMatrix(MDouble))) :: DenseMatrix(MDouble)) implements composite {
        s"""(0::x.numRows, 0::x.numCols) { (i,j) => normpdf(x(i,j), mu(i,j), sigma(i,j)) }"""
      }

    for (Col <- List(DenseVector(MDouble),DenseMatrix(MDouble))) {
      direct (Math) ("normpdf", Nil, (("x",Col),("mu",MDouble),("sigma",Col)) :: Col) implements redirect {
          s"""x.zip(sigma) { (a,b) => normpdf(a, mu, b) }"""
        }
      direct (Math) ("normpdf", Nil, (("x",Col),("mu",Col),("sigma",MDouble)) :: Col) implements redirect {
          s"""x.zip(mu) { (a,b) => normpdf(a, b, sigma) }"""
        }
      direct (Math) ("normpdf", Nil, (("x",Col),("mu",MDouble),("sigma",MDouble)) :: Col) implements redirect {
          s"""x.map { e => normpdf(e, mu, sigma) }"""
        }
    }


    // -- aliases for instance math ops

    for (V <- List(DenseVector(T),DenseVectorView(T),IndexVector,DenseMatrix(T))) {
      val A = if (V != IndexVector) List(TArith(T)) else Nil
      val O = if (V != IndexVector) List(TOrdering(T)) else Nil
      val H = if (V != IndexVector) List(THasMinMax(T)) else Nil
      val C = if (V != IndexVector) List(arg("conv",T ==> MDouble)) else Nil
      val P = if (V != IndexVector) List(T) else Nil
      val R1 = if (V == DenseMatrix(T)) DenseMatrix(T) else if (V == IndexVector) DenseVector(MInt) else DenseVector(T)
      val R2 = if (V != IndexVector) T else MInt
      val size = if (V == DenseMatrix(T)) "size" else "length"

      direct (Math) ("abs", P, V :: R1, A) implements redirect {
        val arg1 = quotedArg(0)
        s"""$arg1.abs"""
      }
      direct (Math) ("exp", P, V :: R1, A) implements redirect {
        val arg1 = quotedArg(0)
        s"""$arg1.exp"""
      }
      direct (Math) ("log", P, V :: R1, A) implements redirect {
        val arg1 = quotedArg(0)
        s"""$arg1.log"""
      }
      direct (Math) ("square", P, V :: R1, A) implements composite {
        val arg1 = quotedArg(0)
        s"""$arg1.map(e => e*e)"""
      }
      direct (Math) ("sum", P, V :: R2, A) implements redirect {
        val arg1 = quotedArg(0)
        s"""$arg1.sum"""
      }
      direct (Math) ("prod", P, V :: R2, A) implements redirect {
        val arg1 = quotedArg(0)
        s"""$arg1.prod"""
      }
      direct (Math) ("mean", P, V :: MDouble, C) implements redirect {
        val arg1 = quotedArg(0)
        s"""$arg1.mean"""
      }
      direct (Math) ("variance", P, V :: MDouble, C) implements composite {
          val arg1 = quotedArg(0)
          s"""fassert($arg1.$size > 0, "variance: input argument must have > 0 elements")
val dbls = $arg1.toDouble
val avg = mean(dbls)
val diffs = dbls map { e => square(e-avg) }


if ($arg1.$size == 1) {
  sum(diffs)
}
else {
  sum(diffs) / (diffs.$size-1.0)
}"""
        }
      direct (Math) ("stddev", P, V :: MDouble, C) implements composite {
        val arg1 = quotedArg(0)
        s"""sqrt(variance($arg1))"""
      }
      direct (Math) ("min", P, V :: R2, O ::: H) implements redirect {
        val arg1 = quotedArg(0)
        s"""$arg1.min"""
      }
      direct (Math) ("max", P, V :: R2, O ::: H) implements redirect {
  val arg1 = quotedArg(0)
  s"""$arg1.max"""
}

      // only DenseVector has sort, and therefore median, defined right now
      if (V == DenseVector(T)) {
     	  direct (Math) ("median", T, V :: T, (TNumeric(T),TOrdering(T))) implements redirect {
        val arg1 = quotedArg(0)
        s"""$arg1.median"""
      }
      }
    }

    for (V <- List(SparseVector(T), SparseMatrix(T))) {
      val R = if (V == SparseMatrix(T)) SparseMatrix(T) else SparseVector(T)
      direct (Math) ("abs", T, V :: R, TArith(T)) implements redirect {
        val arg1 = quotedArg(0)
        s"""$arg1.abs"""
      }
      direct (Math) ("square", T, V :: R, TArith(T)) implements composite {
        val arg1 = quotedArg(0)
        s"""$arg1.mapnz(e => e*e)"""
      }
      direct (Math) ("sum", T, V :: T, TArith(T)) implements redirect {
        val arg1 = quotedArg(0)
        s"""$arg1.sum"""
      }
      direct (Math) ("mean", T, V :: MDouble, ("conv",T ==> MDouble)) implements redirect {
        val arg1 = quotedArg(0)
        s"""$arg1.mean"""
      }
      direct (Math) ("min", T, V :: T, (TOrdering(T), THasMinMax(T))) implements redirect {
        val arg1 = quotedArg(0)
        s"""$arg1.min"""
      }
      direct (Math) ("max", T, V :: T, (TOrdering(T), THasMinMax(T))) implements redirect {
      val arg1 = quotedArg(0)
      s"""$arg1.max"""
    }
    }


    // -- math on sequences
    // can't have a sequence-based sum, because it makes sum(0,n) { i => ... } ambiguous for int arguments
    // direct (Math) ("sum", T, varArgs(T) :: T, TArith(T)) implements composite ${ densevector_fromarray(array_fromseq($0),true).sum }
    direct (Math) ("mean", T, varArgs(T) :: MDouble, ("conv",T ==> MDouble)) implements composite {
      val arg1 = quotedArg(0)
      s"""densevector_fromarray(array_fromseq($arg1),true).mean"""
    }
    direct (Math) ("min", T, varArgs(T) :: T, (TOrdering(T), THasMinMax(T))) implements composite {
      val arg1 = quotedArg(0)
      s"""densevector_fromarray(array_fromseq($arg1),true).min"""
    }
    direct (Math) ("max", T, varArgs(T) :: T, (TOrdering(T), THasMinMax(T))) implements composite {
      val arg1 = quotedArg(0)
      s"""densevector_fromarray(array_fromseq($arg1),true).max"""
    }
    direct (Math) ("median", T, varArgs(T) :: T, (TNumeric(T),TOrdering(T))) implements composite {
    val arg1 = quotedArg(0)
    s"""densevector_fromarray(array_fromseq($arg1),true).median"""
  }
  }
}
