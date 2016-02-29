package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait DenseVectorViewOps {
  this: OptiLADSL =>

  def importDenseVectorViewOps() {
    val T = tpePar("T")
    val DenseVectorView = lookupTpe("DenseVectorView") // tpe("DenseVectorView", T)
    val DenseVector = lookupTpe("DenseVector")
    val IndexVector = lookupTpe("IndexVector")

    // data fields
    data(DenseVectorView, ("_data", MArray(T)), ("_start", MInt), ("_stride", MInt), ("_length", MInt), ("_isRow", MBoolean))

    // static methods
    static (DenseVectorView) ("apply", T, ((MArray(T), MInt ,MInt, MInt, MBoolean) :: DenseVectorView)) implements allocates(DenseVectorView, {
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

    val DenseVectorViewOps = withTpe(DenseVectorView)
    DenseVectorViewOps {
      compiler ("densevectorview_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("densevectorview_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("densevectorview_stride") (Nil :: MInt) implements getter(0, "_stride")

      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("apply") (MInt :: T) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_apply(densevectorview_data($self), densevectorview_start($self) + $arg1*densevectorview_stride($self))"""
      }
      infix ("apply") (IndexVector :: DenseVector(T)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.toDense.apply($arg1)"""
}

      // label DenseVector *:* DenseVectorView so that we can rewrite it in RewriteOpsExp
      label(lookupOverloaded("DenseVectorView","apply",1), "densevectorview_apply_int")

      infix ("slice") ((("start",MInt),("end",MInt)) :: DenseVectorView(T)) implements composite {
          val self = quotedArg("self")
          val start = quotedArg("start")
          val end = quotedArg("end")
          s"""DenseVectorView(densevectorview_data($self), densevectorview_start($self)+$start*densevectorview_stride($self), densevectorview_stride($self), $end-$start, $self.isRow)"""
        }

      // clones return a DenseVector, so that we do not retain the same underlying pointer
      infix ("Clone") (Nil :: DenseVector(T)) implements redirect {
  val self = quotedArg("self")
  s"""$self.toDense"""
}

      infix ("toDense") (Nil :: DenseVector(T)) implements composite {
  val self = quotedArg("self")
  s"""$self.map(e => e)"""
}

      direct ("__equal") (DenseVector(T) :: MBoolean) implements composite {
  val arg1 = quotedArg(1)
  val self = quotedArg("self")
  s"""$arg1 == $self"""
}

      infix ("filter") ((T ==> MBoolean) :: DenseVector(T)) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""$self.toDense.filter($arg1)"""
}

      fimplicit ("viewToDense") (Nil :: DenseVector(T)) implements composite {
          val self = quotedArg("self")
          s"""if (Settings.verbose > 0) println("(performance warning): automatic conversion from DenseVectorView to DenseVector")

$self.toDense"""
        }
      val grpName = if (Config.fastCompile) "$Flat" else "DenseVector"
      fimplicit ("chainViewToDenseOps") (Nil :: ephemeralTpe(grpName+"DenseVectorOpsCls[T]", stage = now)) implements composite {
          val self = quotedArg("self")
          s"""repTo${grpName}DenseVectorOpsCls(viewToDense($self))"""
        }

      compiler ("densevectorview_illegalalloc") (MInt :: MNothing, effect = simple) implements composite {
        s"""fatal("DenseVectorViews cannot be allocated from a parallel op")"""
      }
      compiler ("densevectorview_illegalupdate") ((MInt, T) :: MNothing, effect = simple) implements composite {
  s"""fatal("DenseVectorViews cannot be updated")"""
}

      parallelize as ParallelCollection(T, lookupOp("densevectorview_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("densevectorview_illegalupdate"))
    }

    // allows us to perform operations without converting to a DenseVector first
    addVectorCommonOps(DenseVectorView,T)
  }
}
