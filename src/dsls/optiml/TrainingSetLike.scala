package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * Defines the TrainingSetLike type class for DenseTrainingSet and SparseTrainingSet.
 * This type class enables interfaces to consume a generic TrainingSet with minimal functionality.
 */
trait TrainingSetLikeOps {
  this: OptiMLDSL =>

  object TTrainingSetLike extends TypeClassSignature {
    def name = "TrainingSetLike"
    def prefix = "_ts"
    def wrapper = Some("tstype")
  }

  def importTrainingSetLikeOps() {
    val D = tpePar("D")
    val L = tpePar("L")
    val TS = hkTpePar("TS", (D,L))

    val DenseTrainingSet = lookupTpe("DenseTrainingSet")
    val SparseTrainingSet = lookupTpe("SparseTrainingSet")
    val DenseVector = lookupTpe("DenseVector")
    val SparseVector = lookupTpe("SparseVector")
    val IndexVector = lookupTpe("IndexVector")

    val TrainingSetLike = tpeClass("TrainingSetLike", TTrainingSetLike, (D,L,TS))

    // TrainingSetLike type class interface
    infix (TrainingSetLike) ("labels", (D,L,TS), TS(D,L) :: DenseVector(L))
    infix (TrainingSetLike) ("numSamples", (D,L,TS), TS(D,L) :: MInt)
    infix (TrainingSetLike) ("numFeatures", (D,L,TS), TS(D,L) :: MInt)
    infix (TrainingSetLike) ("getRows", (D,L,TS), (TS(D,L),IndexVector) :: TS(D,L))
    infix (TrainingSetLike) ("getCols", (D,L,TS), (TS(D,L),IndexVector) :: TS(D,L))

    // These are used to update parameters, which we assume are dense. (This may not be a good assumption, in particular with L1 regularization)
    // If we want to store sparse parameters, we will need to actually handle the sparse case explicitly in the Classifier implementation itself,
    // rather than using TrainingSetLike to be generic.
    infix (TrainingSetLike) ("dot", (D,L,TS), (TS(D,L), MInt, DenseVector(D)) :: D, TArith(D))
    infix (TrainingSetLike) ("times", (D,L,TS), (TS(D,L), MInt, DenseVector(D)) :: DenseVector(D), TArith(D))
    infix (TrainingSetLike) ("timesScalar", (D,L,TS), (TS(D,L), MInt, D) :: DenseVector(D), TArith(D))

    val DenseTrainingSetTrainingSetLike = tpeClassInst("TrainingSetLikeDenseTrainingSet", (D, L), TTrainingSetLike(D,L,DenseTrainingSet(D,L)))
    infix (DenseTrainingSetTrainingSetLike) ("labels", (D,L), DenseTrainingSet(D,L) :: DenseVector(L)) implements composite {
      val arg1 = quotedArg(0)
      s"""$arg1.labels"""
    }
    infix (DenseTrainingSetTrainingSetLike) ("numSamples", (D,L), DenseTrainingSet(D,L) :: MInt) implements composite {
      val arg1 = quotedArg(0)
      s"""$arg1.numSamples"""
    }
    infix (DenseTrainingSetTrainingSetLike) ("numFeatures", (D,L), DenseTrainingSet(D,L) :: MInt) implements composite {
      val arg1 = quotedArg(0)
      s"""$arg1.numFeatures"""
    }
    infix (DenseTrainingSetTrainingSetLike) ("getRows", (D,L), (DenseTrainingSet(D,L), IndexVector) :: DenseTrainingSet(D,L)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""DenseTrainingSet($arg1.data.apply($arg2), $arg1.labels.apply($arg2))"""
      }
    infix (DenseTrainingSetTrainingSetLike) ("getCols", (D,L), (DenseTrainingSet(D,L), IndexVector) :: DenseTrainingSet(D,L)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""DenseTrainingSet($arg1.data.getCols($arg2), $arg1.labels)"""
      }
    infix (DenseTrainingSetTrainingSetLike) ("dot", (D,L), (DenseTrainingSet(D,L), MInt, DenseVector(D)) :: D, TArith(D)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        val arg3 = quotedArg(2)
        s"""$arg1($arg2) *:* $arg3"""
      }
    infix (DenseTrainingSetTrainingSetLike) ("times", (D,L), (DenseTrainingSet(D,L), MInt, DenseVector(D)) :: DenseVector(D), TArith(D)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        val arg3 = quotedArg(2)
        s"""$arg1($arg2) * $arg3"""
      }
    infix (DenseTrainingSetTrainingSetLike) ("timesScalar", (D,L), (DenseTrainingSet(D,L), MInt, D) :: DenseVector(D), TArith(D)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        val arg3 = quotedArg(2)
        s"""$arg1($arg2) * $arg3"""
      }

    val SparseTrainingSetTrainingSetLike = tpeClassInst("TrainingSetLikeSparseTrainingSet", (D, L), TTrainingSetLike(D,L,SparseTrainingSet(D,L)))
    infix (SparseTrainingSetTrainingSetLike) ("labels", (D,L), SparseTrainingSet(D,L) :: DenseVector(L)) implements composite {
      val arg1 = quotedArg(0)
      s"""$arg1.labels"""
    }
    infix (SparseTrainingSetTrainingSetLike) ("numSamples", (D,L), SparseTrainingSet(D,L) :: MInt) implements composite {
      val arg1 = quotedArg(0)
      s"""$arg1.numSamples"""
    }
    infix (SparseTrainingSetTrainingSetLike) ("numFeatures", (D,L), SparseTrainingSet(D,L) :: MInt) implements composite {
      val arg1 = quotedArg(0)
      s"""$arg1.numFeatures"""
    }
    infix (SparseTrainingSetTrainingSetLike) ("getRows", (D,L), (SparseTrainingSet(D,L), IndexVector) :: SparseTrainingSet(D,L)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""SparseTrainingSet($arg1.data.apply($arg2), $arg1.labels.apply($arg2))"""
      }
    infix (SparseTrainingSetTrainingSetLike) ("getCols", (D,L), (SparseTrainingSet(D,L), IndexVector) :: SparseTrainingSet(D,L)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""SparseTrainingSet($arg1.data.getCols($arg2), $arg1.labels)"""
      }
    infix (SparseTrainingSetTrainingSetLike) ("dot", (D,L), (SparseTrainingSet(D,L), MInt, DenseVector(D)) :: D, TArith(D)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        val arg3 = quotedArg(2)
        s"""$arg1($arg2) *:* $arg3"""
      }
    infix (SparseTrainingSetTrainingSetLike) ("times", (D,L), (SparseTrainingSet(D,L), MInt, DenseVector(D)) :: DenseVector(D), TArith(D)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        val arg3 = quotedArg(2)
        s"""($arg1($arg2)*$arg3).toDense"""
      }
    infix (SparseTrainingSetTrainingSetLike) ("timesScalar", (D,L), (SparseTrainingSet(D,L), MInt, D) :: DenseVector(D), TArith(D)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        val arg3 = quotedArg(2)
        s"""($arg1($arg2)*$arg3).toDense"""
      }
  }
}
