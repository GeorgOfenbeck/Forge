package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait RandomOps {
  this: OptiLADSL =>

  def importRandomOps() {
    val Rand = grp("Rand")
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val A = tpePar("A")

    direct (Rand) ("random", A, Nil :: A) implements composite {
        s"""val mA = manifest[A]
mA match {
  case Manifest.Double => optila_rand_double.AsInstanceOf[A]
  case Manifest.Float => optila_rand_float.AsInstanceOf[A]
  case Manifest.Int => optila_rand_int.AsInstanceOf[A]
  case Manifest.Boolean => optila_rand_boolean.AsInstanceOf[A]
  case _ => sys.error("no random implementation available for type " + mA.toString)
}"""
      }

    direct (Rand) ("randomElem", A, DenseVector(A) :: A, effect = simple) implements composite {
        val arg1 = quotedArg(0)
        s"""$arg1(randomInt($arg1.length))"""
      }

    val randomint = direct (Rand) ("randomInt", Nil, MInt :: MInt, effect = simple)
    impl (randomint) (codegen($cala, {
        val arg1 = quotedArg(0)
        s"""if (Global.useThreadLocalRandom) java.util.concurrent.ThreadLocalRandom.current().nextInt($arg1.toInt)
else Global.randRef.nextInt($arg1.toInt)"""
      }))
    impl (randomint) (codegen(cpp, {
  val arg1 = quotedArg(0)
  s"""resourceInfo->rand->nextInt($arg1)"""
}))

    val randomgaussian = direct (Rand) ("randomGaussian", Nil, Nil :: MDouble, effect = simple)
    impl (randomgaussian) (codegen($cala, {
        s"""if (Global.useThreadLocalRandom) java.util.concurrent.ThreadLocalRandom.current().nextGaussian()
else Global.randRef.nextGaussian()"""
      }))
    impl (randomgaussian) (codegen(cpp, {
  s"""resourceInfo->rand->nextGaussian()"""
}))

    val reseed = direct (Rand) ("reseed", Nil, Nil :: MUnit, effect = simple)
    impl (reseed) (codegen($cala, {
        s"""if (Global.useThreadLocalRandom) java.util.concurrent.ThreadLocalRandom.current().setSeed(Global.INITIAL_SEED)
else Global.randRef.setSeed(Global.INITIAL_SEED)"""
      }))
    impl (reseed) (codegen(cpp, {
  s"""fprintf(stderr, "WARNING: reseed is not currently implemented\\n")"""
}))

    val randdouble = compiler (Rand) ("optila_rand_double", Nil, Nil :: MDouble, effect = simple)
    impl (randdouble) (codegen($cala, {
        s"""if (Global.useThreadLocalRandom) java.util.concurrent.ThreadLocalRandom.current().nextDouble()
else Global.randRef.nextDouble()"""
      }))
    impl (randdouble) (codegen(cpp, {
  s"""resourceInfo->rand->nextDouble()"""
}))

    val randfloat = compiler (Rand) ("optila_rand_float", Nil, Nil :: MFloat, effect = simple)
    impl (randfloat) (codegen($cala, {
        s"""if (Global.useThreadLocalRandom) java.util.concurrent.ThreadLocalRandom.current().nextFloat()
else Global.randRef.nextFloat()"""
      }))
    impl (randfloat) (codegen(cpp, {
  s"""resourceInfo->rand->nextFloat()"""
}))

    val randint = compiler (Rand) ("optila_rand_int", Nil, Nil :: MInt, effect = simple)
    impl (randint) (codegen($cala, {
        s"""if (Global.useThreadLocalRandom) java.util.concurrent.ThreadLocalRandom.current().nextInt()
else Global.randRef.nextInt()"""
      }))
    impl (randint) (codegen(cpp, {
  s"""resourceInfo->rand->nextInt()"""
}))

    val randboolean = compiler (Rand) ("optila_rand_boolean", Nil, Nil :: MBoolean, effect = simple)
    impl (randboolean) (codegen($cala, {
        s"""if (Global.useThreadLocalRandom) java.util.concurrent.ThreadLocalRandom.current().nextBoolean()
else Global.randRef.nextBoolean()"""
      }))
    impl (randboolean) (codegen(cpp, {
  s"""resourceInfo->rand->nextBoolean()"""
}))

    direct (Rand) ("shuffle", Nil, IndexVector :: IndexVector, effect = simple) implements composite {
        val arg1 = quotedArg(0)
        s"""indexvector_fromarray(densevector_raw_data(shuffle($arg1.toDense)), $arg1.isRow)"""
      }

    direct (Rand) ("shuffle", A, DenseVector(A) :: DenseVector(A), effect = simple) implements composite {
        val arg1 = quotedArg(0)
        s"""val v2 = $arg1.mutable
v2.trim()
val a = optila_shuffle_array(densevector_raw_data(v2))
densevector_fromarray(a, $arg1.isRow)"""
      }

    direct (Rand) ("shuffle", A, DenseMatrix(A) :: DenseMatrix(A), effect = simple) implements composite {
        val arg1 = quotedArg(0)
        s"""val m2 = $arg1.mutable
m2.trim()
val a = optila_shuffle_array(densematrix_raw_data(m2))
densematrix_fromarray(a, $arg1.numRows, $arg1.numCols)"""
      }

    // any good parallel implementation?
    compiler (Rand) ("optila_shuffle_array", A, MArray(A) :: MArray(A), effect = simple) implements composite {
        val arg1 = quotedArg(0)
        s"""val len = array_length($arg1)
val out = array_empty[A](len)
array_copy($arg1, 0, out, 0, len)

var i = len-1
while (i > 1) {
  val swap = randomInt(i+1)
  val a = array_apply(out,i)
  array_update(out,i,array_apply(out,swap))
  array_update(out,swap,a)
  i -= 1
}

out.unsafeImmutable"""
      }

    direct (Rand) ("sample", Nil, (("v",IndexVector), ("pct", MDouble)) :: IndexVector, effect = simple) implements composite {
        val arg1 = quotedArg(0)
        s"""IndexVector(sample($arg1.toDense,pct), $arg1.isRow)"""
      }

    direct (Rand) ("sample", A, (("v",DenseVector(A)), ("pct", MDouble)) :: DenseVector(A), effect = simple) implements composite {
        s"""val candidates = (0::v.length).mutable

val sampled = DenseVector[A](0, v.isRow)
val numSamples = ceil(v.length * pct)
for (i <- 0 until numSamples){
  val r = i + randomInt(v.length-i)
  val idx = candidates(r)
  sampled <<= v(idx)

  
  val t = candidates(r)
  candidates(r) = candidates(i)
  candidates(i) = t
}

sampled.unsafeImmutable"""
      }

    direct (Rand) ("sample", A, MethodSignature(List(("m",DenseMatrix(A)), ("pct", MDouble), ("sampleRows", MBoolean, "unit(true)")), DenseMatrix(A)), effect = simple) implements composite {
        s"""val numSamples = if (sampleRows) ceil(m.numRows*pct) else ceil(m.numCols*pct)
val length = if (sampleRows) m.numRows else m.numCols
val newRows = if (sampleRows) numSamples else m.numRows
val newCols = if (sampleRows) m.numCols else numSamples

val sampled = if (sampleRows) DenseMatrix[A](0, newCols) else DenseMatrix[A](0, newRows) 

val candidates = (0::length).mutable


val mt = if (sampleRows) m else m.t

for (i <- 0 until numSamples){
  val r = i + randomInt(length-i)
  val idx = candidates(r)
  sampled <<= mt(idx).Clone

  
  val t = candidates(r)
  candidates(r) = candidates(i)
  candidates(i) = t
}

if (sampleRows) sampled else sampled.t"""
      }
  }
}
