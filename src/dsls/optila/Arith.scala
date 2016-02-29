package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * Defines the Arith type class for scalars, vectors, and matrices
 */
trait ArithOps {
  this: OptiLADSL =>

  object TArith extends TypeClassSignature {
    def name = "Arith"
    def prefix = "_a"
    def wrapper = Some("atype")
  }

  def importArithOps() {
    val T = tpePar("T")
    val A = tpePar("A")
    val B = tpePar("B")

    val Arith = tpeClass("Arith", TArith, T)

    // Arith type class interface
    infix (Arith) ("zero", T, T :: T)
    infix (Arith) ("empty", T, Nil :: T)
    infix (Arith) ("+", T, (T,T) :: T)
    infix (Arith) ("-", T, (T,T) :: T)
    infix (Arith) ("*", T, (T,T) :: T)
    infix (Arith) ("/", T, (T,T) :: T)
    infix (Arith) ("abs", T, T :: T)
    infix (Arith) ("exp", T, T :: T)
    infix (Arith) ("log", T, T :: T)

    // primitive implementations
    val DoubleArith = tpeClassInst("ArithDouble", Nil, Arith(MDouble))
    infix (DoubleArith) ("zero", Nil, MDouble :: MDouble) implements composite {
      s"""unit(0.0)"""
    }
    infix (DoubleArith) ("empty", Nil, Nil :: MDouble) implements composite {
      s"""unit(0.0)"""
    }
    infix (DoubleArith) ("+", Nil, (MDouble,MDouble) :: MDouble) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_double_plus($arg1,$arg2)"""
    }
    infix (DoubleArith) ("-", Nil, (MDouble,MDouble) :: MDouble) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_double_minus($arg1,$arg2)"""
    }
    infix (DoubleArith) ("*", Nil, (MDouble,MDouble) :: MDouble) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_double_times($arg1,$arg2)"""
    }
    infix (DoubleArith) ("/", Nil, (MDouble,MDouble) :: MDouble) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_double_divide($arg1,$arg2)"""
    }
    infix (DoubleArith) ("abs", Nil, MDouble :: MDouble) implements composite {
      val arg1 = quotedArg(0)
      s"""math_object_abs($arg1)"""
    }
    infix (DoubleArith) ("exp", Nil, MDouble :: MDouble) implements composite {
      val arg1 = quotedArg(0)
      s"""math_object_exp($arg1)"""
    }
    infix (DoubleArith) ("log", Nil, MDouble :: MDouble) implements composite {
  val arg1 = quotedArg(0)
  s"""math_object_log($arg1)"""
}

    val FloatArith = tpeClassInst("ArithFloat", Nil, Arith(MFloat))
    infix (FloatArith) ("zero", Nil, MFloat :: MFloat) implements composite {
      s"""unit(0f)"""
    }
    infix (FloatArith) ("empty", Nil, Nil :: MFloat) implements composite {
      s"""unit(0f)"""
    }
    infix (FloatArith) ("+", Nil, (MFloat,MFloat) :: MFloat) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_float_plus($arg1,$arg2)"""
    }
    infix (FloatArith) ("-", Nil, (MFloat,MFloat) :: MFloat) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_float_minus($arg1,$arg2)"""
    }
    infix (FloatArith) ("*", Nil, (MFloat,MFloat) :: MFloat) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_float_times($arg1,$arg2)"""
    }
    infix (FloatArith) ("/", Nil, (MFloat,MFloat) :: MFloat) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_float_divide($arg1,$arg2)"""
    }
    infix (FloatArith) ("abs", Nil, MFloat :: MFloat) implements composite {
      val arg1 = quotedArg(0)
      s"""math_object_abs($arg1).toFloat"""
    }
    infix (FloatArith) ("exp", Nil, MFloat :: MFloat) implements composite {
      val arg1 = quotedArg(0)
      s"""math_object_exp($arg1).toFloat"""
    }
    infix (FloatArith) ("log", Nil, MFloat :: MFloat) implements composite {
  val arg1 = quotedArg(0)
  s"""math_object_log($arg1).toFloat"""
}

    val IntArith = tpeClassInst("ArithInt", Nil, Arith(MInt))
    infix (IntArith) ("zero", Nil, MInt :: MInt) implements composite {
      s"""unit(0)"""
    }
    infix (IntArith) ("empty", Nil, Nil :: MInt) implements composite {
      s"""unit(0)"""
    }
    infix (IntArith) ("+", Nil, (MInt,MInt) :: MInt) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_int_plus($arg1,$arg2)"""
    }
    infix (IntArith) ("-", Nil, (MInt,MInt) :: MInt) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_int_minus($arg1,$arg2)"""
    }
    infix (IntArith) ("*", Nil, (MInt,MInt) :: MInt) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_int_times($arg1,$arg2)"""
    }
    infix (IntArith) ("/", Nil, (MInt,MInt) :: MInt) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_int_divide($arg1,$arg2)"""
    }
    infix (IntArith) ("abs", Nil, MInt :: MInt) implements composite {
      val arg1 = quotedArg(0)
      s"""math_object_abs($arg1).toInt"""
    }
    infix (IntArith) ("exp", Nil, MInt :: MInt) implements composite {
      val arg1 = quotedArg(0)
      s"""math_object_exp($arg1).toInt"""
    }
    infix (IntArith) ("log", Nil, MInt :: MInt) implements composite {
  val arg1 = quotedArg(0)
  s"""math_object_log($arg1).toInt"""
}

    val LongArith = tpeClassInst("ArithLong", Nil, Arith(MLong))
    infix (LongArith) ("zero", Nil, MLong :: MLong) implements composite {
      s"""unit(0L)"""
    }
    infix (LongArith) ("empty", Nil, Nil :: MLong) implements composite {
      s"""unit(0L)"""
    }
    infix (LongArith) ("+", Nil, (MLong,MLong) :: MLong) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_long_plus($arg1,$arg2)"""
    }
    infix (LongArith) ("-", Nil, (MLong,MLong) :: MLong) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_long_minus($arg1,$arg2)"""
    }
    infix (LongArith) ("*", Nil, (MLong,MLong) :: MLong) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_long_times($arg1,$arg2)"""
    }
    infix (LongArith) ("/", Nil, (MLong,MLong) :: MLong) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_long_divide($arg1,$arg2)"""
    }
    infix (LongArith) ("abs", Nil, MLong :: MLong) implements composite {
      val arg1 = quotedArg(0)
      s"""math_object_abs($arg1.toDouble).toLong"""
    }
    infix (LongArith) ("exp", Nil, MLong :: MLong) implements composite {
      val arg1 = quotedArg(0)
      s"""math_object_exp($arg1.toDouble).toLong"""
    }
    infix (LongArith) ("log", Nil, MLong :: MLong) implements composite {
  val arg1 = quotedArg(0)
  s"""math_object_log($arg1.toDouble).toLong"""
}

    // tuples of ariths
    for (arity <- 2 until maxTuples) {
      val Tup = lookupTpe("Tup"+arity)
      val pars = (0 until arity).map(i => tpePar(('A'.toInt+i).toChar.toString) withBound TArith).toList
      val TupArith = tpeClassInst("ArithTup"+arity, pars, Arith(Tup))

      def tupArithStr(op: String) = "pack((" + pars.map(p => "implicitly[Arith["+p.name+"]]."+op).mkString(",") + "))"
      def tupArithFromSrcStr(op: String) = "pack((" + (1 to arity).map(i => "t._"+i+"."+op).mkString(",") + "))"
      def tupArithBinStr(op: String) = "pack((" + (1 to arity).map(i => "t1._"+i+op+"t2._"+i).mkString(",") + "))"

      infix (TupArith) ("zero", pars, ("t",Tup) :: Tup) implements composite { tupArithFromSrcStr("zero") }
      infix (TupArith) ("empty", pars, Nil :: Tup) implements composite { tupArithStr("empty") }
      infix (TupArith) ("+", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite { tupArithBinStr("+") }
      infix (TupArith) ("-", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite { tupArithBinStr("-") }
      infix (TupArith) ("*", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite { tupArithBinStr("*") }
      infix (TupArith) ("/", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite { tupArithBinStr("/") }
      infix (TupArith) ("abs", pars, ("t",Tup) :: Tup) implements composite { tupArithFromSrcStr("abs") }
      infix (TupArith) ("exp", pars, ("t",Tup) :: Tup) implements composite { tupArithFromSrcStr("exp") }
      infix (TupArith) ("log", pars, ("t",Tup) :: Tup) implements composite { tupArithFromSrcStr("log") }
    }
  }
}
