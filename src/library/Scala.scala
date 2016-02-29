package ppl.dsl.forge
package library

import core.{ForgeApplication,ForgeApplicationRunner}
import templates.Utilities.nl

/**
 * This file re-implements LMS common ops in Forge.
 */
trait ScalaOps extends PrimitiveMathGen {
  this: ForgeApplication =>

  def importScalaOps() = {
    importProxies()
    importPrimitives()
    importMisc()
    importCasts()
    importNumerics()
    importOrdering()
    importStrings()
    importMath()
    importTuples()
    importHashMap()
    importByteBuffer()
  }

  /**
   * TODO: Use reflection to auto-generate the ops and code generators
   * Question: how do we handle effects?
   *   withWrites("update", "insert", ...)?
   */
  def importProxies() = {
    // proxy(scala.collection.immutable.Array)
  }

  def importPrimitives() = {
    val Prim = grp("Primitive")
    val T = tpePar("T")

    lift (Prim) (MBoolean)

    val not = infix (Prim) ("unary_!", Nil, MBoolean :: MBoolean)
    val or = infix (Prim) ("||", Nil, (MBoolean, MBoolean) :: MBoolean)
    val and = infix (Prim) ("&&", Nil, (MBoolean, MBoolean) :: MBoolean)

    for (g <- List($cala, cuda, cpp)) {
      impl (not) (codegen(g, "!" + quotedArg(0)))
      impl (or) (codegen(g, quotedArg(0) + " || " + quotedArg(1)))
      impl (and) (codegen(g, quotedArg(0) + " && " + quotedArg(1)))
    }

    infix (Prim) ("unary_-", Nil, MInt :: MInt) implements redirect {
      val arg1 = quotedArg(0)
      s"""unit(-1)*$arg1"""
    }
    infix (Prim) ("unary_-", Nil, MLong :: MLong) implements redirect {
      val arg1 = quotedArg(0)
      s"""unit(-1L)*$arg1"""
    }
    infix (Prim) ("unary_-", Nil, MFloat :: MFloat) implements redirect {
      val arg1 = quotedArg(0)
      s"""unit(-1f)*$arg1"""
    }
    infix (Prim) ("unary_-", Nil, MDouble :: MDouble) implements redirect {
  val arg1 = quotedArg(0)
  s"""unit(-1)*$arg1"""
}

    lift (Prim) (MShort)
    lift (Prim) (MInt)
    lift (Prim) (MLong)
    lift (Prim) (MFloat)
    lift (Prim) (MDouble)

    val toInt = infix (Prim) ("toInt", T withBound TNumeric, T :: MInt)
    val toFloat = infix (Prim) ("toFloat", T withBound TNumeric, T :: MFloat)
    val toDouble = infix (Prim) ("toDouble", T withBound TNumeric, T :: MDouble)
    val toLong = infix (Prim) ("toLong", T withBound TNumeric, T :: MLong)

    impl (toInt) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.toInt"""
    }))
    impl (toFloat) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.toFloat"""
    }))
    impl (toDouble) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.toDouble"""
    }))
    impl (toLong) (codegen($cala, {
  val arg1 = quotedArg(0)
  s"""$arg1.toLong"""
}))

    for (g <- List(cuda, cpp)) {
      impl (toInt) (codegen(g, {
        val arg1 = quotedArg(0)
        s"""(int32_t) $arg1"""
      }))
      impl (toFloat) (codegen(g, {
        val arg1 = quotedArg(0)
        s"""(float) $arg1"""
      }))
      impl (toDouble) (codegen(g, {
        val arg1 = quotedArg(0)
        s"""(double) $arg1"""
      }))
      impl (toLong) (codegen(g, {
      val arg1 = quotedArg(0)
      s"""(int64_t) $arg1"""
    }))
    }

    fimplicit (Prim) ("repInt2ToRepDouble", Nil, MInt :: MDouble) implements composite {
      val arg1 = quotedArg(0)
      s"""$arg1.toDouble"""
    }
    fimplicit (Prim) ("repInt2ToRepFloat", Nil, MInt :: MFloat) implements composite {
      val arg1 = quotedArg(0)
      s"""$arg1.toFloat"""
    }
    fimplicit (Prim) ("repInt2ToRepLong", Nil, MInt :: MLong) implements composite {
      val arg1 = quotedArg(0)
      s"""$arg1.toLong"""
    }
    fimplicit (Prim) ("repFloat2ToRepDouble", Nil, MFloat :: MDouble) implements composite {
  val arg1 = quotedArg(0)
  s"""$arg1.toDouble"""
}

    // specialized versions for primitives
    // the forge_ prefix is to avoid conflicting with LMS primitive ops
    val int_plus = direct (Prim) ("forge_int_plus", Nil, (MInt,MInt) :: MInt)
    val int_minus = direct (Prim) ("forge_int_minus", Nil, (MInt,MInt) :: MInt)
    val int_times = direct (Prim) ("forge_int_times", Nil, (MInt,MInt) :: MInt)
    val int_divide = direct (Prim) ("forge_int_divide", Nil, (MInt,MInt) :: MInt)
    val int_shift_left = direct (Prim) ("forge_int_shift_left", Nil, (MInt,MInt) :: MInt)
    val int_shift_right_unsigned = direct (Prim) ("forge_int_shift_right_unsigned", Nil, (MInt,MInt) :: MInt)
    impl (int_shift_right_unsigned) (codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1 >>> $arg2"""
    }))
    val int_binary_and = direct (Prim) ("forge_int_and", Nil, (MInt,MInt) :: MInt)
    val int_binary_or = direct (Prim) ("forge_int_or", Nil, (MInt,MInt) :: MInt)
    val int_shift_right = direct (Prim) ("forge_int_shift_right", Nil, (MInt,MInt) :: MInt)
    val int_mod = infix (Prim) ("%", Nil, (MInt,MInt) :: MInt)
    val int_bitwise_not = infix (Prim) ("unary_~", Nil, MInt :: MInt)

    val float_plus = direct (Prim) ("forge_float_plus", Nil, (MFloat,MFloat) :: MFloat)
    val float_minus = direct (Prim) ("forge_float_minus", Nil, (MFloat,MFloat) :: MFloat)
    val float_times = direct (Prim) ("forge_float_times", Nil, (MFloat,MFloat) :: MFloat)
    val float_divide = direct (Prim) ("forge_float_divide", Nil, (MFloat,MFloat) :: MFloat)

    val double_plus = direct (Prim) ("forge_double_plus", Nil, (MDouble,MDouble) :: MDouble)
    val double_minus = direct (Prim) ("forge_double_minus", Nil, (MDouble,MDouble) :: MDouble)
    val double_times = direct (Prim) ("forge_double_times", Nil, (MDouble,MDouble) :: MDouble)
    val double_divide = direct (Prim) ("forge_double_divide", Nil, (MDouble,MDouble) :: MDouble)

    val long_plus = direct (Prim) ("forge_long_plus", Nil, (MLong, MLong) :: MLong)
    val long_minus = direct (Prim) ("forge_long_minus", Nil, (MLong, MLong) :: MLong)
    val long_times = direct (Prim) ("forge_long_times", Nil, (MLong,MLong) :: MLong)
    val long_divide = direct (Prim) ("forge_long_divide", Nil, (MLong,MLong) :: MLong)
    val long_divide_double = direct (Prim) ("forge_long_divide_double", Nil, (MLong,MDouble) :: MDouble)
    val long_binary_and = direct (Prim) ("forge_long_and", Nil, (MLong,MLong) :: MLong)
    val long_binary_or = direct (Prim) ("forge_long_or", Nil, (MLong,MLong) :: MLong)
    val long_binary_xor = direct (Prim) ("forge_long_xor", Nil, (MLong,MLong) :: MLong)
    val long_shift_right_unsigned = direct (Prim) ("forge_long_shift_right_unsigned", Nil, (MLong,MInt) :: MLong)
    val long_shift_right = direct (Prim) ("forge_long_shift_right", Nil, (MLong,MInt) :: MLong)
    val long_shift_left = direct (Prim) ("forge_long_shift_left", Nil, (MLong,MInt) :: MLong)
    val long_mod = infix (Prim) ("%", Nil, (MLong,MLong) :: MLong)
    val long_bitwise_not = infix (Prim) ("unary_~", Nil, MLong :: MLong)
    impl (long_shift_right_unsigned) (codegen($cala, {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""$arg1 >>> $arg2"""
}))

    for (g <- List($cala, cuda, cpp)) {
      impl (int_plus) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 + $arg2"""
      }))
      impl (int_minus) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 - $arg2"""
      }))
      impl (int_times) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 * $arg2"""
      }))
      impl (int_divide) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 / $arg2"""
      }))
      impl (int_shift_left) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 << $arg2"""
      }))
      impl (int_shift_right) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 >> $arg2"""
      }))
      impl (int_mod) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 % $arg2"""
      }))
      impl (int_bitwise_not) (codegen(g, {
        val arg1 = quotedArg(0)
        s"""~$arg1"""
      }))
      impl (int_binary_and) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 & $arg2"""
      }))
      impl (int_binary_or) (codegen(g, {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""$arg1 | $arg2"""
}))

      impl (float_plus) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 + $arg2"""
      }))
      impl (float_minus) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 - $arg2"""
      }))
      impl (float_times) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 * $arg2"""
      }))
      impl (float_divide) (codegen(g, {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""$arg1 / $arg2"""
}))

      impl (double_plus) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 + $arg2"""
      }))
      impl (double_minus) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 - $arg2"""
      }))
      impl (double_times) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 * $arg2"""
      }))
      impl (double_divide) (codegen(g, {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""$arg1 / $arg2"""
}))

      impl (long_plus) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 + $arg2"""
      }))
      impl (long_minus) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 - $arg2"""
      }))
      impl (long_times) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 * $arg2"""
      }))
      impl (long_divide) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 / $arg2"""
      }))
      impl (long_divide_double) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 / $arg2"""
      }))
      impl (long_binary_and) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 & $arg2"""
      }))
      impl (long_binary_or) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 | $arg2"""
      }))
      impl (long_binary_xor) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 ^ $arg2"""
      }))
      impl (long_shift_right) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 >> $arg2"""
      }))
      impl (long_shift_left) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 << $arg2"""
      }))
      impl (long_mod) (codegen(g, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1 % $arg2"""
      }))
      impl (long_bitwise_not) (codegen(g, {
      val arg1 = quotedArg(0)
      s"""~$arg1"""
    }))
    }

    infix (Prim) ("<<",Nil, (MInt,MInt) :: MInt) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_int_shift_left($arg1,$arg2)"""
    }
    infix (Prim) (">>",Nil, (MInt,MInt) :: MInt) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_int_shift_right($arg1,$arg2)"""
    }
    infix (Prim) (">>>",Nil, (MInt,MInt) :: MInt) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_int_shift_right_unsigned($arg1,$arg2)"""
    }
    infix (Prim) ("&", Nil, (MInt,MInt) :: MInt) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_int_and($arg1,$arg2)"""
    }
    infix (Prim) ("|", Nil, (MInt,MInt) :: MInt) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_int_or($arg1,$arg2)"""
    }
    infix (Prim) ("&", Nil, (MLong,MLong) :: MLong) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_long_and($arg1,$arg2)"""
    }
    infix (Prim) ("|", Nil, (MLong,MLong) :: MLong) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_long_or($arg1,$arg2)"""
    }
    infix (Prim) ("^", Nil, (MLong,MLong) :: MLong) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_long_xor($arg1,$arg2)"""
    }
    infix (Prim) (">>>", Nil, (MLong,MInt) :: MLong) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_long_shift_right_unsigned($arg1,$arg2)"""
    }
    infix (Prim) ("<<", Nil, (MLong,MInt) :: MLong) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_long_shift_left($arg1,$arg2)"""
    }
    infix (Prim) (">>", Nil, (MLong,MInt) :: MLong) implements redirect {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""forge_long_shift_right($arg1,$arg2)"""
}

    // Uncomment this to generate type classes for primitive math combinations.
    // importPrimitiveMathTypeClasses()

    // Uncomment this to generate infix methods for primitive math combinations.
    importPrimitiveMathInfix(Prim)
  }

  def importMisc() = {
    val Misc = grp("Misc")

    val exit = direct (Misc) ("exit", Nil, MInt :: MNothing, effect = simple)
    impl (exit) (codegen($cala, {
  val arg1 = quotedArg(0)
  s"""sys.exit($arg1)"""
}))

    val print = direct (Misc) ("print", Nil, MAny :: MUnit, effect = simple)
    impl (print) (codegen($cala, {
  val arg1 = quotedArg(0)
  s"""print($arg1)"""
}))

    val fatal = direct (Misc) ("fatal", Nil, MString :: MNothing, effect = simple)
    impl (fatal) (codegen($cala, {
  val arg1 = quotedArg(0)
  s"""throw new Exception($arg1)"""
}))

    val println = direct (Misc) ("println", List(), List(MAny) :: MUnit, effect = simple)
    val println2 = direct (Misc) ("println", List(), List() :: MUnit, effect = simple)
    impl (println) (codegen($cala, "println(" + quotedArg(0) + ")"))
    impl (println2) (codegen($cala, "println()"))

    val whileDo = direct (Misc) ("__whileDo", List(), List(MThunk(MBoolean),MThunk(MUnit)) :: MUnit)

    // function (block) arguments should be referenced using $b[<arg name>]
    impl (whileDo) (codegen($cala, {
        val arg1 = quotedBlock(0, whileDo, List())
        val arg2 = quotedBlock(1, whileDo, List())
        s"""while ($arg1) {
  $arg2
}"""
      }))

    // TODO: something is broken with IfThenElse here; bound symbols (effects) are getting hoisted if the frequencies are not set to cold.
    val T = tpePar("T")
    val ifThenElse = direct (Misc) ("__ifThenElse", List(T), List(MThunk(MBoolean),MThunk(T,cold),MThunk(T,cold)) :: T)
    impl (ifThenElse) (codegen($cala, {
        val arg1 = quotedBlock(0, ifThenElse, List())
        val arg2 = quotedBlock(1, ifThenElse, List())
        val arg3 = quotedBlock(2, ifThenElse, List())
        s"""if ($arg1) {
  $arg2
}
else {
  $arg3
}"""
      }))

    val immutable = infix (Misc) ("unsafeImmutable", List(T), List(T) :: T, aliasHint = copies(0))
    impl (immutable) (codegen($cala, quotedArg(0) + " /* unsafe immutable */"))

    val mut = infix (Misc) ("unsafeMutable", List(T), List(T) :: T, effect = mutable, aliasHint = copies(0))
    impl (mut) (codegen($cala, quotedArg(0) + " /* unsafe mutable */"))

    for (g <- List(cuda, cpp)) {
      impl (exit) (codegen(g, {
        val arg1 = quotedArg(0)
        s"""exit($arg1)"""
      }))
      impl (print) (codegen(g, {
        val arg1 = quotedArg(0)
        s"""std::cout << $arg1"""
      }))
      impl (fatal) (codegen(g, {
        s"""assert(0)"""
      }))
      impl (println) (codegen(g, {
        val arg1 = quotedArg(0)
        s"""std::cout << $arg1 << std::endl"""
      }))
      impl (println2) (codegen(g, {
        s"""std::cout << std::endl"""
      }))
      impl (immutable) (codegen(g, {
      val arg1 = quotedArg(0)
      s"""$arg1"""
    }))
    }

    direct (Misc) ("getMaxHeapSize", Nil, Nil :: MLong) implements codegen($cala, {
        s"""Runtime.getRuntime.maxMemory()"""
      })
  }

  def importCasts() = {
    val Cast = grp("Cast")
    val A = tpePar("A")
    val B = tpePar("B")

    // these don't work as infix_ methods
    noInfixList :::= List("AsInstanceOf", "IsInstanceOf")

    val asinstance = infix (Cast) ("AsInstanceOf", (A,B), A :: B)
    impl (asinstance) (codegen($cala, {
      val arg1 = quotedArg(0)
      val tpeB = quotedTpe("B", asinstance)
      s"""$arg1.asInstanceOf[$tpeB]"""
    }))
    //impl (asinstance) (codegen(cuda, ${ ($t[B])$0 }))
    //impl (asinstance) (codegen(cpp, ${ ($t[B])$0 }))
    //TODO: what is the proper way to get the result type (sym.tp) to call with remapWithRef?
    impl (asinstance) (codegen(cuda, "(" + unquotes("remapWithRef(sym.tp)") + ")" + quotedArg(0)))
    impl (asinstance) (codegen(cpp, "(" + unquotes("remapWithRef(sym.tp)") + ")" + quotedArg(0)))

    val isinstance = infix (Cast) ("IsInstanceOf", (A,B), A :: MBoolean)
    impl (isinstance) (codegen($cala, {
      val arg1 = quotedArg(0)
      val tpeB = quotedTpe("B", isinstance)
      s"""$arg1.isInstanceOf[$tpeB]"""
    }))
    // todo: how to implement isinstance for clike targets?
  }

  def importNumerics() = {
    val Num = grp("Numeric")
    val T = tpePar("T")

    lift (Num) (T withBound TNumeric)

    val zero = infix (Num) ("zero", List(T withBound TNumeric), List() :: T)
    val plus = infix (Num) ("+", List(T withBound TNumeric), List(T,T) :: T)
    val minus = infix (Num) ("-", List(T withBound TNumeric), List(T,T) :: T)
    val times = infix (Num) ("*", List(T withBound TNumeric), List(T,T) :: T)
    impl (zero) (codegen($cala, "implicitly[Numeric["+quotedTpe(0,zero)+"]].zero"))
    impl (zero) (codegen(cuda, "0"))
    impl (zero) (codegen(cpp, "0"))
    for (g <- List($cala, cuda, cpp)) {
      impl (plus) (codegen(g, quotedArg(0) + " + " + quotedArg(1)))
      impl (minus) (codegen(g, quotedArg(0) + " - " + quotedArg(1)))
      impl (times) (codegen(g, quotedArg(0) + " * " + quotedArg(1)))
    }

    val Frac = grp("Fractional")
    val R = tpePar("R")
    val div = infix (Frac) ("/", List(T,R withBound TFractional), (T,R) :: R, T ==> R)
    impl (div) (codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      val tpeR = quotedTpe("R", div)
      s"""implicitly[Fractional[$tpeR]].div($arg1,$arg2)"""
    }))
    impl (div) (codegen(cuda, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1 / $arg2"""
    }))
    impl (div) (codegen(cpp, {
    val arg1 = quotedArg(0)
    val arg2 = quotedArg(1)
    s"""$arg1 / $arg2"""
  }))
  }

  def importOrdering() = {
    val Ord = grp("Ordering")
    val A = tpePar("A")
    val B = tpePar("B")
    val AC = tpePar("A", stage = now)
    val BC = tpePar("B", stage = now)

    val eq = direct (Ord) ("__equal", (A,B), (A,B) :: MBoolean)
    label (eq, "forge_equals")
    impl (eq) (codegen($cala, quotedArg(0) + " == " + quotedArg(1)))
    impl (eq) (codegen(cuda, quotedArg(0) + " == " + quotedArg(1)))
    impl (eq) (codegen(cpp, quotedArg(0) + " == " + quotedArg(1)))

    direct (Ord) ("__equal", (A,B), (MVar(A),B) :: MBoolean) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_equals(readVar($arg1), $arg2)"""
    }
    direct (Ord) ("__equal", (A,B), (A,MVar(B)) :: MBoolean) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_equals($arg1, readVar($arg2))"""
    }
    direct (Ord) ("__equal", (A,B), (MVar(A),MVar(B)) :: MBoolean) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_equals(readVar($arg1), readVar($arg2))"""
    }
    direct (Ord) ("__equal", (A,BC), (A,BC) :: MBoolean) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_equals($arg1, unit($arg2))"""
    }
    direct (Ord) ("__equal", (A,BC), (MVar(A),BC) :: MBoolean) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_equals(readVar($arg1), unit($arg2))"""
    }
    direct (Ord) ("__equal", (AC,B), (AC,B) :: MBoolean) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_equals(unit($arg1), $arg2)"""
    }
    direct (Ord) ("__equal", (AC,B), (AC,MVar(B)) :: MBoolean) implements redirect {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""forge_equals(unit($arg1), readVar($arg2))"""
}

    val neq = infix (Ord) ("!=", (A,B), (A,B) :: MBoolean)
    label (neq, "forge_notequals")
    impl (neq) (codegen($cala, quotedArg(0) + " != " + quotedArg(1)))
    impl (neq) (codegen(cuda, quotedArg(0) + " != " + quotedArg(1)))
    impl (neq) (codegen(cpp, quotedArg(0) + " != " + quotedArg(1)))

    infix (Ord) ("!=", (A,B), (MVar(A),B) :: MBoolean) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_notequals(readVar($arg1), $arg2)"""
    }
    infix (Ord) ("!=", (A,B), (A,MVar(B)) :: MBoolean) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_notequals($arg1, readVar($arg2))"""
    }
    infix (Ord) ("!=", (A,B), (MVar(A),MVar(B)) :: MBoolean) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_notequals(readVar($arg1), readVar($arg2))"""
    }
    infix (Ord) ("!=", (A,BC), (A,BC) :: MBoolean) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_notequals($arg1, unit($arg2))"""
    }
    infix (Ord) ("!=", (A,BC), (MVar(A),BC) :: MBoolean) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_notequals(readVar($arg1), unit($arg2))"""
    }
    infix (Ord) ("!=", (AC,B), (AC,B) :: MBoolean) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_notequals(unit($arg1), $arg2)"""
    }
    infix (Ord) ("!=", (AC,B), (AC,MVar(B)) :: MBoolean) implements redirect {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""forge_notequals(unit($arg1), readVar($arg2))"""
}

    val min = infix (Ord) ("min", List(A withBound TOrdering), List(A,A) :: A)
    val max = infix (Ord) ("max", List(A withBound TOrdering), List(A,A) :: A)
    impl (min) (codegen($cala, quotedArg(0) + " min " + quotedArg(1)))
    impl (max) (codegen($cala, quotedArg(0) + " max " + quotedArg(1)))
    for (g <- List(cuda,cpp)) {
      impl (min) (codegen(g, quotedArg(0) + " < " + quotedArg(1) + "?" + quotedArg(0) + ":" + quotedArg(1)))
      impl (max) (codegen(g, quotedArg(0) + " > " + quotedArg(1) + "?" + quotedArg(0) + ":" + quotedArg(1)))
    }
    //infix (Ord) ("compare", List(A withBound TOrdering), List(A,A) :: MInt) implements (codegen($cala, quotedArg(0) + " compare " + quotedArg(1)))
    val lt = infix (Ord) ("<", List(A withBound TOrdering), List(A,A) :: MBoolean)
    val lte = infix (Ord) ("<=", List(A withBound TOrdering), List(A,A) :: MBoolean)
    val gt = infix (Ord) (">", List(A withBound TOrdering), List(A,A) :: MBoolean)
    val gte = infix (Ord) (">=", List(A withBound TOrdering), List(A,A) :: MBoolean)

    for (g <- List($cala, cuda, cpp)) {
      impl (lt) (codegen(g, quotedArg(0) + " < " + quotedArg(1)))
      impl (lte) (codegen(g, quotedArg(0) + " <= " + quotedArg(1)))
      impl (gt) (codegen(g, quotedArg(0) + " > " + quotedArg(1)))
      impl (gte) (codegen(g, quotedArg(0) + " >= " + quotedArg(1)))
    }
  }

  def importStrings() = {
    val Str = grp("FString") // have to use either grp("xx") or MString, NOT grp("String") which is ambiguous with MString
    lift (Str) (MString)

    // overloaded variants of string concat
    val T = tpePar("T")

    val toInt = infix (Str) ("toInt", Nil, MString :: MInt)
    val toLong = infix (Str) ("toLong", Nil, MString :: MLong)
    val toFloat = infix (Str) ("toFloat", Nil, MString :: MFloat)
    val toDouble = infix (Str) ("toDouble", Nil, MString :: MDouble)
    val toBoolean = infix (Str) ("toBoolean", Nil, MString :: MBoolean)
    val trim = infix (Str) ("trim", Nil, MString :: MString)
    val fcharAt = infix (Str) ("fcharAt", Nil, (MString,MInt) :: MChar)
    val length = infix (Str) ("length", Nil, MString :: MInt)
    val startsWith = infix (Str) ("startsWith", Nil, (MString,MString) :: MBoolean)
    infix (Str) ("slice", Nil, (MString,MInt,MInt) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      val arg3 = quotedArg(2)
      s"""fstring_substring($arg1,$arg2,$arg3)"""
    }
    val endsWith = infix (Str) ("endsWith", Nil, (MString,MString) :: MBoolean)
    val contains = infix (Str) ("contains", Nil, (MString,MString) :: MBoolean)
    val substring1 = infix (Str) ("substring", Nil, (MString,MInt) :: MString)
    val substring2 = infix (Str) ("substring", Nil, (MString,MInt,MInt) :: MString)
    val toLowerCase = infix (Str) ("toLowerCase", Nil, MString :: MString)
    val toUpperCase = infix (Str) ("toUpperCase", Nil, MString :: MString)
    val getBytes = infix (Str) ("getBytes", Nil, MString :: MArray(MByte))
    val replaceAll = infix (Str) ("replaceAllLiterally", Nil, (MString,MString,MString) :: MString)

    impl (toInt) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.toInt"""
    }))
    impl (toLong) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.toLong"""
    }))
    impl (toFloat) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.toFloat"""
    }))
    impl (toDouble) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.toDouble"""
    }))
    impl (toBoolean) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.toBoolean"""
    }))
    impl (trim) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.trim"""
    }))
    impl (fcharAt) (codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1.charAt($arg2)"""
    }))
    impl (length) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.length"""
    }))
    impl (startsWith) (codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1.startsWith($arg2)"""
    }))
    impl (endsWith) (codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1.endsWith($arg2)"""
    }))
    impl (contains) (codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1.contains($arg2)"""
    }))
    impl (substring1) (codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1.substring($arg2)"""
    }))
    impl (substring2) (codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      val arg3 = quotedArg(2)
      s"""$arg1.substring($arg2,$arg3)"""
    }))
    impl (toLowerCase) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.toLowerCase"""
    }))
    impl (toUpperCase) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.toUpperCase"""
    }))
    impl (getBytes) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.getBytes()"""
    }))
    impl (replaceAll) (codegen($cala, {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  val arg3 = quotedArg(2)
  s"""$arg1.replaceAllLiterally($arg2,$arg3)"""
}))

    impl (toInt) (codegen(cpp, {
      val arg1 = quotedArg(0)
      s"""string_toInt($arg1)"""
    }))
    impl (toLong) (codegen(cpp, {
      val arg1 = quotedArg(0)
      s"""string_toLong($arg1)"""
    }))
    impl (toFloat) (codegen(cpp, {
      val arg1 = quotedArg(0)
      s"""string_toFloat($arg1)"""
    }))
    impl (toDouble) (codegen(cpp, {
      val arg1 = quotedArg(0)
      s"""string_toDouble($arg1)"""
    }))
    impl (toBoolean) (codegen(cpp, {
      val arg1 = quotedArg(0)
      s"""string_toBoolean($arg1)"""
    }))
    impl (trim) (codegen(cpp, {
      val arg1 = quotedArg(0)
      s"""string_trim($arg1)"""
    }))
    impl (fcharAt) (codegen(cpp, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""string_charAt($arg1,$arg2)"""
    }))
    impl (startsWith) (codegen(cpp, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""string_startsWith($arg1,$arg2)"""
    }))
    impl (length) (codegen(cpp, {
      val arg1 = quotedArg(0)
      s"""string_length($arg1)"""
    }))
    impl (endsWith) (codegen(cpp, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""string_endsWith($arg1,$arg2)"""
    }))
    impl (contains) (codegen(cpp, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""string_contains($arg1,$arg2)"""
    }))
    impl (substring1) (codegen(cpp, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""string_substr($arg1,$arg2)"""
    }))
    impl (substring2) (codegen(cpp, {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  val arg3 = quotedArg(2)
  s"""string_substr($arg1,$arg2,$arg3)"""
}))

    // We leave fsplit, though deprecated, here for compatibility
    infix (Str) ("fsplit", Nil, MethodSignature(List(MString,MString,("numSplits",MInt,"unit(0)")), MArray(MString))) implements redirect {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  val arg3 = quotedArg(2)
  s"""$arg1.split($arg2, $arg3)"""
}

    infix (Str) ("split", Nil, MethodSignature(List(MString,MString,("numSplits",MInt,"unit(0)")), MArray(MString))) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        val arg3 = quotedArg(2)
        s"""array_string_split($arg1,$arg2,$arg3)"""
      }

    val B = tpePar("B")
    val concat = direct (Str) ("forge_string_plus", (T,B), (T,B) :: MString)
    impl (concat) (codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1.toString + $arg2.toString"""
    }))
    impl (concat) (codegen(cpp, "string_plus( convert_to_string< " + unquotes("remapWithRef("+opArgPrefix+"0.tp)") + ">(" + quotedArg(0) + "), convert_to_string< " + unquotes("remapWithRef("+opArgPrefix+"1.tp)") + ">(" + quotedArg(1) + "))"))

    // TODO: check these combinations to see if they could be condensed or if there is anything missing

    infix (Str) ("+", T, (CString, T) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_string_plus(unit($arg1), $arg2)"""
    }
    infix (Str) ("+", T, (MString, T) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_string_plus($arg1, $arg2)"""
    }
    infix (Str) ("+", T, (CString, MVar(T)) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_string_plus(unit($arg1), readVar($arg2))"""
    }
    infix (Str) ("+", T, (MString, MVar(T)) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_string_plus($arg1, readVar($arg2))"""
    }
    infix (Str) ("+", T, (MVar(MString), T) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_string_plus(readVar($arg1), $arg2)"""
    }
    infix (Str) ("+", T, (MVar(MString), MVar(T)) :: MString) implements redirect {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""forge_string_plus(readVar($arg1), readVar($arg2))"""
}

    infix (Str) ("+", T, (T, CString) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_string_plus($arg1, unit($arg2))"""
    }
    infix (Str) ("+", T, (T, MString) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_string_plus($arg1, $arg2)"""
    }
    infix (Str) ("+", T, (MVar(T), CString) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_string_plus(readVar($arg1), unit($arg2))"""
    }
    infix (Str) ("+", T, (MVar(T), MString) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_string_plus(readVar($arg1), $arg2)"""
    }
    infix (Str) ("+", T, (MVar(T), MVar(MString)) :: MString) implements redirect {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""forge_string_plus(readVar($arg1), readVar($arg2))"""
}

    // infix (Str) ("+", Nil, (MString, CString) :: MString) implements redirect ${ forge_string_plus($0, unit($1)) }
    infix (Str) ("+", Nil, (CString, MString) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_string_plus(unit($arg1), $arg2)"""
    }
    infix (Str) ("+", Nil, (MString, MString) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_string_plus($arg1, $arg2)"""
    }
    infix (Str) ("+", Nil, (MString, MVar(MString)) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_string_plus($arg1, readVar($arg2))"""
    }
    infix (Str) ("+", Nil, (MVar(MString), MString) :: MString) implements redirect {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""forge_string_plus(readVar($arg1), $arg2)"""
    }
    infix (Str) ("+", Nil, (MVar(MString), MVar(MString)) :: MString) implements redirect {
    val arg1 = quotedArg(0)
    val arg2 = quotedArg(1)
    s"""forge_string_plus(readVar($arg1), readVar($arg2))"""
  }
  }

  def importMath() = {
    val Math = grp("Math")

    // constants
    val inf = direct (Math) ("INF", Nil, Nil :: MDouble)
    val ninf = direct (Math) ("nINF", Nil, Nil :: MDouble)
    direct (Math) ("Pi", Nil, Nil :: MDouble) implements redirect {
      s"""unit(java.lang.Math.PI)"""
    }
    direct (Math) ("E", Nil, Nil :: MDouble) implements redirect {
  s"""unit(java.lang.Math.E)"""
}

    impl (inf) (codegen($cala, "Double.PositiveInfinity"))
    impl (ninf) (codegen($cala, "Double.NegativeInfinity"))
    impl (inf) (codegen(cpp, "std::numeric_limits<double>::infinity()"))
    impl (ninf) (codegen(cpp, "-std::numeric_limits<double>::infinity()"))
    impl (inf) (codegen(cuda, "__longlong_as_double(0x7ff0000000000000ULL)"))
    impl (ninf) (codegen(cuda, "__longlong_as_double(0xfff0000000000000ULL)"))

    // methods
    val bitCount = static (Math) ("bitcount", Nil, MLong :: MInt)
    val abs = static (Math) ("abs", Nil, MDouble :: MDouble)
    val exp = static (Math) ("exp", Nil, MDouble :: MDouble)
    val log = static (Math) ("log", Nil, MDouble :: MDouble)
    val log10 = static (Math) ("log10", Nil, MDouble :: MDouble)
    val sqrt = static (Math) ("sqrt", Nil, MDouble :: MDouble)
    val ceil = static (Math) ("ceil", Nil, MDouble :: MDouble)
    val floor = static (Math) ("floor", Nil, MDouble :: MDouble)
    val round = static (Math) ("round", Nil, MDouble :: MLong)
    val sin = static (Math) ("sin", Nil, MDouble :: MDouble)
    val sinh = static (Math) ("sinh", Nil, MDouble :: MDouble)
    val asin = static (Math) ("asin", Nil, MDouble :: MDouble)
    val cos = static (Math) ("cos", Nil, MDouble :: MDouble)
    val cosh = static (Math) ("cosh", Nil, MDouble :: MDouble)
    val acos = static (Math) ("acos", Nil, MDouble :: MDouble)
    val tan = static (Math) ("tan", Nil, MDouble :: MDouble)
    val tanh = static (Math) ("tanh", Nil, MDouble :: MDouble)
    val atan = static (Math) ("atan", Nil, MDouble :: MDouble)
    val atan2 = static (Math) ("atan2", Nil, (MDouble, MDouble) :: MDouble)
    val pow = static (Math) ("pow", Nil, (MDouble, MDouble) :: MDouble)
    val max = static (Math) ("max", Nil, (MDouble,MDouble) :: MDouble)
    val min = static (Math) ("min", Nil, (MDouble,MDouble) :: MDouble)

    impl (bitCount) (codegen($cala, "java.lang.Long.bitCount(" + quotedArg(0) + ")"))
    impl (abs) (codegen($cala, "java.lang.Math.abs(" + quotedArg(0) + ")"))
    impl (exp) (codegen($cala, "java.lang.Math.exp(" + quotedArg(0) + ")"))
    impl (log) (codegen($cala, "java.lang.Math.log(" + quotedArg(0) + ")"))
    impl (log10) (codegen($cala, "java.lang.Math.log10(" + quotedArg(0) + ")"))
    impl (sqrt) (codegen($cala, "java.lang.Math.sqrt(" + quotedArg(0) + ")"))
    impl (ceil) (codegen($cala, "java.lang.Math.ceil(" + quotedArg(0) + ")"))
    impl (floor) (codegen($cala, "java.lang.Math.floor(" + quotedArg(0) + ")"))
    impl (round) (codegen($cala, "java.lang.Math.round(" + quotedArg(0) + ")"))
    impl (sin) (codegen($cala, "java.lang.Math.sin(" + quotedArg(0) + ")"))
    impl (sinh) (codegen($cala, "java.lang.Math.sinh(" + quotedArg(0) + ")"))
    impl (asin) (codegen($cala, "java.lang.Math.asin(" + quotedArg(0) + ")"))
    impl (cos) (codegen($cala, "java.lang.Math.cos(" + quotedArg(0) + ")"))
    impl (cosh) (codegen($cala, "java.lang.Math.cosh(" + quotedArg(0) + ")"))
    impl (acos) (codegen($cala, "java.lang.Math.acos(" + quotedArg(0) + ")"))
    impl (tan) (codegen($cala, "java.lang.Math.tan(" + quotedArg(0) + ")"))
    impl (tanh) (codegen($cala, "java.lang.Math.tanh(" + quotedArg(0) + ")"))
    impl (atan) (codegen($cala, "java.lang.Math.atan(" + quotedArg(0) + ")"))
    impl (atan2) (codegen($cala, "java.lang.Math.atan2(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
    impl (pow) (codegen($cala, "java.lang.Math.pow(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
    impl (max) (codegen($cala, "java.lang.Math.max(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
    impl (min) (codegen($cala, "java.lang.Math.min(" + quotedArg(0) + ", " + quotedArg(1) + ")"))

    for (g <- List(cuda, cpp)) {
      impl (abs) (codegen(g, "fabs(" + quotedArg(0) + ")"))
      impl (exp) (codegen(g, "exp(" + quotedArg(0) + ")"))
      impl (log) (codegen(g, "log(" + quotedArg(0) + ")"))
      impl (log10) (codegen(g, "log10(" + quotedArg(0) + ")"))
      impl (sqrt) (codegen(g, "sqrt(" + quotedArg(0) + ")"))
      impl (ceil) (codegen(g, "ceil(" + quotedArg(0) + ")"))
      impl (floor) (codegen(g, "floor(" + quotedArg(0) + ")"))
      impl (round) (codegen(g, "(int64_t) round(" + quotedArg(0) + ")"))
      impl (sin) (codegen(g, "sin(" + quotedArg(0) + ")"))
      impl (sinh) (codegen(g, "sinh(" + quotedArg(0) + ")"))
      impl (asin) (codegen(g, "asin(" + quotedArg(0) + ")"))
      impl (cos) (codegen(g, "cos(" + quotedArg(0) + ")"))
      impl (cosh) (codegen(g, "cosh(" + quotedArg(0) + ")"))
      impl (acos) (codegen(g, "acos(" + quotedArg(0) + ")"))
      impl (tan) (codegen(g, "tan(" + quotedArg(0) + ")"))
      impl (tanh) (codegen(g, "tan(" + quotedArg(0) + ")"))
      impl (atan) (codegen(g, "atan(" + quotedArg(0) + ")"))
      impl (atan2) (codegen(g, "atan2(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
      impl (pow) (codegen(g, "pow(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
      impl (max) (codegen(g, "fmax(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
      impl (min) (codegen(g, "fmin(" + quotedArg(0) + ", " + quotedArg(1) + ")"))
    }
  }

  def importTuples() = {
    val e = tpePar("_")

    // only go to 10 for the sake of compile-time overhead (for now)
    for (arity <- (2 until maxTuples)) {
      val pars = (0 until arity).map(i => tpePar(('A'.toInt+i).toChar.toString)).toList
      val elems = (0 until arity).map(i => "_" + (i+1))

      // the abstract name needs to be different than the Scala name, since we don't want to shadow it.
      val TT = tpe("Tup" + arity, pars)
      data(TT, elems.zip(pars): _*)

      for (i <- 0 until arity) {
        val concrete = pars.zipWithIndex.map(t => if (t._2 == i) t._1 else e)
        infix (TT) ("_"+(i+1), pars(i), TT(concrete: _*) :: pars(i)) implements getter(0, elems(i))
      }

      val CT = tpe("Tuple"+arity, pars, stage = compile)
      val unpackTupleStr = "(" + elems.zipWithIndex.map(t => "tup"+arity+"__"+(t._2+1)+"(t)").mkString("(",",",")") + ")"
      val parStr = elems.map(e => "t."+e)

      direct (TT) ("unpack", pars, ("t",TT(pars: _*)) :: CT(pars: _*)) implements composite {
        s"""$unpackTupleStr"""
      }
      direct (TT) ("pack", pars, ("t",CT(pars: _*)) :: TT(pars: _*)) implements composite ("internal_pack" + arity + "(" + parStr.mkString(",") + ")")

      // internal_pack is necessary so that we don't store a stage = now type (CT) in a Delite IR node, which expects Reps.
      val argStr = (0 until pars.length).map(i => unit(quotedArg(i)))
      compiler (TT) ("internal_pack" + arity, pars, (pars :: TT(pars: _*))) implements allocates(TT, argStr: _*)

      val makeTupleStrStr = "\"(\"+" + (1 to arity).map(i => "t._"+i).mkString("+\",\"+") + "+\")\""
      infix (TT) ("toString", pars, ("t",TT(pars: _*)) :: MString) implements composite {
      s"""$makeTupleStrStr"""
    }
    }

    // using an implicit conversion requires us to name all of the type parameters, whereas infix does not
    for (arity <- 1 until maxTuples) {
      mustInfixList ::= "_" + arity
    }

    // add pack for Var combinations inside Tuple2s. We don't do this for all of them,
    // since the number of T,Var[T],Rep[T] combinations is exponential in the size of the tuple
    val Tuple2 = lookupTpe("Tup2")
    val A = tpePar("A")
    val B = tpePar("B")
    direct (Tuple2) ("pack", (A,B), CTuple2(MVar(A),B) :: Tuple2(A,B)) implements redirect {
      val arg1 = quotedArg(0)
      s"""tup2_pack(($arg1._1,$arg1._2))"""
    }
    direct (Tuple2) ("pack", (A,B), CTuple2(A,MVar(B)) :: Tuple2(A,B)) implements redirect {
      val arg1 = quotedArg(0)
      s"""tup2_pack(($arg1._1,$arg1._2))"""
    }
    direct (Tuple2) ("pack", (A,B), CTuple2(MVar(A),MVar(B)) :: Tuple2(A,B)) implements redirect {
    val arg1 = quotedArg(0)
    s"""tup2_pack(($arg1._1,$arg1._2))"""
  }
  }

  // Forge's HashMap is not mutable, so a Scala HashMap can be used if updates are necessary.
  def importHashMap() = {
    val K = tpePar("K")
    val V = tpePar("V")
    val T = tpePar("T")

    // in order to define lifted operations on an existing Scala type, we must place the lifted ops in a separate group
    // to avoid Forge attempting to use the fully qualified type name in traits
    val SArray = tpe("scala.Array", T)
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))
    val HashMapOps = grp("SHashMap")

    val hashmap = direct (HashMapOps) ("SHashMap", (K,V), Nil :: SHashMap(K,V), effect = mutable)
    impl (hashmap) (codegen($cala, {
      val tpeK = quotedTpe("K", hashmap)
      val tpeV = quotedTpe("V", hashmap)
      s"""new scala.collection.mutable.HashMap[$tpeK,$tpeV]()"""
    }))
    impl (hashmap) (codegen(cpp, {
  val tpeK = quotedTpe("K", hashmap)
  val tpeV = quotedTpe("V", hashmap)
  s"""new std::map<$tpeK,$tpeV>()"""
}))

    compiler (HashMapOps) ("shashmap_from_arrays", (K,V), (MArray(K),MArray(V)) :: SHashMap(K,V), effect = mutable) implements codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""scala.collection.mutable.HashMap($arg1.zip($arg2): _*)"""
    })
    val keys_array = compiler (HashMapOps) ("shashmap_keys_array", (K,V), (SHashMap(K,V)) :: SArray(K))
    val values_array = compiler (HashMapOps) ("shashmap_values_array", (K,V), (SHashMap(K,V)) :: SArray(V))
    impl (keys_array) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.keys.toArray"""
    }))
    impl (values_array) (codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.values.toArray"""
    }))
    impl (keys_array) (codegen(cpp, "new " + unquotes("remap(sym.tp)") + {
      val arg1 = quotedArg(0)
      val tpeK = quotedTpe("K", keys_array)
      val tpeV = quotedTpe("V", keys_array)
      s"""($arg1->size()); int keys_idx_$arg1 = 0; for(std::map<$tpeK,$tpeV>::iterator it = $arg1->begin(); it != $arg1->end(); ++it)"""
    } + unquotes("quote(sym)") + {
      val arg1 = quotedArg(0)
      s"""->update(keys_idx_$arg1++, it->first);"""
    }))
    impl (values_array) (codegen(cpp, "new " + unquotes("remap(sym.tp)") + {
  val arg1 = quotedArg(0)
  val tpeK = quotedTpe("K", values_array)
  val tpeV = quotedTpe("V", values_array)
  s"""($arg1->size()); int values_idx_$arg1 = 0; for(std::map<$tpeK,$tpeV>::iterator it = $arg1->begin(); it != $arg1->end(); ++it)"""
} + unquotes("quote(sym)") + {
  val arg1 = quotedArg(0)
  s"""->update(values_idx_$arg1++, it->second);"""
}))

    val apply = infix (HashMapOps) ("apply", (K,V), (SHashMap(K,V), K) :: V)
    val update = infix (HashMapOps) ("update", (K,V), (SHashMap(K,V), K, V) :: MUnit, effect = write(0))
    val contains = infix (HashMapOps) ("contains", (K,V), (SHashMap(K,V), K) :: MBoolean)
    infix (HashMapOps) ("keys", (K,V), SHashMap(K,V) :: MArray(K)) implements composite {
      val arg1 = quotedArg(0)
      s"""farray_from_sarray(shashmap_keys_array($arg1))"""
    }
    infix (HashMapOps) ("values", (K,V), SHashMap(K,V) :: MArray(V)) implements composite {
  val arg1 = quotedArg(0)
  s"""farray_from_sarray(shashmap_values_array($arg1))"""
}

    impl (apply) (codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1($arg2)"""
    }))
    impl (apply) (codegen(cpp, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1->find($arg2)->second"""
    }))
    impl (update) (codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      val arg3 = quotedArg(2)
      s"""$arg1.put($arg2,$arg3); ()"""
    }))
    impl (update) (codegen(cpp, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      val arg3 = quotedArg(2)
      val tpeK = quotedTpe("K", update)
      val tpeV = quotedTpe("V", update)
      s"""$arg1->insert(std::pair<$tpeK,$tpeV>($arg2,$arg3))"""
    }))
    impl (contains) (codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1.contains($arg2)"""
    }))
    impl (contains) (codegen(cpp, {
    val arg1 = quotedArg(0)
    val arg2 = quotedArg(1)
    s"""$arg1->find($arg2) != $arg1->end()"""
  }))
  }

  def importConcurrentHashMap() = {
    val K = tpePar("K")
    val V = tpePar("V")
    val T = tpePar("T")

    val SArray = tpe("scala.Array", T)
    val CHashMap = tpe("java.util.concurrent.ConcurrentHashMap", (K,V))
    val HashMapOps = grp("CHashMap")

    direct (HashMapOps) ("CHashMap", (K,V), Nil :: CHashMap(K,V), effect = mutable) implements codegen($cala, {
      val tpeK = quotedTpe("K", lookupOp(HashMapOps,"CHashMap"))
      val tpeV = quotedTpe("V", lookupOp(HashMapOps,"CHashMap"))
      s"""new java.util.concurrent.ConcurrentHashMap[$tpeK,$tpeV]()"""
    })
    compiler (HashMapOps) ("chashmap_from_arrays", (K,V), (MArray(K),MArray(V)) :: CHashMap(K,V), effect = mutable) implements codegen($cala, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        val tpeK = quotedTpe("K", lookupOp(HashMapOps,"chashmap_from_arrays"))
        val tpeV = quotedTpe("V", lookupOp(HashMapOps,"chashmap_from_arrays"))
        s"""val map = new java.util.concurrent.ConcurrentHashMap[$tpeK,$tpeV]()
for (i <- 0 until $arg1.length) {
  map.put($arg1(i),$arg2(i))
}
map"""
      })
    compiler (HashMapOps) ("chashmap_keys_array", (K,V), (CHashMap(K,V)) :: SArray(K)) implements codegen($cala, {
      val arg1 = quotedArg(0)
      s"""scala.collection.JavaConverters.enumerationAsScalaIteratorConverter($arg1.keys).asScala.toArray"""
    })
    compiler (HashMapOps) ("chashmap_values_array", (K,V), (CHashMap(K,V)) :: SArray(V)) implements codegen($cala, {
  val arg1 = quotedArg(0)
  s"""scala.collection.JavaConverters.collectionAsScalaIterableConverter($arg1.values).asScala.toArray"""
})

    infix (HashMapOps) ("apply", (K,V), (CHashMap(K,V), K) :: V) implements codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1.get($arg2)"""
    })
    infix (HashMapOps) ("update", (K,V), (CHashMap(K,V), K, V) :: MUnit, effect = write(0)) implements codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      val arg3 = quotedArg(2)
      s"""$arg1.put($arg2,$arg3); ()"""
    })
    infix (HashMapOps) ("contains", (K,V), (CHashMap(K,V), K) :: MBoolean) implements codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1.containsKey($arg2)"""
    })
    infix (HashMapOps) ("keys", (K,V), CHashMap(K,V) :: MArray(K)) implements composite {
      val arg1 = quotedArg(0)
      s"""farray_from_sarray(chashmap_keys_array($arg1))"""
    }
    infix (HashMapOps) ("values", (K,V), CHashMap(K,V) :: MArray(V)) implements composite {
    val arg1 = quotedArg(0)
    s"""farray_from_sarray(chashmap_values_array($arg1))"""
  }
  }

  def importByteBuffer() = {
    val ByteBuffer = tpe("java.nio.ByteBuffer")
    val IntBuffer = tpe("java.nio.IntBuffer")
    val DoubleBuffer = tpe("java.nio.DoubleBuffer")

    val ByteBufferOps = grp("SByteBuffer")
    direct (ByteBufferOps) ("ByteBuffer", Nil, MInt :: ByteBuffer, effect = mutable) implements codegen($cala, {
      val arg1 = quotedArg(0)
      s"""java.nio.ByteBuffer.allocate($arg1)"""
    })
    direct (ByteBufferOps) ("ByteBufferWrap", Nil, MArray(MByte) :: ByteBuffer, effect = mutable) implements codegen($cala, {
  val arg1 = quotedArg(0)
  s"""java.nio.ByteBuffer.wrap($arg1)"""
})

    infix (ByteBufferOps) ("rewind", Nil, ByteBuffer :: MUnit, effect = write(0)) implements codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.rewind(); ()"""
    })
    infix (ByteBufferOps) ("array", Nil, ByteBuffer :: MArray(MByte)) implements codegen($cala, {
  val arg1 = quotedArg(0)
  s"""$arg1.array"""
})

    infix (ByteBufferOps) ("getInt", Nil, ByteBuffer :: MInt) implements codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.getInt()"""
    })
    infix (ByteBufferOps) ("getDouble", Nil, ByteBuffer :: MDouble) implements codegen($cala, {
      val arg1 = quotedArg(0)
      s"""$arg1.getDouble()"""
    })
    infix (ByteBufferOps) ("putInt", Nil, (ByteBuffer, MInt) :: ByteBuffer, effect = write(0)) implements codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1.putInt($arg2)"""
    })
    infix (ByteBufferOps) ("putDouble", Nil, (ByteBuffer, MDouble) :: ByteBuffer, effect = write(0)) implements codegen($cala, {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""$arg1.putDouble($arg2)"""
})

    // We deviate slightly from the actual ByteBuffer API here to observe our nested mutability rules by chaining the operations together implicitly.
    infix (ByteBufferOps) ("get", Nil, (ByteBuffer, MArray(MInt), MInt, MInt) :: IntBuffer, effect = write(1)) implements codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      val arg3 = quotedArg(2)
      val arg4 = quotedArg(3)
      s"""$arg1.asIntBuffer.get($arg2, $arg3, $arg4)"""
    })
    infix (ByteBufferOps) ("get", Nil, (ByteBuffer, MArray(MDouble), MInt, MInt) :: DoubleBuffer, effect = write(1)) implements codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      val arg3 = quotedArg(2)
      val arg4 = quotedArg(3)
      s"""$arg1.asDoubleBuffer.get($arg2, $arg3, $arg4)"""
    })
    infix (ByteBufferOps) ("put", Nil, (ByteBuffer, MArray(MInt), MInt, MInt) :: IntBuffer, effect = write(0)) implements codegen($cala, {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      val arg3 = quotedArg(2)
      val arg4 = quotedArg(3)
      s"""$arg1.asIntBuffer.put($arg2, $arg3, $arg4)"""
    })
    infix (ByteBufferOps) ("put", Nil, (ByteBuffer, MArray(MDouble), MInt, MInt) :: DoubleBuffer, effect = write(0)) implements codegen($cala, {
    val arg1 = quotedArg(0)
    val arg2 = quotedArg(1)
    val arg3 = quotedArg(2)
    val arg4 = quotedArg(3)
    s"""$arg1.asDoubleBuffer.put($arg2, $arg3, $arg4)"""
  })
  }
}
