package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait ComplexOps {
  this: OptiLADSL =>

  def importComplexOps() {
    val Complex = tpe("Complex")
    
    data(Complex, ("_real", MDouble), ("_imag", MDouble))

    static (Complex) ("apply", Nil, (MDouble,MDouble) :: Complex) implements allocates(Complex, {
  val arg1 = quotedArg(0)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

    val ComplexOps = withTpe(Complex)
    ComplexOps {
   	  infix ("real") (Nil :: MDouble) implements getter(0, "_real")
   	  infix ("imag") (Nil :: MDouble) implements getter(0, "_imag")

   	  infix ("conj") (Nil :: Complex) implements composite {
     val self = quotedArg("self")
     s"""Complex($self.real, -$self.imag)"""
   }
   	  infix ("+") (Complex :: Complex) implements composite {
     val self = quotedArg("self")
     val arg1 = quotedArg(1)
     s"""Complex($self.real+$arg1.real, $self.imag+$arg1.imag)"""
   }
   	  infix ("-") (Complex :: Complex) implements composite {
     val self = quotedArg("self")
     val arg1 = quotedArg(1)
     s"""Complex($self.real-$arg1.real, $self.imag-$arg1.imag)"""
   }
   	  infix ("*") (Complex :: Complex) implements composite {
     val self = quotedArg("self")
     val arg1 = quotedArg(1)
     s"""Complex($self.real*$arg1.real - $self.imag*$arg1.imag, $self.real*$arg1.imag+$self.imag*$arg1.real)"""
   }
   	  infix ("/") (Complex :: Complex) implements composite {
     val self = quotedArg("self")
     val arg1 = quotedArg(1)
     s"""Complex(($self.real*$arg1.real+$self.imag*$arg1.imag)/(square($arg1.real)+square($arg1.imag)), ($self.imag*$arg1.real-$self.real*$arg1.imag)/(square($arg1.real)+square($arg1.imag)))"""
   }
   	  infix ("abs") (Nil :: Complex) implements composite {
     val self = quotedArg("self")
     s"""Complex(sqrt(square($self.real)+square($self.imag)), unit(0.0))"""
   }
   	  infix ("exp") (Nil :: Complex) implements composite {
     val self = quotedArg("self")
     s"""Complex(exp($self.real)*cos($self.imag), exp($self.real)*sin($self.imag))"""
   }
   	  infix ("log") (Nil :: Complex) implements composite {
  val self = quotedArg("self")
  s"""Complex(log(sqrt(square($self.real)+square($self.imag))), atan2($self.imag, $self.real))"""
}

   	  direct ("__equal") (Complex :: MBoolean) implements composite {
      val self = quotedArg("self")
      val arg1 = quotedArg(1)
      s"""$self.real == $arg1.real && $self.imag == $arg1.imag"""
    }
    }

    // add Complex to Arith
    val Arith = lookupGrp("Arith").asInstanceOf[Rep[DSLTypeClass]]
    val ComplexArith = tpeClassInst("ArithComplex", Nil, Arith(Complex))
    infix (ComplexArith) ("zero", Nil, Complex :: Complex) implements composite {
      s"""Complex(unit(0.0),unit(0.0))"""
    }
    infix (ComplexArith) ("empty", Nil, Nil :: Complex) implements composite {
      s"""Complex(unit(0.0),unit(0.0))"""
    }
    infix (ComplexArith) ("+", Nil, (Complex,Complex) :: Complex) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""complex_pl($arg1,$arg2)"""
    }
    infix (ComplexArith) ("-", Nil, (Complex,Complex) :: Complex) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""complex_sub($arg1,$arg2)"""
    }
    infix (ComplexArith) ("*", Nil, (Complex,Complex) :: Complex) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""complex_mul($arg1,$arg2)"""
    }
    infix (ComplexArith) ("/", Nil, (Complex,Complex) :: Complex) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""complex_div($arg1,$arg2)"""
    }
    infix (ComplexArith) ("abs", Nil, Complex :: Complex) implements composite {
      val arg1 = quotedArg(0)
      s"""complex_abs($arg1)"""
    }
    infix (ComplexArith) ("exp", Nil, Complex :: Complex) implements composite {
      val arg1 = quotedArg(0)
      s"""complex_exp($arg1)"""
    }
    infix (ComplexArith) ("log", Nil, Complex :: Complex) implements composite {
  val arg1 = quotedArg(0)
  s"""complex_log($arg1)"""
}

    // add Complex to Stringable
    val Stringable = lookupGrp("Stringable").asInstanceOf[Rep[DSLTypeClass]]
    val ComplexStringable = tpeClassInst("StringableComplex", Nil, Stringable(Complex))
    infix (ComplexStringable) ("makeStr", Nil, Complex :: MString) implements composite {
  val arg1 = quotedArg(0)
  s"""if ($arg1.imag < unit(0.0)) {
	    $arg1.real.makeStr + " - " + abs($arg1.imag) + "i"
	  }
	  else {
	    $arg1.real.makeStr + " + " + abs($arg1.imag) + "i"
	  }"""
}

  }
}
