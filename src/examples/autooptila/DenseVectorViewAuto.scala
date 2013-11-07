package ppl.dsl.forge
package examples
package autooptila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait DenseVectorViewOps {
  this: AutoOptiLADSL with ppl.dsl.forge.core.ForgeOpsExp =>

  def importDenseVectorViewOps() {
    val DenseVectorView = lookupTpe("DenseVectorView")
    val IndexVector = lookupTpe("IndexVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseVector = lookupTpe("DenseVector")

    data(DenseVectorView, ("_data", MArray(tpePar("T"))), ("_start", MInt), ("_stride", MInt), ("_length", MInt), ("_isRow", MBoolean))

    val DenseVectorViewOps = withTpe (DenseVectorView)

    compiler (DenseVectorView) ("densevectorview_data", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVectorView, None)), MArray(tpePar("T"))), implicitArgs = List()) implements getter(0, "_data")
    compiler (DenseVectorView) ("densevectorview_start", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVectorView, None)), MInt), implicitArgs = List()) implements getter(0, "_start")
    compiler (DenseVectorView) ("densevectorview_stride", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVectorView, None)), MInt), implicitArgs = List()) implements getter(0, "_stride")
    compiler (DenseVectorView) ("densevectorview_illegalalloc", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVectorView, None), Arg("__arg1", MInt, None)), MNothing), implicitArgs = List(), effect = simple) implements composite("fatal(unit(\"DenseVectorViews cannot be allocated from a parallel op\"))")
    compiler (DenseVectorView) ("densevectorview_illegalupdate", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVectorView, None), Arg("__arg1", MInt, None), Arg("__arg2", tpePar("T"), None)), MNothing), implicitArgs = List(), effect = simple) implements composite("fatal(unit(\"DenseVectorViews cannot be updated\"))")
    compiler (DenseVectorView) ("densevectorview_densevector_filter_map", List(tpePar("T"), tpePar("R")), MethodSignature(List(Arg("self", DenseVectorView, None), Arg("__arg1", (tpePar("T")) ==> MBoolean, None), Arg("__arg2", (tpePar("T")) ==> tpePar("R"), None)), DenseVector(tpePar("R"))), implicitArgs = List()) implements filter(scala.Tuple2(tpePar("T"), tpePar("R")), 0, "((e: this.Rep[T]) => __arg1.apply(e))", "((e: this.Rep[T]) => __arg2.apply(e))")
    static (DenseVectorView) ("apply", List(tpePar("T")), MethodSignature(List(Arg("__arg0", MArray(tpePar("T")), None), Arg("__arg1", MInt, None), Arg("__arg2", MInt, None), Arg("__arg3", MInt, None), Arg("__arg4", MBoolean, None)), DenseVectorView(tpePar("T"))), implicitArgs = List()) implements allocates(DenseVectorView, "__arg0", "__arg1", "__arg2", "__arg3", "__arg4")
    DenseVectorViewOps {
      infix ("toBoolean") (MethodSignature(Nil, DenseVector(MBoolean)), implicitArgs = List(("conv", (tpePar("T")) ==> MBoolean))) implements map(scala.Tuple2(tpePar("T"), MBoolean), 0, "conv")
      infix ("toDouble") (MethodSignature(Nil, DenseVector(MDouble)), implicitArgs = List(("conv", (tpePar("T")) ==> MDouble))) implements map(scala.Tuple2(tpePar("T"), MDouble), 0, "conv")
      infix ("toFloat") (MethodSignature(Nil, DenseVector(MFloat)), implicitArgs = List(("conv", (tpePar("T")) ==> MFloat))) implements map(scala.Tuple2(tpePar("T"), MFloat), 0, "conv")
      infix ("toInt") (MethodSignature(Nil, DenseVector(MInt)), implicitArgs = List(("conv", (tpePar("T")) ==> MInt))) implements map(scala.Tuple2(tpePar("T"), MInt), 0, "conv")
      infix ("indices") (MethodSignature(Nil, IndexVector)) implements composite("IndexVector(unit(0), self.length(), self.isRow())")
      infix ("isEmpty") (MethodSignature(Nil, MBoolean)) implements single("infix_$eq$eq(self.length(), unit(0))")
      infix ("first") (MethodSignature(Nil, tpePar("T"))) implements single("self.apply(unit(0))")
      infix ("last") (MethodSignature(Nil, tpePar("T"))) implements single("self.apply(self.length().$minus(unit(1)))")
      infix ("contains") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), MBoolean)) implements single("""{
  var found: this.Rep[Boolean] = __newVar(unit(false));
  var i: this.Rep[Int] = __newVar(unit(0));
  __whileDo(i.$less(self.length()).$amp$amp(found.unary_$bang), {
    __ifThenElse(infix_$eq$eq(self.apply(i), __arg0), __assign(found, unit(true)), unit(()));
    __assign(i, i.$plus(unit(1)))
  });
  found
}""")
      infix ("distinct") (MethodSignature(Nil, DenseVector(tpePar("T")))) implements single("""{
  val out: this.Rep[DenseVector[T]] = DenseVector[T](unit(0), self.isRow());
  unit(0).until(self.length()).foreach(((i: this.Rep[Int]) => __ifThenElse(out.contains(self.apply(i)).unary_$bang, out.$less$less$eq(self.apply(i)), unit(()))));
  out.unsafeImmutable
}""")
      infix ("makeString") (MethodSignature(Nil, tpePar("String")), implicitArgs = List(TStringable(tpePar("T")))) implements single("""{
  var s: this.Rep[String] = __newVar(unit(""));
  __ifThenElse(infix_$eq$eq(self.length(), unit(0)), unit("[ ]"), __ifThenElse(self.isRow(), {
    __assign(s, s.$plus(unit("[")));
    unit(0).until(self.length().$minus(unit(1))).foreach(((i: this.Rep[Int]) => __assign(s, s.$plus(self.apply(i).makeStr()).$plus(unit(" ")))));
    __assign(s, s.$plus(self.apply(self.length().$minus(unit(1))).makeStr()));
    __assign(s, s.$plus(unit("]")))
  }, {
    unit(0).until(self.length().$minus(unit(1))).foreach(((i: this.Rep[Int]) => __assign(s, s.$plus(unit("[")).$plus(self.apply(i).makeStr()).$plus(unit("]\n")))));
    __assign(s, s.$plus(unit("[")).$plus(self.apply(self.length().$minus(unit(1))).makeStr()).$plus(unit("]")))
  }));
  s
}""")
      infix ("toString") (MethodSignature(Nil, tpePar("String"))) implements single("""{
  var s: this.Rep[String] = __newVar(unit(""));
  __ifThenElse(infix_$eq$eq(self.length(), unit(0)), unit("[ ]"), __ifThenElse(self.isRow(), {
    __assign(s, s.$plus(unit("[")));
    unit(0).until(self.length().$minus(unit(1))).foreach(((i: this.Rep[Int]) => __assign(s, s.$plus(self.apply(i)).$plus(unit(" ")))));
    __assign(s, s.$plus(self.apply(self.length().$minus(unit(1)))));
    __assign(s, s.$plus(unit("]")))
  }, {
    unit(0).until(self.length().$minus(unit(1))).foreach(((i: this.Rep[Int]) => __assign(s, s.$plus(unit("[")).$plus(self.apply(i)).$plus(unit("]\n")))));
    __assign(s, s.$plus(unit("[")).$plus(self.apply(self.length().$minus(unit(1)))).$plus(unit("]")))
  }));
  s
}""")
      infix ("pprint") (MethodSignature(Nil, MUnit), implicitArgs = List(TStringable(tpePar("T"))), effect = simple) implements composite("println(self.makeStr())")
      infix ("+") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), tpePar("T")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$plus(b))")
      infix ("-") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), tpePar("T")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$minus(b))")
      infix ("*") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), tpePar("T")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$times(b))")
      infix ("*:*") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), tpePar("T")), implicitArgs = List(TArith(tpePar("T")))) implements composite("""{
  __ifThenElse(infix_$bang$eq(self.length(), __arg0.length()), fatal(unit("dimension mismatch: vector dot product")), unit(()));
  self.$times(__arg0).sum()
}""")
      infix ("**") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements composite("""{
  __ifThenElse(self.isRow().$bar$bar(__arg0.isRow().unary_$bang), fatal(unit("dimension mismatch: vector outer product")), unit(()));
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](self.length(), __arg0.length());
  unit(0).until(self.length()).foreach(((i: this.Rep[Int]) => unit(0).until(__arg0.length()).foreach(((j: this.Rep[Int]) => out.update(i, j, self.apply(i).$times(__arg0.apply(j)))))));
  out.unsafeImmutable
}""")
      infix ("/") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), tpePar("T")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$div(b))")
      infix ("+") (MethodSignature(List(Arg("__arg0", DenseVectorView(tpePar("T")), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), tpePar("T")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$plus(b))")
      infix ("-") (MethodSignature(List(Arg("__arg0", DenseVectorView(tpePar("T")), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), tpePar("T")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$minus(b))")
      infix ("*") (MethodSignature(List(Arg("__arg0", DenseVectorView(tpePar("T")), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), tpePar("T")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$times(b))")
      infix ("*:*") (MethodSignature(List(Arg("__arg0", DenseVectorView(tpePar("T")), None)), tpePar("T")), implicitArgs = List(TArith(tpePar("T")))) implements composite("""{
  __ifThenElse(infix_$bang$eq(self.length(), __arg0.length()), fatal(unit("dimension mismatch: vector dot product")), unit(()));
  self.$times(__arg0).sum()
}""")
      infix ("**") (MethodSignature(List(Arg("__arg0", DenseVectorView(tpePar("T")), None)), DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements composite("""{
  __ifThenElse(self.isRow().$bar$bar(__arg0.isRow().unary_$bang), fatal(unit("dimension mismatch: vector outer product")), unit(()));
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](self.length(), __arg0.length());
  unit(0).until(self.length()).foreach(((i: this.Rep[Int]) => unit(0).until(__arg0.length()).foreach(((j: this.Rep[Int]) => out.update(i, j, self.apply(i).$times(__arg0.apply(j)))))));
  out.unsafeImmutable
}""")
      infix ("/") (MethodSignature(List(Arg("__arg0", DenseVectorView(tpePar("T")), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), tpePar("T")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$div(b))")
      infix ("zip") (CurriedMethodSignature(List(List(Arg("__arg0", DenseVector(tpePar("B")), None)), List(Arg("__arg1", (tpePar("T"),tpePar("B")) ==> tpePar("R"), None))), DenseVector(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("B"), tpePar("R"))) implements zip(scala.Tuple3(tpePar("T"), tpePar("B"), tpePar("R")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[B]) => __arg1.apply(a, b))")
      infix ("zip") (CurriedMethodSignature(List(List(Arg("__arg0", DenseVectorView(tpePar("B")), None)), List(Arg("__arg1", (tpePar("T"),tpePar("B")) ==> tpePar("R"), None))), DenseVector(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("B"), tpePar("R"))) implements zip(scala.Tuple3(tpePar("T"), tpePar("B"), tpePar("R")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[B]) => __arg1.apply(a, b))")
      infix ("+") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.$plus(__arg0))")
      infix ("-") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.$minus(__arg0))")
      infix ("*") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.$times(__arg0))")
      infix ("*") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements composite("""{
  __ifThenElse(self.isRow().unary_$bang, fatal(unit("dimension mismatch: vector * matrix")), unit(()));
  __arg0.t().mapRowsToVector[T](((row: this.Rep[DenseVectorView[T]]) => self.$times$colon$times(row)))
}""")
      infix ("/") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.$div(__arg0))")
      infix ("abs") (MethodSignature(Nil, DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.abs())")
      infix ("exp") (MethodSignature(Nil, DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.exp())")
      infix ("log") (MethodSignature(Nil, DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.log())")
      infix ("sum") (MethodSignature(Nil, tpePar("T")), implicitArgs = List(TArith(tpePar("T")))) implements reduce(tpePar("T"), 0, "implicitly[Arith[T]].zero(self.apply(unit(0)))", "((a: this.Rep[T], b: this.Rep[T]) => a.$plus(b))")
      infix ("mean") (MethodSignature(Nil, MDouble), implicitArgs = List(("conv", (tpePar("T")) ==> MDouble))) implements composite("self.map[Double](conv).sum().$div(self.length())")
      infix ("min") (MethodSignature(Nil, tpePar("T")), implicitArgs = List(TOrdering(tpePar("T")))) implements reduce(tpePar("T"), 0, "self.apply(unit(0))", "((a: this.Rep[T], b: this.Rep[T]) => __ifThenElse(a.$less(b), a, b))")
      infix ("max") (MethodSignature(Nil, tpePar("T")), implicitArgs = List(TOrdering(tpePar("T")))) implements reduce(tpePar("T"), 0, "self.apply(unit(0))", "((a: this.Rep[T], b: this.Rep[T]) => __ifThenElse(a.$greater(b), a, b))")
      infix ("minIndex") (MethodSignature(Nil, MInt), implicitArgs = List(TOrdering(tpePar("T")))) implements single("""{
  var min: this.Rep[T] = __newVar(self.apply(unit(0)));
  var minIndex: this.Rep[Int] = __newVar(unit(0));
  unit(0).until(self.length()).foreach(((i: this.Rep[Int]) => __ifThenElse(self.apply(i).$less(min), {
    __assign(min, self.apply(i));
    __assign(minIndex, i)
  }, unit(()))));
  minIndex
}""")
      infix ("maxIndex") (MethodSignature(Nil, MInt), implicitArgs = List(TOrdering(tpePar("T")))) implements single("""{
  var max: this.Rep[T] = __newVar(self.apply(unit(0)));
  var maxIndex: this.Rep[Int] = __newVar(unit(0));
  unit(0).until(self.length()).foreach(((i: this.Rep[Int]) => __ifThenElse(self.apply(i).$greater(max), {
    __assign(max, self.apply(i));
    __assign(maxIndex, i)
  }, unit(()))));
  maxIndex
}""")
      infix ("map") (MethodSignature(List(Arg("__arg0", (tpePar("T")) ==> tpePar("R"), None)), DenseVector(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements map(scala.Tuple2(tpePar("T"), tpePar("R")), 0, "((e: this.Rep[T]) => __arg0.apply(e))")
      infix ("reduce") (MethodSignature(List(Arg("__arg0", (tpePar("T"),tpePar("T")) ==> tpePar("T"), None)), tpePar("T")), implicitArgs = List(TArith(tpePar("T")))) implements reduce(tpePar("T"), 0, "implicitly[Arith[T]].zero(self.apply(unit(0)))", "((a: this.Rep[T], b: this.Rep[T]) => __arg0.apply(a, b))")
      infix ("filter") (MethodSignature(List(Arg("__arg0", (tpePar("T")) ==> MBoolean, None)), DenseVector(tpePar("T")))) implements filter(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => __arg0.apply(e))", "((e: this.Rep[T]) => e)")
      infix ("foreach") (MethodSignature(List(Arg("__arg0", (tpePar("T")) ==> MUnit, None)), MUnit)) implements foreach(tpePar("T"), 0, "((e: this.Rep[T]) => __arg0.apply(e))")
      infix ("find") (MethodSignature(List(Arg("__arg0", (tpePar("T")) ==> MBoolean, None)), IndexVector)) implements composite("IndexVector(self.indices().filter(((i: this.Rep[Int]) => __arg0.apply(self.apply(i)))))")
      infix ("partition") (MethodSignature(List(Arg("pred", (tpePar("T")) ==> MBoolean, None)), lookupTpe("Tup2")(DenseVector(tpePar("T")), DenseVector(tpePar("T"))))) implements single("""{
  val outT: this.Rep[DenseVector[T]] = DenseVector[T](unit(0), self.isRow());
  val outF: this.Rep[DenseVector[T]] = DenseVector[T](unit(0), self.isRow());
  unit(0).until(self.length()).foreach(((i: this.Rep[Int]) => {
    val x: this.Rep[T] = self.apply(i);
    __ifThenElse(pred.apply(x), outT.$less$less$eq(x), outF.$less$less$eq(x))
  }));
  scala.Tuple2.apply(outT.unsafeImmutable, outF.unsafeImmutable)
}""")
      infix ("flatMap") (MethodSignature(List(Arg("__arg0", (tpePar("T")) ==> DenseVector(tpePar("R")), None)), DenseVector(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements composite("DenseVector.flatten[R](self.map[DenseVector[R]](__arg0))")
      infix ("scan") (CurriedMethodSignature(List(List(Arg("zero", tpePar("R"), None)), List(Arg("__arg1", (tpePar("R"),tpePar("T")) ==> tpePar("R"), None))), DenseVector(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements single("""{
  val out: this.Rep[DenseVector[R]] = DenseVector[R](self.length(), self.isRow());
  out.update(unit(0), __arg1.apply(zero, self.apply(unit(0))));
  var i: this.Rep[Int] = __newVar(unit(1));
  __whileDo(i.$less(self.length()), {
    out.update(i, __arg1.apply(out.apply(i.$minus(unit(1))), self.apply(i)));
    __assign(i, i.$plus(unit(1)))
  });
  out.unsafeImmutable
}""")
      infix ("prefixSum") (MethodSignature(Nil, DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements composite("self.scan[T](implicitly[Arith[T]].zero(self.apply(unit(0))))(((a: this.Rep[T], b: this.Rep[T]) => a.$plus(b)))")
      infix ("length") (MethodSignature(Nil, MInt)) implements getter(0, "_length")
      infix ("isRow") (MethodSignature(Nil, MBoolean)) implements getter(0, "_isRow")
      infix ("apply") (MethodSignature(List(Arg("__arg0", MInt, None)), tpePar("T"))) implements composite("array_apply[T](densevectorview_data[T](self), densevectorview_start[T](self).$plus(__arg0.$times(densevectorview_stride[T](self))))")
      infix ("slice") (MethodSignature(List(Arg("start", MInt, None), Arg("end", MInt, None)), DenseVectorView(tpePar("T")))) implements composite("DenseVectorView[T](densevectorview_data[T](self), densevectorview_start[T](self).$plus(start.$times(densevectorview_stride[T](self))), densevectorview_stride[T](self), end.$minus(start), self.isRow())")
      infix ("toDense") (MethodSignature(Nil, DenseVector(tpePar("T")))) implements composite("self.map[T](((e: this.Rep[T]) => e))")
      infix ("drop") (MethodSignature(List(Arg("__arg0", MInt, None)), DenseVectorView(tpePar("T")))) implements composite("self.slice(__arg0, self.length())")
      infix ("take") (MethodSignature(List(Arg("__arg0", MInt, None)), DenseVectorView(tpePar("T")))) implements composite("self.slice(unit(0), __arg0)")
      infix ("count") (MethodSignature(List(Arg("__arg0", (tpePar("T")) ==> MBoolean, None)), MInt)) implements composite("densevectorview_densevector_filter_map[T, Int](self, __arg0, ((e: this.Rep[T]) => unit(1))).sum()")
    }
  }
}
