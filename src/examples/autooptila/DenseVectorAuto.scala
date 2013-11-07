package ppl.dsl.forge
package examples
package autooptila

import core.{ForgeApplication,ForgeApplicationRunner}

trait DenseVectorOps {
  this: AutoOptiLADSL with ppl.dsl.forge.core.ForgeOpsExp =>

  def importDenseVectorOps() {
    val DenseVectorView = lookupTpe("DenseVectorView")
    val IndexVector = lookupTpe("IndexVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseVector = lookupTpe("DenseVector")

    data(DenseVector, ("_length", MInt), ("_isRow", MBoolean), ("_data", MArray(tpePar("T"))))

    val DenseVectorOps = withTpe (DenseVector)

    compiler (DenseVector) ("densevector_fromarray", List(tpePar("T")), MethodSignature(List(Arg("__arg0", MArray(tpePar("T")), None), Arg("__arg1", MBoolean, None)), DenseVector(tpePar("T"))), implicitArgs = List()) implements allocates(DenseVector, "array_length[T](__arg0)", "__arg1", "__arg0")
    compiler (DenseVector) ("densevector_fromfunc", List(tpePar("T")), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", (MInt) ==> tpePar("T"), None)), DenseVector(tpePar("T"))), implicitArgs = List()) implements composite("""{
  val x$1: this.Rep[Int] = unit(0);
  __arg0.$colon$colon(x$1).apply[T](((i: this.Rep[Int]) => __arg1.apply(i)))
}""")
    compiler (DenseVector) ("densevector_precumulate", List(tpePar("T")), MethodSignature(List(Arg("v", DenseVector(tpePar("T")), None), Arg("identity", tpePar("T"), None), Arg("func", (tpePar("T"),tpePar("T")) ==> tpePar("T"), None)), lookupTpe("Tup2")(tpePar("T"), DenseVector(tpePar("T")))), implicitArgs = List()) implements composite("""__ifThenElse(infix_$eq$eq(v.length(), unit(0)), scala.Tuple2.apply(identity, DenseVector[T](unit(0), v.isRow()).unsafeImmutable), {
  val result: this.Rep[DenseVector[T]] = DenseVector[T](unit(0), v.isRow());
  var accum: this.Rep[T] = __newVar(identity);
  unit(0).until(v.length()).foreach(((i: this.Rep[Int]) => {
    result.$less$less$eq(accum);
    __assign(accum, func.apply(accum, v.apply(i)))
  }));
  scala.Tuple2.apply(accum, result.unsafeImmutable)
})""")
    /*
    compiler (DenseVector) ("densevector_raw_alloc", List(tpePar("R"), tpePar("CR")), MethodSignature(List(Arg("__arg0", tpePar("CR"), None), Arg("__arg1", MInt, None)), DenseVector(tpePar("R"))), implicitArgs = List()) implements composite("""{
  val simpleName: this.Rep[String] = manifest[CR].erasure.getSimpleName();
  val isRow: this.Rep[Boolean] = simpleName match {
    case (s @ _) if s.startsWith(unit("IndexVector")) => infix_asInstanceOf[IndexVector](__arg0).isRow()
    case (s @ _) if s.startsWith(unit("DenseVectorView")) => infix_asInstanceOf[DenseVectorView[Any]](__arg0).isRow()
    case (s @ _) if s.startsWith(unit("DenseVector")) => infix_asInstanceOf[DenseVector[Any]](__arg0).isRow()
  };
  DenseVector[R](__arg1, isRow)
}""")
    compiler (DenseVector) ("densevector_sortindex_helper", List(tpePar("T")), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None), Arg("__arg2", MArray(tpePar("T")), None)), MArray(MInt)), implicitArgs = List(TOrdering(tpePar("T")))) implements single("((__arg0.until(__arg1)): this.Rep[Range]).toArray[Int].sortWith(((a: this.Rep[Int], b: this.Rep[Int]) => __arg2.apply(a).$less(__arg2.apply(b))))")
    compiler (DenseVector) ("densevector_groupby_helper", List(tpePar("T"), tpePar("R")), MethodSignature(List(Arg("__arg0", MArray(tpePar("T")), None), Arg("__arg1", (tpePar("T")) ==> tpePar("R"), None)), MArray(MArray(tpePar("T")))), implicitArgs = List()) implements single("__arg0.groupBy[R](((e: this.Rep[T]) => __arg1.apply(e))).values.toArray[Array[T]]")
    */
    compiler (DenseVector) ("densevector_raw_data", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVector, None)), MArray(tpePar("T"))), implicitArgs = List()) implements getter(0, "_data")
    compiler (DenseVector) ("densevector_get_length", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVector, None)), MInt), implicitArgs = List()) implements getter(0, "_length")
    compiler (DenseVector) ("densevector_is_row", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVector, None)), MBoolean), implicitArgs = List()) implements getter(0, "_isRow")
    compiler (DenseVector) ("densevector_set_raw_data", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVector, None), Arg("__arg1", MArray(tpePar("T")), None)), MUnit), implicitArgs = List(), effect = write(0)) implements setter(0, "_data", "__arg1")
    compiler (DenseVector) ("densevector_set_length", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVector, None), Arg("__arg1", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements setter(0, "_length", "__arg1")
    compiler (DenseVector) ("densevector_set_isrow", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVector, None), Arg("__arg1", MBoolean, None)), MUnit), implicitArgs = List(), effect = write(0)) implements setter(0, "_isRow", "__arg1")
    compiler (DenseVector) ("densevector_insertspace", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVector, None), Arg("pos", MInt, None), Arg("len", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("""{
  densevector_ensureextra[T](self, len);
  val data: this.Rep[ForgeArray[T]] = densevector_raw_data[T](self);
  array_copy[T](data, pos, data, pos.$plus(len), self.length().$minus(pos));
  densevector_set_length[T](self, self.length().$plus(len))
}""")
    compiler (DenseVector) ("densevector_ensureextra", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVector, None), Arg("extra", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("""{
  val data: this.Rep[ForgeArray[T]] = densevector_raw_data[T](self);
  __ifThenElse(array_length[T](data).$minus(self.length()).$less(extra), densevector_realloc[T](self, self.length().$plus(extra)), unit(()))
}""")
    compiler (DenseVector) ("densevector_realloc", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVector, None), Arg("minLen", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("""{
  val data: this.Rep[ForgeArray[T]] = densevector_raw_data[T](self);
  var n: this.Rep[Int] = __newVar(max(unit(4), array_length[T](data).$times(unit(2))));
  __whileDo(n.$less(minLen), __assign(n, n.$times(unit(2))));
  val d: this.Rep[ForgeArray[T]] = array_empty[T](n);
  array_copy[T](data, unit(0), d, unit(0), self.length());
  densevector_set_raw_data[T](self, d.unsafeImmutable)
}""")
    compiler (DenseVector) ("densevector_appendable", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVector, None), Arg("__arg1", MInt, None), Arg("__arg2", tpePar("T"), None)), MBoolean), implicitArgs = List()) implements single("unit(true)")
    compiler (DenseVector) ("densevector_append", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVector, None), Arg("__arg1", MInt, None), Arg("__arg2", tpePar("T"), None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("self.insert(self.length(), __arg2)")
    compiler (DenseVector) ("densevector_copy", List(tpePar("T")), MethodSignature(List(Arg("self", DenseVector, None), Arg("__arg1", MInt, None), Arg("__arg2", DenseVector(tpePar("T")), None), Arg("__arg3", MInt, None), Arg("__arg4", MInt, None)), MUnit), implicitArgs = List(), effect = write(2)) implements single("""{
  val src: this.Rep[ForgeArray[T]] = densevector_raw_data[T](self);
  val dest: this.Rep[ForgeArray[T]] = densevector_raw_data[T](__arg2);
  array_copy[T](src, __arg1, dest, __arg3, __arg4)
}""")
    compiler (DenseVector) ("densevector_densevector_filter_map", List(tpePar("T"), tpePar("R")), MethodSignature(List(Arg("self", DenseVector, None), Arg("__arg1", (tpePar("T")) ==> MBoolean, None), Arg("__arg2", (tpePar("T")) ==> tpePar("R"), None)), DenseVector(tpePar("R"))), implicitArgs = List()) implements filter(scala.Tuple2(tpePar("T"), tpePar("R")), 0, "((e: this.Rep[T]) => __arg1.apply(e))", "((e: this.Rep[T]) => __arg2.apply(e))")
    static (DenseVector) ("apply", List(tpePar("T")), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MBoolean, None)), DenseVector(tpePar("T"))), implicitArgs = List(), effect = mutable) implements allocates(DenseVector, "__arg0", "__arg1", "array_empty[T](__arg0)")
    static (DenseVector) ("apply", List(tpePar("T")), MethodSignature(List(Arg("__arg0", varArgs(tpePar("T")), None)), DenseVector(tpePar("T"))), implicitArgs = List()) implements allocates(DenseVector, "unit(__arg0.length)", "unit(true)", "array_fromseq[T](__arg0)")
    static (DenseVector) ("zeros", List(), MethodSignature(List(Arg("__arg0", MInt, None)), DenseVector(MDouble))) implements composite("densevector_fromfunc[Double](__arg0, ((i: this.Rep[Int]) => unit(0.0)))")
    static (DenseVector) ("zerosf", List(), MethodSignature(List(Arg("__arg0", MInt, None)), DenseVector(MFloat))) implements composite("densevector_fromfunc[Float](__arg0, ((i: this.Rep[Int]) => unit(0.0F)))")
    static (DenseVector) ("ones", List(), MethodSignature(List(Arg("__arg0", MInt, None)), DenseVector(MDouble))) implements composite("densevector_fromfunc[Double](__arg0, ((i: this.Rep[Int]) => unit(1.0)))")
    static (DenseVector) ("onesf", List(), MethodSignature(List(Arg("__arg0", MInt, None)), DenseVector(MFloat))) implements composite("densevector_fromfunc[Float](__arg0, ((i: this.Rep[Int]) => unit(1.0F)))")
    static (DenseVector) ("rand", List(), MethodSignature(List(Arg("__arg0", MInt, None)), DenseVector(MDouble))) implements composite("densevector_fromfunc[Double](__arg0, ((i: this.Rep[Int]) => random[Double]))")
    static (DenseVector) ("randf", List(), MethodSignature(List(Arg("__arg0", MInt, None)), DenseVector(MFloat))) implements composite("densevector_fromfunc[Float](__arg0, ((i: this.Rep[Int]) => random[Float]))")
    static (DenseVector) ("uniform", List(), MethodSignature(List(Arg("start", MInt, None), Arg("step_size", MDouble, None), Arg("end", MInt, None), Arg("isRow", MBoolean, Some("true"))), DenseVector(MDouble))) implements composite("""{
  val length: this.Rep[Int] = ceil(end.$minus(start).$div(step_size));
  densevector_fromfunc[Double](length, ((i: this.Rep[Int]) => step_size.$times(i).$plus(start)))
}""")
    /*
    static (DenseVector) ("flatten", List(tpePar("T")), MethodSignature(List(Arg("pieces", DenseVector(DenseVector(tpePar("T"))), None)), DenseVector(tpePar("T"))), implicitArgs = List()) implements single("""__ifThenElse(infix_$eq$eq(pieces.length(), unit(0)), DenseVector[T](unit(0), pieces.isRow()).unsafeImmutable, {
  val sizes: this.Rep[DenseVector[Int]] = pieces.map[Int](((e: this.Rep[DenseVector[T]]) => e.length()));
  val x$4: this.Rep[Tuple2[Int, DenseVector[Int]]] = ((densevector_precumulate[Int](sizes, unit(0), ((x$2: this.Rep[Int], x$3: this.Rep[Int]) => ((x$2): this.Rep[Int]).$plus(((x$3): this.Rep[Int]))))): this.Rep[Tuple2[Int, DenseVector[Int]]]) match {
    case this.Rep[(_1: Int, _2: optiml.shallow.ops.DenseVector[Int])(Int, optiml.shallow.ops.DenseVector[Int])]((total @ _), (begins @ _)) => scala.Tuple2.apply(total, begins)
  };
  val total: this.Rep[Int] = x$4._1;
  val begins: this.Rep[DenseVector[Int]] = x$4._2;
  val result: this.Rep[DenseVector[T]] = DenseVector[T](total, pieces.isRow());
  unit(0).until(pieces.length()).foreach(((i: this.Rep[Int]) => result.copyFrom(begins.apply(i), pieces.apply(i))));
  result.unsafeImmutable
})""")
    */
    DenseVectorOps {
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
      infix ("apply") (MethodSignature(List(Arg("__arg0", MInt, None)), tpePar("T")), implicitArgs = List()) implements composite("array_apply[T](densevector_raw_data[T](self), __arg0)")
      infix ("apply") (MethodSignature(List(Arg("__arg0", IndexVector, None)), DenseVector(tpePar("T"))), implicitArgs = List()) implements composite("""{
  val out: this.Rep[DenseVector[T]] = __arg0.map[T](((i: this.Rep[Int]) => self.apply(i)));
  __ifThenElse(infix_$bang$eq(self.isRow(), __arg0.isRow()), out.t(), out)
}""")
      infix ("slice") (MethodSignature(List(Arg("start", MInt, None), Arg("end", MInt, None)), DenseVector(tpePar("T")))) implements single("""{
  val out: this.Rep[DenseVector[T]] = DenseVector[T](end.$minus(start), self.isRow());
  start.until(end).foreach(((i: this.Rep[Int]) => out.update(i.$minus(start), self.apply(i))));
  out.unsafeImmutable
}""")
      infix ("t") (MethodSignature(Nil, DenseVector(tpePar("T")))) implements allocates(DenseVector, "densevector_get_length[T](self)", "densevector_is_row[T](self).unary_$bang", "array_clone[T](densevector_raw_data[T](self))")
      infix ("mt") (MethodSignature(Nil, MUnit), effect = write(0)) implements composite("densevector_set_isrow[T](self, self.isRow().unary_$bang)")
      infix ("Clone") (MethodSignature(Nil, DenseVector(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e)")
      infix ("mutable") (MethodSignature(Nil, DenseVector(tpePar("T"))), effect = mutable) implements single("""{
  val out: this.Rep[DenseVector[T]] = DenseVector[T](self.length(), self.isRow());
  unit(0).until(out.length()).foreach(((i: this.Rep[Int]) => out.update(i, self.apply(i))));
  out
}""")
      infix ("replicate") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), DenseMatrix(tpePar("T")))) implements single("""__ifThenElse(self.isRow(), {
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](__arg0, __arg1.$times(self.length()));
  unit(0).until(__arg1.$times(self.length())).foreach(((col: this.Rep[Int]) => {
    val colToJ: this.Rep[Int] = col.$percent(self.length());
    unit(0).until(__arg0).foreach(((rI: this.Rep[Int]) => out.update(rI, col, self.apply(colToJ))))
  }));
  out.unsafeImmutable
}, {
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](__arg0.$times(self.length()), __arg1);
  unit(0).until(__arg0.$times(self.length())).foreach(((row: this.Rep[Int]) => {
    val rowToI: this.Rep[Int] = row.$percent(self.length());
    unit(0).until(__arg1).foreach(((cI: this.Rep[Int]) => out.update(row, cI, self.apply(rowToI))))
  }));
  out.unsafeImmutable
})""")
      infix ("update") (MethodSignature(List(Arg("i", MInt, None), Arg("e", tpePar("T"), None)), MUnit), effect = write(0)) implements composite("array_update[T](densevector_raw_data[T](self), i, e)")
      infix ("<<") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), DenseVector(tpePar("T"))), implicitArgs = List()) implements single("""{
  val out: this.Rep[DenseVector[T]] = DenseVector[T](unit(0), self.isRow());
  out.$less$less$eq(self);
  out.$less$less$eq(__arg0);
  out.unsafeImmutable
}""")
      infix ("<<") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), DenseVector(tpePar("T"))), implicitArgs = List()) implements single("""{
  val out: this.Rep[DenseVector[T]] = DenseVector[T](self.length().$plus(__arg0.length()), self.isRow());
  unit(0).until(self.length()).foreach(((i: this.Rep[Int]) => out.update(i, self.apply(i))));
  unit(0).until(__arg0.length()).foreach(((i: this.Rep[Int]) => out.update(i.$plus(self.length()), __arg0.apply(i))));
  out.unsafeImmutable
}""")
      infix ("<<=") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), MUnit), implicitArgs = List(), effect = write(0)) implements composite("self.insert(self.length(), __arg0)")
      infix ("<<=") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), MUnit), implicitArgs = List(), effect = write(0)) implements composite("self.insertAll(self.length(), __arg0)")
      infix ("insert") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", tpePar("T"), None)), MUnit), effect = write(0)) implements single("""{
  densevector_insertspace[T](self, __arg0, unit(1));
  self.update(__arg0, __arg1)
}""")
      infix ("insertAll") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", DenseVector(tpePar("T")), None)), MUnit), effect = write(0)) implements single("""{
  densevector_insertspace[T](self, __arg0, __arg1.length());
  self.copyFrom(__arg0, __arg1)
}""")
      infix ("remove") (MethodSignature(List(Arg("__arg0", MInt, None)), MUnit), effect = write(0)) implements composite("self.removeAll(__arg0, unit(1))")
      infix ("removeAll") (MethodSignature(List(Arg("pos", MInt, None), Arg("len", MInt, None)), MUnit), effect = write(0)) implements single("""{
  val data: this.Rep[ForgeArray[T]] = densevector_raw_data[T](self);
  array_copy[T](data, pos.$plus(len), data, pos, self.length().$minus(pos.$plus(len)));
  densevector_set_length[T](self, self.length().$minus(len))
}""")
      infix ("copyFrom") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", DenseVector(tpePar("T")), None)), MUnit), effect = write(0)) implements single("""{
  val d: this.Rep[ForgeArray[T]] = densevector_raw_data[T](self);
  unit(0).until(__arg1.length()).foreach(((i: this.Rep[Int]) => array_update[T](d, __arg0.$plus(i), __arg1.apply(i))))
}""")
      infix ("trim") (MethodSignature(Nil, MUnit), effect = write(0)) implements single("""{
  val data: this.Rep[ForgeArray[T]] = densevector_raw_data[T](self);
  __ifThenElse(self.length().$less(array_length[T](data)), {
    val d: this.Rep[ForgeArray[T]] = array_empty[T](self.length());
    array_copy[T](data, unit(0), d, unit(0), self.length());
    densevector_set_raw_data[T](self, d.unsafeImmutable)
  }, unit(()))
}""")
      infix ("clear") (MethodSignature(Nil, MUnit), effect = write(0)) implements single("""{
  densevector_set_length[T](self, unit(0));
  densevector_set_raw_data[T](self, array_empty[T](unit(0)).unsafeImmutable)
}""")
      infix ("+=") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("self.indices().foreach(((i: this.Rep[Int]) => self.update(i, self.apply(i).$plus(__arg0.apply(i)))))")
      infix ("+=") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("self.indices().foreach(((i: this.Rep[Int]) => self.update(i, self.apply(i).$plus(__arg0))))")
      infix ("+=") (MethodSignature(List(Arg("__arg0", DenseVectorView(tpePar("T")), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("self.indices().foreach(((i: this.Rep[Int]) => self.update(i, self.apply(i).$plus(__arg0.apply(i)))))")
      infix ("*=") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("self.indices().foreach(((i: this.Rep[Int]) => self.update(i, self.apply(i).$times(__arg0.apply(i)))))")
      infix ("*=") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("self.indices().foreach(((i: this.Rep[Int]) => self.update(i, self.apply(i).$times(__arg0))))")
      infix ("*=") (MethodSignature(List(Arg("__arg0", DenseVectorView(tpePar("T")), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("self.indices().foreach(((i: this.Rep[Int]) => self.update(i, self.apply(i).$times(__arg0.apply(i)))))")
      infix ("-=") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("self.indices().foreach(((i: this.Rep[Int]) => self.update(i, self.apply(i).$minus(__arg0.apply(i)))))")
      infix ("-=") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("self.indices().foreach(((i: this.Rep[Int]) => self.update(i, self.apply(i).$minus(__arg0))))")
      infix ("-=") (MethodSignature(List(Arg("__arg0", DenseVectorView(tpePar("T")), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("self.indices().foreach(((i: this.Rep[Int]) => self.update(i, self.apply(i).$minus(__arg0.apply(i)))))")
      infix ("/=") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("self.indices().foreach(((i: this.Rep[Int]) => self.update(i, self.apply(i).$div(__arg0.apply(i)))))")
      infix ("/=") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("self.indices().foreach(((i: this.Rep[Int]) => self.update(i, self.apply(i).$div(__arg0))))")
      infix ("/=") (MethodSignature(List(Arg("__arg0", DenseVectorView(tpePar("T")), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("self.indices().foreach(((i: this.Rep[Int]) => self.update(i, self.apply(i).$div(__arg0.apply(i)))))")
      infix ("sort") (MethodSignature(Nil, DenseVector(tpePar("T"))), implicitArgs = List(TOrdering(tpePar("T")))) implements composite("""{
  val v2: this.Rep[DenseVector[T]] = self.mutable();
  v2.trim();
  val a: this.Rep[ForgeArray[T]] = array_sort[T](densevector_raw_data[T](v2));
  densevector_fromarray[T](a, self.isRow())
}""")
      infix ("sortWithIndex") (MethodSignature(Nil, lookupTpe("Tup2")(DenseVector(tpePar("T")), IndexVector)), implicitArgs = List(TOrdering(tpePar("T")))) implements composite("""{
  val sortedIndicesRaw: this.Rep[ForgeArray[Int]] = densevector_sortindex_helper[T](unit(0), self.length(), densevector_raw_data[T](self));
  val sortedIndices: this.Rep[IndexVector] = IndexVector(densevector_fromarray[Int](sortedIndicesRaw, self.isRow()));
  scala.Tuple2.apply(self.apply(sortedIndices), sortedIndices)
}""")
      infix ("median") (MethodSignature(Nil, tpePar("T")), implicitArgs = List(TNumeric(tpePar("T")), TOrdering(tpePar("T")))) implements single("""{
  val x: this.Rep[DenseVector[T]] = self.sort();
  val mid: this.Rep[Int] = x.length().$div(unit(2));
  __ifThenElse(infix_$eq$eq(x.length().$percent(unit(2)), unit(0)), infix_asInstanceOf[T](infix_asInstanceOf[Double](x.apply(mid)).$plus(infix_asInstanceOf[Double](x.apply(mid.$minus(unit(1))))).$div(unit(2))), x.apply(mid))
}""")
      infix (":>") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), DenseVector(MBoolean)), implicitArgs = List(TOrdering(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), MBoolean), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$greater(b))")
      infix (":<") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), DenseVector(MBoolean)), implicitArgs = List(TOrdering(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), MBoolean), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$less(b))")
      infix ("groupBy") (MethodSignature(List(Arg("__arg0", (tpePar("T")) ==> tpePar("R"), None)), DenseVector(DenseVector(tpePar("T")))), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements composite("""{
  val a: this.Rep[ForgeArray[ForgeArray[T]]] = densevector_groupby_helper[T, R](densevector_raw_data[T](self), __arg0);
  {
    val x$5: this.Rep[Int] = unit(0);
    array_length[ForgeArray[T]](a).$colon$colon(x$5).apply[DenseVector[T]](((i: this.Rep[Int]) => densevector_fromarray[T](array_apply[ForgeArray[T]](a, i), self.isRow())))
  }
}""")
      infix ("drop") (MethodSignature(List(Arg("__arg0", MInt, None)), DenseVector(tpePar("T")))) implements composite("self.slice(__arg0, self.length())")
      infix ("take") (MethodSignature(List(Arg("__arg0", MInt, None)), DenseVector(tpePar("T")))) implements composite("self.slice(unit(0), __arg0)")
      infix ("count") (MethodSignature(List(Arg("__arg0", (tpePar("T")) ==> MBoolean, None)), MInt)) implements composite("densevector_densevector_filter_map[T, Int](self, __arg0, ((e: this.Rep[T]) => unit(1))).sum()")
    }
  }
}
