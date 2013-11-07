package ppl.dsl.forge
package examples
package autooptila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait IndexVectorOps {
  this: AutoOptiLADSL with ppl.dsl.forge.core.ForgeOpsExp =>

  def importIndexVectorOps() {
    val DenseVectorView = lookupTpe("DenseVectorView")
    val IndexVector = lookupTpe("IndexVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseVector = lookupTpe("DenseVector")

    data(IndexVector, ("_data", MArray(MInt)), ("_start", MInt), ("_end", MInt), ("_isRow", MBoolean), ("_isRange", MBoolean))

    val IndexVectorOps = withTpe (IndexVector)

    compiler (IndexVector) ("indexvector_fromarray", List(), MethodSignature(List(Arg("__arg0", MArray(MInt), None), Arg("__arg1", MBoolean, None)), IndexVector)) implements allocates(IndexVector, "__arg0", "unit(0)", "unit(0)", "__arg1", "unit(false)")
    compiler (IndexVector) ("indexvector_copyarray", List(), MethodSignature(List(Arg("__arg0", DenseVector(MInt), None)), MArray(MInt))) implements composite("""{
  val d: this.Rep[ForgeArray[Int]] = array_empty[Int](__arg0.length());
  __arg0.indices().foreach(((i: this.Rep[Int]) => d.update(i, __arg0.apply(i))));
  d.unsafeImmutable
}""")
    compiler (IndexVector) ("indexvector_start", List(), MethodSignature(List(Arg("self", IndexVector, None)), MInt)) implements getter(0, "_start")
    compiler (IndexVector) ("indexvector_end", List(), MethodSignature(List(Arg("self", IndexVector, None)), MInt)) implements getter(0, "_end")
    compiler (IndexVector) ("indexvector_raw_data", List(), MethodSignature(List(Arg("self", IndexVector, None)), MArray(MInt))) implements getter(0, "_data")
    compiler (IndexVector) ("indexvector_is_range", List(), MethodSignature(List(Arg("self", IndexVector, None)), MBoolean)) implements getter(0, "_isRange")
    compiler (IndexVector) ("indexvector_is_row", List(), MethodSignature(List(Arg("self", IndexVector, None)), MBoolean)) implements getter(0, "_isRow")
    compiler (IndexVector) ("indexvector_illegalalloc", List(), MethodSignature(List(Arg("self", IndexVector, None), Arg("__arg1", MInt, None)), MNothing)) implements composite("fatal(unit(\"IndexVectors cannot be allocated from a parallel op\"))")
    compiler (IndexVector) ("indexvector_illegalupdate", List(), MethodSignature(List(Arg("self", IndexVector, None), Arg("__arg1", MInt, None), Arg("__arg2", MInt, None)), MNothing)) implements composite("fatal(unit(\"IndexVectors cannot be updated\"))")
    compiler (IndexVector) ("zeroT", List(), MethodSignature(Nil, MInt)) implements composite("infix_asInstanceOf[Int](unit(0))")
    compiler (IndexVector) ("indexvector_densevector_filter_map", List(tpePar("R")), MethodSignature(List(Arg("self", IndexVector, None), Arg("__arg1", (MInt) ==> MBoolean, None), Arg("__arg2", (MInt) ==> tpePar("R"), None)), DenseVector(tpePar("R"))), implicitArgs = List()) implements filter(scala.Tuple2(MInt, tpePar("R")), 0, "((e: this.Rep[Int]) => __arg1.apply(e))", "((e: this.Rep[Int]) => __arg2.apply(e))")
    static (IndexVector) ("apply", List(), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), IndexVector), implicitArgs = List()) implements single("IndexVector(__arg0, __arg1, unit(true))")
    static (IndexVector) ("apply", List(), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None), Arg("__arg2", MBoolean, None)), IndexVector), implicitArgs = List()) implements allocates(IndexVector, "array_empty_imm[Int](unit(0))", "__arg0", "__arg1", "__arg2", "unit(true)")
    static (IndexVector) ("apply", List(), MethodSignature(List(Arg("__arg0", DenseVector(MInt), None)), IndexVector), implicitArgs = List()) implements single("IndexVector(__arg0, __arg0.isRow())")
    static (IndexVector) ("apply", List(), MethodSignature(List(Arg("__arg0", DenseVector(MInt), None), Arg("__arg1", MBoolean, None)), IndexVector), implicitArgs = List()) implements allocates(IndexVector, "indexvector_copyarray(__arg0)", "unit(0)", "unit(0)", "__arg1", "unit(false)")
    /*static (IndexVector) ("apply", List(tpePar("T")), MethodSignature(List(Arg("__arg0", lookupTpe("Tup2")(IndexVector, IndexVector), None), Arg("__arg1", (MInt,MInt) ==> tpePar("T"), None)), DenseMatrix(tpePar("T"))), implicitArgs = List()) implements single("""{
  val x$1: this.Rep[Tuple2[IndexVector, IndexVector]] = ((__arg0): this.Rep[Tuple2[IndexVector, IndexVector]]) match {
    case this.Rep[(_1: optiml.shallow.ops.IndexVector, _2: optiml.shallow.ops.IndexVector)(optiml.shallow.ops.IndexVector, optiml.shallow.ops.IndexVector)]((rowIndices @ _), (colIndices @ _)) => scala.Tuple2.apply(rowIndices, colIndices)
  };
  val rowIndices: this.Rep[IndexVector] = x$1._1;
  val colIndices: this.Rep[IndexVector] = x$1._2;
  val v: this.Rep[DenseVector[Int]] = ({
  val x$2: this.Rep[Int] = unit(0);
  rowIndices.length().$times(colIndices.length()).$colon$colon(x$2)
}).toDense();
  val indices: this.Rep[DenseMatrix[Int]] = densematrix_fromarray[Int](densevector_raw_data[Int](v), rowIndices.length(), colIndices.length());
  indices.map[T](((i: this.Rep[Int]) => {
    val rowIndex: this.Rep[Int] = i.$div(colIndices.length());
    val colIndex: this.Rep[Int] = i.$percent(colIndices.length());
    __arg1.apply(rowIndex, colIndex)
  }))
}""")*/
    IndexVectorOps {
      infix ("length") (MethodSignature(Nil, MInt)) implements composite("__ifThenElse(indexvector_is_range(self), indexvector_end(self).$minus(indexvector_start(self)), array_length[Int](indexvector_raw_data(self)))")
      infix ("isRow") (MethodSignature(Nil, MBoolean)) implements getter(0, "_isRow")
      infix ("apply") (MethodSignature(List(Arg("__arg0", MInt, None)), MInt), implicitArgs = List()) implements composite("__ifThenElse(indexvector_is_range(self), indexvector_start(self).$plus(__arg0), indexvector_raw_data(self).apply(__arg0))")
      infix ("slice") (MethodSignature(List(Arg("start", MInt, None), Arg("end", MInt, None)), IndexVector)) implements composite("__ifThenElse(indexvector_is_range(self), IndexVector(start, end, self.isRow()), IndexVector(densevector_fromarray[Int](indexvector_raw_data(self), self.isRow()).slice(start, end)))")
      infix ("t") (MethodSignature(Nil, IndexVector)) implements allocates(IndexVector, "indexvector_raw_data(self)", "indexvector_start(self)", "indexvector_end(self)", "indexvector_is_row(self).unary_$bang", "indexvector_is_range(self)")
      infix ("toDense") (MethodSignature(Nil, DenseVector(MInt))) implements composite("self.map[Int](((e: this.Rep[Int]) => e))")
      infix ("toBoolean") (MethodSignature(Nil, DenseVector(MBoolean)), implicitArgs = List(("conv", (MInt) ==> MBoolean))) implements map(scala.Tuple2(MInt, MBoolean), 0, "conv")
      infix ("toDouble") (MethodSignature(Nil, DenseVector(MDouble)), implicitArgs = List(("conv", (MInt) ==> MDouble))) implements map(scala.Tuple2(MInt, MDouble), 0, "conv")
      infix ("toFloat") (MethodSignature(Nil, DenseVector(MFloat)), implicitArgs = List(("conv", (MInt) ==> MFloat))) implements map(scala.Tuple2(MInt, MFloat), 0, "conv")
      infix ("toInt") (MethodSignature(Nil, DenseVector(MInt)), implicitArgs = List(("conv", (MInt) ==> MInt))) implements map(scala.Tuple2(MInt, MInt), 0, "conv")
      infix ("indices") (MethodSignature(Nil, IndexVector)) implements composite("IndexVector(unit(0), self.length(), self.isRow())")
      infix ("isEmpty") (MethodSignature(Nil, MBoolean)) implements single("infix_$eq$eq(self.length(), unit(0))")
      infix ("first") (MethodSignature(Nil, MInt)) implements single("self.apply(unit(0))")
      infix ("last") (MethodSignature(Nil, MInt)) implements single("self.apply(self.length().$minus(unit(1)))")
      infix ("drop") (MethodSignature(List(Arg("__arg0", MInt, None)), IndexVector)) implements composite("self.slice(__arg0, self.length())")
      infix ("take") (MethodSignature(List(Arg("__arg0", MInt, None)), IndexVector)) implements composite("self.slice(unit(0), __arg0)")
      infix ("contains") (MethodSignature(List(Arg("__arg0", MInt, None)), MBoolean)) implements single("""{
  var found: this.Rep[Boolean] = __newVar(unit(false));
  var i: this.Rep[Int] = __newVar(unit(0));
  __whileDo(i.$less(self.length()).$amp$amp(found.unary_$bang), {
    __ifThenElse(infix_$eq$eq(self.apply(i), __arg0), __assign(found, unit(true)), unit(()));
    __assign(i, i.$plus(unit(1)))
  });
  found
}""")
      infix ("distinct") (MethodSignature(Nil, DenseVector(MInt))) implements single("""{
  val out: this.Rep[DenseVector[Int]] = DenseVector[Int](unit(0), self.isRow());
  unit(0).until(self.length()).foreach(((i: this.Rep[Int]) => __ifThenElse(out.contains(self.apply(i)).unary_$bang, out.$less$less$eq(self.apply(i)), unit(()))));
  out.unsafeImmutable
}""")
      infix ("makeString") (MethodSignature(Nil, tpePar("String"))) implements single("""{
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
      infix ("pprint") (MethodSignature(Nil, MUnit), effect = simple) implements composite("println(self.makeStr())")
      infix ("+") (MethodSignature(List(Arg("__arg0", DenseVector(MInt), None)), DenseVector(MInt)), implicitArgs = List()) implements zip(scala.Tuple3(MInt, MInt, MInt), scala.Tuple2(0, 1), "((a: this.Rep[Int], b: this.Rep[Int]) => a.$plus(b))")
      infix ("-") (MethodSignature(List(Arg("__arg0", DenseVector(MInt), None)), DenseVector(MInt)), implicitArgs = List()) implements zip(scala.Tuple3(MInt, MInt, MInt), scala.Tuple2(0, 1), "((a: this.Rep[Int], b: this.Rep[Int]) => a.$minus(b))")
      infix ("*") (MethodSignature(List(Arg("__arg0", DenseVector(MInt), None)), DenseVector(MInt)), implicitArgs = List()) implements zip(scala.Tuple3(MInt, MInt, MInt), scala.Tuple2(0, 1), "((a: this.Rep[Int], b: this.Rep[Int]) => a.$times(b))")
      infix ("*:*") (MethodSignature(List(Arg("__arg0", DenseVector(MInt), None)), MInt), implicitArgs = List()) implements composite("""{
  __ifThenElse(infix_$bang$eq(self.length(), __arg0.length()), fatal(unit("dimension mismatch: vector dot product")), unit(()));
  self.$times(__arg0).sum()
}""")
      infix ("**") (MethodSignature(List(Arg("__arg0", DenseVector(MInt), None)), DenseMatrix(MInt)), implicitArgs = List()) implements composite("""{
  __ifThenElse(self.isRow().$bar$bar(__arg0.isRow().unary_$bang), fatal(unit("dimension mismatch: vector outer product")), unit(()));
  val out: this.Rep[DenseMatrix[Int]] = DenseMatrix[Int](self.length(), __arg0.length());
  unit(0).until(self.length()).foreach(((i: this.Rep[Int]) => unit(0).until(__arg0.length()).foreach(((j: this.Rep[Int]) => out.update(i, j, self.apply(i).$times(__arg0.apply(j)))))));
  out.unsafeImmutable
}""")
      infix ("/") (MethodSignature(List(Arg("__arg0", DenseVector(MInt), None)), DenseVector(MInt)), implicitArgs = List()) implements zip(scala.Tuple3(MInt, MInt, MInt), scala.Tuple2(0, 1), "((a: this.Rep[Int], b: this.Rep[Int]) => a.$div(b))")
      infix ("+") (MethodSignature(List(Arg("__arg0", DenseVectorView(MInt), None)), DenseVector(MInt)), implicitArgs = List()) implements zip(scala.Tuple3(MInt, MInt, MInt), scala.Tuple2(0, 1), "((a: this.Rep[Int], b: this.Rep[Int]) => a.$plus(b))")
      infix ("-") (MethodSignature(List(Arg("__arg0", DenseVectorView(MInt), None)), DenseVector(MInt)), implicitArgs = List()) implements zip(scala.Tuple3(MInt, MInt, MInt), scala.Tuple2(0, 1), "((a: this.Rep[Int], b: this.Rep[Int]) => a.$minus(b))")
      infix ("*") (MethodSignature(List(Arg("__arg0", DenseVectorView(MInt), None)), DenseVector(MInt)), implicitArgs = List()) implements zip(scala.Tuple3(MInt, MInt, MInt), scala.Tuple2(0, 1), "((a: this.Rep[Int], b: this.Rep[Int]) => a.$times(b))")
      infix ("*:*") (MethodSignature(List(Arg("__arg0", DenseVectorView(MInt), None)), MInt), implicitArgs = List()) implements composite("""{
  __ifThenElse(infix_$bang$eq(self.length(), __arg0.length()), fatal(unit("dimension mismatch: vector dot product")), unit(()));
  self.$times(__arg0).sum()
}""")
      infix ("**") (MethodSignature(List(Arg("__arg0", DenseVectorView(MInt), None)), DenseMatrix(MInt)), implicitArgs = List()) implements composite("""{
  __ifThenElse(self.isRow().$bar$bar(__arg0.isRow().unary_$bang), fatal(unit("dimension mismatch: vector outer product")), unit(()));
  val out: this.Rep[DenseMatrix[Int]] = DenseMatrix[Int](self.length(), __arg0.length());
  unit(0).until(self.length()).foreach(((i: this.Rep[Int]) => unit(0).until(__arg0.length()).foreach(((j: this.Rep[Int]) => out.update(i, j, self.apply(i).$times(__arg0.apply(j)))))));
  out.unsafeImmutable
}""")
      infix ("/") (MethodSignature(List(Arg("__arg0", DenseVectorView(MInt), None)), DenseVector(MInt)), implicitArgs = List()) implements zip(scala.Tuple3(MInt, MInt, MInt), scala.Tuple2(0, 1), "((a: this.Rep[Int], b: this.Rep[Int]) => a.$div(b))")
      infix ("zip") (CurriedMethodSignature(List(List(Arg("__arg0", DenseVector(tpePar("B")), None)), List(Arg("__arg1", (MInt,tpePar("B")) ==> tpePar("R"), None))), DenseVector(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("B"), tpePar("R"))) implements zip(scala.Tuple3(MInt, tpePar("B"), tpePar("R")), scala.Tuple2(0, 1), "((a: this.Rep[Int], b: this.Rep[B]) => __arg1.apply(a, b))")
      infix ("zip") (CurriedMethodSignature(List(List(Arg("__arg0", DenseVectorView(tpePar("B")), None)), List(Arg("__arg1", (MInt,tpePar("B")) ==> tpePar("R"), None))), DenseVector(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("B"), tpePar("R"))) implements zip(scala.Tuple3(MInt, tpePar("B"), tpePar("R")), scala.Tuple2(0, 1), "((a: this.Rep[Int], b: this.Rep[B]) => __arg1.apply(a, b))")
      infix ("+") (MethodSignature(List(Arg("__arg0", MInt, None)), DenseVector(MInt)), implicitArgs = List()) implements map(scala.Tuple2(MInt, MInt), 0, "((e: this.Rep[Int]) => e.$plus(__arg0))")
      infix ("-") (MethodSignature(List(Arg("__arg0", MInt, None)), DenseVector(MInt)), implicitArgs = List()) implements map(scala.Tuple2(MInt, MInt), 0, "((e: this.Rep[Int]) => e.$minus(__arg0))")
      infix ("*") (MethodSignature(List(Arg("__arg0", MInt, None)), DenseVector(MInt)), implicitArgs = List()) implements map(scala.Tuple2(MInt, MInt), 0, "((e: this.Rep[Int]) => e.$times(__arg0))")
      infix ("*") (MethodSignature(List(Arg("__arg0", DenseMatrix(MInt), None)), DenseVector(MInt)), implicitArgs = List()) implements composite("""{
  __ifThenElse(self.isRow().unary_$bang, fatal(unit("dimension mismatch: vector * matrix")), unit(()));
  __arg0.t().mapRowsToVector[Int](((row: this.Rep[DenseVectorView[Int]]) => self.$times$colon$times(row)))
}""")
      infix ("/") (MethodSignature(List(Arg("__arg0", MInt, None)), DenseVector(MInt)), implicitArgs = List()) implements map(scala.Tuple2(MInt, MInt), 0, "((e: this.Rep[Int]) => e.$div(__arg0))")
      infix ("abs") (MethodSignature(Nil, DenseVector(MInt))) implements map(scala.Tuple2(MInt, MInt), 0, "((e: this.Rep[Int]) => e.abs)")
      infix ("exp") (MethodSignature(Nil, DenseVector(MInt))) implements map(scala.Tuple2(MInt, MInt), 0, "((e: this.Rep[Int]) => e.exp())")
      infix ("log") (MethodSignature(Nil, DenseVector(MInt))) implements map(scala.Tuple2(MInt, MInt), 0, "((e: this.Rep[Int]) => e.log())")
      infix ("sum") (MethodSignature(Nil, MInt)) implements reduce(MInt, 0, "zeroT()", "((a: this.Rep[Int], b: this.Rep[Int]) => a.$plus(b))")
      infix ("mean") (MethodSignature(Nil, MDouble), implicitArgs = List(("conv", (MInt) ==> MDouble))) implements composite("self.map[Double](conv).sum().$div(self.length())")
      infix ("min") (MethodSignature(Nil, MInt)) implements reduce(MInt, 0, "self.apply(unit(0))", "((a: this.Rep[Int], b: this.Rep[Int]) => __ifThenElse(a.$less(b), a, b))")
      infix ("max") (MethodSignature(Nil, MInt)) implements reduce(MInt, 0, "self.apply(unit(0))", "((a: this.Rep[Int], b: this.Rep[Int]) => __ifThenElse(a.$greater(b), a, b))")
      infix ("minIndex") (MethodSignature(Nil, MInt)) implements single("""{
  var min: this.Rep[Int] = __newVar(self.apply(unit(0)));
  var minIndex: this.Rep[Int] = __newVar(unit(0));
  unit(0).until(self.length()).foreach(((i: this.Rep[Int]) => __ifThenElse(self.apply(i).$less(min), {
    __assign(min, self.apply(i));
    __assign(minIndex, i)
  }, unit(()))));
  minIndex
}""")
      infix ("maxIndex") (MethodSignature(Nil, MInt)) implements single("""{
  var max: this.Rep[Int] = __newVar(self.apply(unit(0)));
  var maxIndex: this.Rep[Int] = __newVar(unit(0));
  unit(0).until(self.length()).foreach(((i: this.Rep[Int]) => __ifThenElse(self.apply(i).$greater(max), {
    __assign(max, self.apply(i));
    __assign(maxIndex, i)
  }, unit(()))));
  maxIndex
}""")
      infix ("map") (MethodSignature(List(Arg("__arg0", (MInt) ==> tpePar("R"), None)), DenseVector(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements map(scala.Tuple2(MInt, tpePar("R")), 0, "((e: this.Rep[Int]) => __arg0.apply(e))")
      infix ("reduce") (MethodSignature(List(Arg("__arg0", (MInt,MInt) ==> MInt, None)), MInt)) implements reduce(MInt, 0, "zeroT()", "((a: this.Rep[Int], b: this.Rep[Int]) => __arg0.apply(a, b))")
      infix ("filter") (MethodSignature(List(Arg("__arg0", (MInt) ==> MBoolean, None)), DenseVector(MInt))) implements filter(scala.Tuple2(MInt, MInt), 0, "((e: this.Rep[Int]) => __arg0.apply(e))", "((e: this.Rep[Int]) => e)")
      infix ("foreach") (MethodSignature(List(Arg("__arg0", (MInt) ==> MUnit, None)), MUnit)) implements foreach(MInt, 0, "((e: this.Rep[Int]) => __arg0.apply(e))")
      infix ("find") (MethodSignature(List(Arg("__arg0", (MInt) ==> MBoolean, None)), IndexVector)) implements composite("IndexVector(self.indices().filter(((i: this.Rep[Int]) => __arg0.apply(self.apply(i)))))")
      infix ("count") (MethodSignature(List(Arg("__arg0", (MInt) ==> MBoolean, None)), MInt)) implements composite("indexvector_densevector_filter_map[Int](self, __arg0, ((e: this.Rep[Int]) => unit(1))).sum()")
      infix ("partition") (MethodSignature(List(Arg("pred", (MInt) ==> MBoolean, None)), lookupTpe("Tup2")(DenseVector(MInt), DenseVector(MInt)))) implements single("""{
  val outT: this.Rep[DenseVector[Int]] = DenseVector[Int](unit(0), self.isRow());
  val outF: this.Rep[DenseVector[Int]] = DenseVector[Int](unit(0), self.isRow());
  unit(0).until(self.length()).foreach(((i: this.Rep[Int]) => {
    val x: this.Rep[Int] = self.apply(i);
    __ifThenElse(pred.apply(x), outT.$less$less$eq(x), outF.$less$less$eq(x))
  }));
  scala.Tuple2.apply(outT.unsafeImmutable, outF.unsafeImmutable)
}""")
      infix ("flatMap") (MethodSignature(List(Arg("__arg0", (MInt) ==> DenseVector(tpePar("R")), None)), DenseVector(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements composite("DenseVector.flatten[R](self.map[DenseVector[R]](__arg0))")
      infix ("scan") (CurriedMethodSignature(List(List(Arg("zero", tpePar("R"), None)), List(Arg("__arg1", (tpePar("R"),MInt) ==> tpePar("R"), None))), DenseVector(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements single("""{
  val out: this.Rep[DenseVector[R]] = DenseVector[R](self.length(), self.isRow());
  out.update(unit(0), __arg1.apply(zero, self.apply(unit(0))));
  var i: this.Rep[Int] = __newVar(unit(1));
  __whileDo(i.$less(self.length()), {
    out.update(i, __arg1.apply(out.apply(i.$minus(unit(1))), self.apply(i)));
    __assign(i, i.$plus(unit(1)))
  });
  out.unsafeImmutable
}""")
      infix ("prefixSum") (MethodSignature(Nil, DenseVector(MInt))) implements composite("self.scan[Int](zeroT())(((a: this.Rep[Int], b: this.Rep[Int]) => a.$plus(b)))")
      infix ("apply") (MethodSignature(List(Arg("__arg1", (MInt) ==> tpePar("T"), None)), DenseVector(tpePar("T"))), implicitArgs = List(), addTpePars = List(tpePar("T"))) implements composite("self.map[T](__arg1)")
    }
  }
}
