package ppl.dsl.forge
package examples
package autooptila

import core.{ForgeApplication,ForgeApplicationRunner}

trait DenseMatrixOps {
  this: AutoOptiLADSL with ppl.dsl.forge.core.ForgeOpsExp =>

  def importDenseMatrixOps() {
    val DenseVectorView = lookupTpe("DenseVectorView")
    val IndexVector = lookupTpe("IndexVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseVector = lookupTpe("DenseVector")

    data(DenseMatrix, ("_numRows", MInt), ("_numCols", MInt), ("_data", MArray(tpePar("T"))))

    val DenseMatrixOps = withTpe (DenseMatrix)

    compiler (DenseMatrix) ("densematrix_fromarray", List(tpePar("T")), MethodSignature(List(Arg("__arg0", MArray(tpePar("T")), None), Arg("__arg1", MInt, None), Arg("__arg2", MInt, None)), DenseMatrix(tpePar("T"))), implicitArgs = List()) implements allocates(DenseMatrix, "__arg1", "__arg2", "__arg0")
    compiler (DenseMatrix) ("densematrix_fromfunc", List(tpePar("T")), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None), Arg("__arg2", (MInt,MInt) ==> tpePar("T"), None)), DenseMatrix(tpePar("T"))), implicitArgs = List()) implements composite("""scala.Tuple2.apply({
  val x$5: this.Rep[Int] = unit(0);
  __arg0.$colon$colon(x$5)
}, {
  val x$6: this.Rep[Int] = unit(0);
  __arg1.$colon$colon(x$6)
}).apply[T](((i: this.Rep[Int], j: this.Rep[Int]) => __arg2.apply(i, j)))""")
    compiler (DenseMatrix) ("densematrix_index", List(tpePar("T")), MethodSignature(List(Arg("self", DenseMatrix, None), Arg("__arg1", MInt, None), Arg("__arg2", MInt, None)), MInt), implicitArgs = List()) implements composite("__arg1.$times(self.numCols()).$plus(__arg2)")
    compiler (DenseMatrix) ("densematrix_raw_data", List(tpePar("T")), MethodSignature(List(Arg("self", DenseMatrix, None)), MArray(tpePar("T"))), implicitArgs = List()) implements getter(0, "_data")
    compiler (DenseMatrix) ("densematrix_set_raw_data", List(tpePar("T")), MethodSignature(List(Arg("self", DenseMatrix, None), Arg("__arg1", MArray(tpePar("T")), None)), MUnit), implicitArgs = List(), effect = write(0)) implements setter(0, "_data", "__arg1")
    compiler (DenseMatrix) ("densematrix_set_numrows", List(tpePar("T")), MethodSignature(List(Arg("self", DenseMatrix, None), Arg("__arg1", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements setter(0, "_numRows", "__arg1")
    compiler (DenseMatrix) ("densematrix_set_numcols", List(tpePar("T")), MethodSignature(List(Arg("self", DenseMatrix, None), Arg("__arg1", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements setter(0, "_numCols", "__arg1")
    compiler (DenseMatrix) ("densematrix_insertspace", List(tpePar("T")), MethodSignature(List(Arg("self", DenseMatrix, None), Arg("pos", MInt, None), Arg("len", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("""{
  __ifThenElse(pos.$less(unit(0)).$bar$bar(pos.$greater(self.size())), fatal(unit("DenseMatrix IndexOutOfBounds")), unit(()));
  densematrix_ensureextra[T](self, len);
  val d: this.Rep[ForgeArray[T]] = densematrix_raw_data[T](self);
  array_copy[T](d, pos, d, pos.$plus(len), self.size().$minus(pos))
}""")
    compiler (DenseMatrix) ("densematrix_ensureextra", List(tpePar("T")), MethodSignature(List(Arg("self", DenseMatrix, None), Arg("extra", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("""{
  val data: this.Rep[ForgeArray[T]] = densematrix_raw_data[T](self);
  __ifThenElse(array_length[T](data).$minus(self.size()).$less(extra), densematrix_realloc[T](self, self.size().$plus(extra)), unit(()))
}""")
    compiler (DenseMatrix) ("densematrix_realloc", List(tpePar("T")), MethodSignature(List(Arg("self", DenseMatrix, None), Arg("minLen", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("""{
  val data: this.Rep[ForgeArray[T]] = densematrix_raw_data[T](self);
  var n: this.Rep[Int] = __newVar(max(unit(4), array_length[T](data).$times(unit(2))));
  __whileDo(n.$less(minLen), __assign(n, n.$times(unit(2))));
  val d: this.Rep[ForgeArray[T]] = array_empty[T](n);
  array_copy[T](data, unit(0), d, unit(0), self.size());
  densematrix_set_raw_data[T](self, d.unsafeImmutable)
}""")
    compiler (DenseMatrix) ("densematrix_raw_alloc", List(tpePar("T"), tpePar("R")), MethodSignature(List(Arg("self", DenseMatrix, None), Arg("__arg1", MInt, None)), DenseMatrix(tpePar("R"))), implicitArgs = List()) implements composite("DenseMatrix[R](self.numRows(), self.numCols())")
    static (DenseMatrix) ("apply", List(tpePar("T")), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), DenseMatrix(tpePar("T"))), implicitArgs = List(), effect = mutable) implements allocates(DenseMatrix, "__arg0", "__arg1", "array_empty[T](__arg0.$times(__arg1))")
    static (DenseMatrix) ("apply", List(tpePar("T")), MethodSignature(List(Arg("__arg0", DenseVector(DenseVector(tpePar("T"))), None)), DenseMatrix(tpePar("T"))), implicitArgs = List()) implements composite("""{
  val numRows: this.Rep[Int] = __arg0.length();
  val numCols: this.Rep[Int] = __arg0.apply(unit(0)).length();
  scala.Tuple2.apply({
  val x$1: this.Rep[Int] = unit(0);
  numRows.$colon$colon(x$1)
}, {
  val x$2: this.Rep[Int] = unit(0);
  numCols.$colon$colon(x$2)
}).apply[T](((i: this.Rep[Int], j: this.Rep[Int]) => __arg0.apply(i).apply(j)))
}""")
    static (DenseMatrix) ("apply", List(tpePar("T")), MethodSignature(List(Arg("__arg0", DenseVector(DenseVectorView(tpePar("T"))), None)), DenseMatrix(tpePar("T"))), implicitArgs = List()) implements composite("""{
  val numRows: this.Rep[Int] = __arg0.length();
  val numCols: this.Rep[Int] = __arg0.apply(unit(0)).length();
  scala.Tuple2.apply({
  val x$3: this.Rep[Int] = unit(0);
  numRows.$colon$colon(x$3)
}, {
  val x$4: this.Rep[Int] = unit(0);
  numCols.$colon$colon(x$4)
}).apply[T](((i: this.Rep[Int], j: this.Rep[Int]) => __arg0.apply(i).apply(j)))
}""")
    /*
    static (DenseMatrix) ("apply", List(tpePar("T")), MethodSignature(List(Arg("__arg0", varArgs(DenseVector(tpePar("T"))), None)), DenseMatrix(tpePar("T"))), implicitArgs = List()) implements single("""{
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](unit(0), unit(0));
  scala.collection.immutable.Range.apply(unit(0), __arg0.length).withFilter(((check$ifrefutable$1: this.Rep[Int]) => ((check$ifrefutable$1): this.Rep[Int]) match {
  case (i @ ((_): this.Rep[Int])) => unit(true)
  case _ => unit(false)
})).foreach[Unit](((i: this.Rep[Int]) => out.$less$less$eq(__arg0.apply(i))));
  out.unsafeImmutable
}""")
    */
    static (DenseMatrix) ("diag", List(tpePar("T")), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", DenseVector(tpePar("T")), None)), DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements single("densematrix_fromfunc[T](__arg0, __arg0, ((i: this.Rep[Int], j: this.Rep[Int]) => __ifThenElse(infix_$eq$eq(i, j), __arg1.apply(i), implicitly[Arith[T]].empty)))")
    static (DenseMatrix) ("identity", List(), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), DenseMatrix(MDouble)), implicitArgs = List()) implements single("densematrix_fromfunc[Double](__arg0, __arg1, ((i: this.Rep[Int], j: this.Rep[Int]) => __ifThenElse(infix_$eq$eq(i, j), unit(1.0), unit(0.0))))")
    static (DenseMatrix) ("identity", List(), MethodSignature(List(Arg("__arg0", MInt, None)), DenseMatrix(MDouble)), implicitArgs = List()) implements single("DenseMatrix.identity(__arg0, __arg0)")
    static (DenseMatrix) ("zeros", List(), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), DenseMatrix(MDouble))) implements composite("densematrix_fromfunc[Double](__arg0, __arg1, ((i: this.Rep[Int], j: this.Rep[Int]) => unit(0.0)))")
    static (DenseMatrix) ("zerosf", List(), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), DenseMatrix(MFloat))) implements composite("densematrix_fromfunc[Float](__arg0, __arg1, ((i: this.Rep[Int], j: this.Rep[Int]) => unit(0.0F)))")
    static (DenseMatrix) ("ones", List(), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), DenseMatrix(MDouble))) implements composite("densematrix_fromfunc[Double](__arg0, __arg1, ((i: this.Rep[Int], j: this.Rep[Int]) => unit(1.0)))")
    static (DenseMatrix) ("onesf", List(), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), DenseMatrix(MFloat))) implements composite("densematrix_fromfunc[Float](__arg0, __arg1, ((i: this.Rep[Int], j: this.Rep[Int]) => unit(1.0F)))")
    static (DenseMatrix) ("rand", List(), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), DenseMatrix(MDouble))) implements composite("densematrix_fromfunc[Double](__arg0, __arg1, ((i: this.Rep[Int], j: this.Rep[Int]) => random[Double]))")
    static (DenseMatrix) ("randf", List(), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), DenseMatrix(MFloat))) implements composite("densematrix_fromfunc[Float](__arg0, __arg1, ((i: this.Rep[Int], j: this.Rep[Int]) => random[Float]))")
    static (DenseMatrix) ("randn", List(), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), DenseMatrix(MDouble))) implements composite("densematrix_fromfunc[Double](__arg0, __arg1, ((i: this.Rep[Int], j: this.Rep[Int]) => randomGaussian))")
    static (DenseMatrix) ("randnf", List(), MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), DenseMatrix(MFloat))) implements composite("densematrix_fromfunc[Float](__arg0, __arg1, ((i: this.Rep[Int], j: this.Rep[Int]) => randomGaussian.toFloat))")
    DenseMatrixOps {
      infix ("toBoolean") (MethodSignature(Nil, DenseMatrix(MBoolean)), implicitArgs = List(("conv", (tpePar("T")) ==> MBoolean))) implements map(scala.Tuple2(tpePar("T"), MBoolean), 0, "conv")
      infix ("toDouble") (MethodSignature(Nil, DenseMatrix(MDouble)), implicitArgs = List(("conv", (tpePar("T")) ==> MDouble))) implements map(scala.Tuple2(tpePar("T"), MDouble), 0, "conv")
      infix ("toFloat") (MethodSignature(Nil, DenseMatrix(MFloat)), implicitArgs = List(("conv", (tpePar("T")) ==> MFloat))) implements map(scala.Tuple2(tpePar("T"), MFloat), 0, "conv")
      infix ("toInt") (MethodSignature(Nil, DenseMatrix(MInt)), implicitArgs = List(("conv", (tpePar("T")) ==> MInt))) implements map(scala.Tuple2(tpePar("T"), MInt), 0, "conv")
      infix ("numRows") (MethodSignature(Nil, MInt)) implements getter(0, "_numRows")
      infix ("numCols") (MethodSignature(Nil, MInt)) implements getter(0, "_numCols")
      infix ("size") (MethodSignature(Nil, MInt)) implements composite("self.numRows().$times(self.numCols())")
      infix ("apply") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), tpePar("T")), implicitArgs = List()) implements composite("array_apply[T](densematrix_raw_data[T](self), densematrix_index[T](self, __arg0, __arg1))")
      infix ("apply") (MethodSignature(List(Arg("__arg0", MInt, None)), DenseVectorView(tpePar("T"))), implicitArgs = List()) implements composite("self.getRow(__arg0)")
      infix ("apply") (MethodSignature(List(Arg("__arg0", IndexVector, None)), DenseMatrix(tpePar("T"))), implicitArgs = List()) implements composite("__ifThenElse(__arg0.isRow(), DenseMatrix[T](__arg0.map[DenseVectorView[T]](((i: this.Rep[Int]) => self.getCol(i)))).t(), DenseMatrix[T](__arg0.map[DenseVectorView[T]](((i: this.Rep[Int]) => self.apply(i)))))")
      infix ("rowIndices") (MethodSignature(Nil, IndexVector)) implements composite("IndexVector(unit(0), self.numRows(), unit(false))")
      infix ("colIndices") (MethodSignature(Nil, IndexVector)) implements composite("IndexVector(unit(0), self.numCols())")
      infix ("vview") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None), Arg("__arg2", MInt, None), Arg("__arg3", MBoolean, None)), DenseVectorView(tpePar("T")))) implements composite("DenseVectorView[T](densematrix_raw_data[T](self).unsafeImmutable, __arg0, __arg1, __arg2, __arg3)")
      infix ("getRow") (MethodSignature(List(Arg("__arg0", MInt, None)), DenseVectorView(tpePar("T")))) implements composite("self.vview(__arg0.$times(self.numCols()), unit(1), self.numCols(), unit(true))")
      infix ("getCol") (MethodSignature(List(Arg("__arg0", MInt, None)), DenseVectorView(tpePar("T")))) implements composite("self.vview(__arg0, self.numCols(), self.numRows(), unit(false))")
      infix ("slice") (MethodSignature(List(Arg("startRow", MInt, None), Arg("endRow", MInt, None), Arg("startCol", MInt, None), Arg("endCol", MInt, None)), DenseMatrix(tpePar("T")))) implements single("""{
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](endRow.$minus(startRow), endCol.$minus(startCol));
  startRow.until(endRow).foreach(((i: this.Rep[Int]) => startCol.until(endCol).foreach(((j: this.Rep[Int]) => out.update(i.$minus(startRow), j.$minus(startCol), self.apply(i, j))))));
  out.unsafeImmutable
}""")
      infix ("sliceRows") (MethodSignature(List(Arg("start", MInt, None), Arg("end", MInt, None)), DenseMatrix(tpePar("T")))) implements single("""{
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](end.$minus(start), self.numCols());
  start.until(end).foreach(((i: this.Rep[Int]) => unit(0).until(self.numCols()).foreach(((j: this.Rep[Int]) => out.update(i.$minus(start), j, self.apply(i, j))))));
  out.unsafeImmutable
}""")
      infix ("sliceCols") (MethodSignature(List(Arg("start", MInt, None), Arg("end", MInt, None)), DenseMatrix(tpePar("T")))) implements single("""{
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](self.numRows(), end.$minus(start));
  unit(0).until(self.numRows()).foreach(((i: this.Rep[Int]) => start.until(end).foreach(((j: this.Rep[Int]) => out.update(i, j.$minus(start), self.apply(i, j))))));
  out.unsafeImmutable
}""")
      infix ("pprint") (MethodSignature(Nil, MUnit), implicitArgs = List(TStringable(tpePar("T"))), effect = simple) implements composite("println(self.makeStr())")
      infix ("makeString") (MethodSignature(Nil, tpePar("String")), implicitArgs = List(TStringable(tpePar("T")))) implements single("""{
  var s: this.Rep[String] = __newVar(unit(""));
  unit(0).until(self.numRows().$minus(unit(1))).foreach(((i: this.Rep[Int]) => __assign(s, s.$plus(self.apply(i).makeStr()).$plus(unit("\n")))));
  __ifThenElse(self.numRows().$greater(unit(0)), s.$plus(self.apply(self.numRows().$minus(unit(1))).makeStr()), unit("[ ]"))
}""")
      infix ("toString") (MethodSignature(Nil, tpePar("String"))) implements single("""{
  var s: this.Rep[String] = __newVar(unit(""));
  unit(0).until(self.numRows().$minus(unit(1))).foreach(((i: this.Rep[Int]) => __assign(s, s.$plus(infix_toString(self.apply(i))).$plus(unit("\n")))));
  __ifThenElse(self.numRows().$greater(unit(0)), s.$plus(infix_toString(self.apply(self.numRows().$minus(unit(1))))), unit("[ ]"))
}""")
      infix ("t") (MethodSignature(Nil, DenseMatrix(tpePar("T")))) implements single("""{
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](self.numCols(), self.numRows());
  unit(0).until(self.numCols()).foreach(((i: this.Rep[Int]) => unit(0).until(self.numRows()).foreach(((j: this.Rep[Int]) => out.update(i, j, self.apply(j, i))))));
  out.unsafeImmutable
}""")
      infix ("Clone") (MethodSignature(Nil, DenseMatrix(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e)")
      infix ("mutable") (MethodSignature(Nil, DenseMatrix(tpePar("T"))), effect = mutable) implements single("""{
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](self.numRows(), self.numCols());
  unit(0).until(self.numRows()).foreach(((i: this.Rep[Int]) => unit(0).until(self.numCols()).foreach(((j: this.Rep[Int]) => out.update(i, j, self.apply(i, j))))));
  out
}""")
      infix ("replicate") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None)), DenseMatrix(tpePar("T")))) implements single("""{
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](__arg0.$times(self.numRows()), __arg1.$times(self.numCols()));
  unit(0).until(__arg0).foreach(((ii: this.Rep[Int]) => unit(0).until(self.numRows()).foreach(((i: this.Rep[Int]) => unit(0).until(__arg1).foreach(((jj: this.Rep[Int]) => unit(0).until(self.numCols()).foreach(((j: this.Rep[Int]) => out.update(ii.$times(self.numRows()).$plus(i), jj.$times(self.numCols()).$plus(j), self.apply(i, j))))))))));
  out.unsafeImmutable
}""")
      infix ("update") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", MInt, None), Arg("__arg2", tpePar("T"), None)), MUnit), implicitArgs = List(), effect = write(0)) implements composite("array_update[T](densematrix_raw_data[T](self), densematrix_index[T](self, __arg0, __arg1), __arg2)")
      infix ("update") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", DenseVector(tpePar("T")), None)), MUnit), implicitArgs = List(), effect = write(0)) implements composite("self.updateRow(__arg0, __arg1)")
      infix ("updateRow") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", DenseVector(tpePar("T")), None)), MUnit), effect = write(0)) implements single("unit(0).until(__arg1.length()).foreach(((j: this.Rep[Int]) => self.update(__arg0, j, __arg1.apply(j))))")
      infix ("updateCol") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", DenseVector(tpePar("T")), None)), MUnit), effect = write(0)) implements single("unit(0).until(__arg1.length()).foreach(((i: this.Rep[Int]) => self.update(i, __arg0, __arg1.apply(i))))")
      infix ("<<") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), DenseMatrix(tpePar("T"))), implicitArgs = List()) implements single("""{
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](unit(0), unit(0));
  out.$less$less$eq(self);
  out.$less$less$eq(__arg0);
  out.unsafeImmutable
}""")
      infix ("<<") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), DenseMatrix(tpePar("T"))), implicitArgs = List()) implements single("""{
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](unit(0), unit(0));
  out.$less$less$eq(self);
  out.$less$less$eq(__arg0);
  out.unsafeImmutable
}""")
      infix ("<<|") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), DenseMatrix(tpePar("T"))), implicitArgs = List()) implements single("""{
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](unit(0), unit(0));
  out.insertAllCols(unit(0), self);
  out.insertCol(self.numCols(), __arg0);
  out.unsafeImmutable
}""")
      infix ("<<|") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), DenseMatrix(tpePar("T"))), implicitArgs = List()) implements single("""{
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](unit(0), unit(0));
  out.insertAllCols(unit(0), self);
  out.insertAllCols(self.numCols(), __arg0);
  out.unsafeImmutable
}""")
      infix ("<<=") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), MUnit), implicitArgs = List(), effect = write(0)) implements composite("self.insertRow(self.numRows(), __arg0)")
      infix ("<<=") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), MUnit), implicitArgs = List(), effect = write(0)) implements composite("self.insertAllRows(self.numRows(), __arg0)")
      infix ("insertRow") (MethodSignature(List(Arg("pos", MInt, None), Arg("y", DenseVector(tpePar("T")), None)), MUnit), effect = write(0)) implements single("""{
  val idx: this.Rep[Int] = pos.$times(self.numCols());
  __ifThenElse(infix_$eq$eq(self.size(), unit(0)), densematrix_set_numcols[T](self, y.length()), unit(()));
  densematrix_insertspace[T](self, idx, self.numCols());
  val data: this.Rep[ForgeArray[T]] = densematrix_raw_data[T](self);
  idx.until(idx.$plus(self.numCols())).foreach(((i: this.Rep[Int]) => array_update[T](data, i, y.apply(i.$minus(idx)))));
  densematrix_set_numrows[T](self, self.numRows().$plus(unit(1)))
}""")
      infix ("insertAllRows") (MethodSignature(List(Arg("pos", MInt, None), Arg("xs", DenseMatrix(tpePar("T")), None)), MUnit), effect = write(0)) implements single("""{
  val idx: this.Rep[Int] = pos.$times(self.numCols());
  __ifThenElse(infix_$eq$eq(self.size(), unit(0)), densematrix_set_numcols[T](self, xs.numCols()), unit(()));
  val sz: this.Rep[Int] = self.numCols().$times(xs.numRows());
  densematrix_insertspace[T](self, idx, sz);
  val data: this.Rep[ForgeArray[T]] = densematrix_raw_data[T](self);
  idx.until(idx.$plus(sz)).foreach(((i: this.Rep[Int]) => array_update[T](data, i, densematrix_raw_apply[T](xs, i.$minus(idx)))));
  densematrix_set_numrows[T](self, self.numRows().$plus(xs.numRows()))
}""")
      infix ("insertCol") (MethodSignature(List(Arg("pos", MInt, None), Arg("y", DenseVector(tpePar("T")), None)), MUnit), effect = write(0)) implements single("""{
  val newCols: this.Rep[Int] = self.numCols().$plus(unit(1));
  __ifThenElse(infix_$eq$eq(self.size(), unit(0)), densematrix_set_numrows[T](self, y.length()), unit(()));
  val outData: this.Rep[ForgeArray[T]] = array_empty[T](self.numRows().$times(newCols));
  unit(0).until(self.numRows()).foreach(((i: this.Rep[Int]) => {
    var col: this.Rep[Int] = __newVar(unit(0));
    unit(0).until(newCols).foreach(((j: this.Rep[Int]) => __ifThenElse(infix_$eq$eq(j, pos), outData.update(i.$times(newCols).$plus(j), y.apply(i)), {
      outData.update(i.$times(newCols).$plus(j), self.apply(i, col));
      __assign(col, col.$plus(unit(1)))
    })))
  }));
  densematrix_set_raw_data[T](self, outData.unsafeImmutable);
  densematrix_set_numcols[T](self, newCols)
}""")
      infix ("insertAllCols") (MethodSignature(List(Arg("pos", MInt, None), Arg("xs", DenseMatrix(tpePar("T")), None)), MUnit), effect = write(0)) implements single("""{
  val newCols: this.Rep[Int] = self.numCols().$plus(xs.numCols());
  __ifThenElse(infix_$eq$eq(self.size(), unit(0)), densematrix_set_numrows[T](self, xs.numRows()), unit(()));
  val outData: this.Rep[ForgeArray[T]] = array_empty[T](self.numRows().$times(newCols));
  unit(0).until(self.numRows()).foreach(((i: this.Rep[Int]) => {
    var col: this.Rep[Int] = __newVar(unit(0));
    unit(0).until(newCols).foreach(((j: this.Rep[Int]) => __ifThenElse(j.$less(pos).$bar$bar(j.$greater$eq(pos.$plus(xs.numCols()))), {
      outData.update(i.$times(newCols).$plus(j), self.apply(i, col));
      __assign(col, col.$plus(unit(1)))
    }, outData.update(i.$times(newCols).$plus(j), xs.apply(i, j.$minus(pos))))))
  }));
  densematrix_set_raw_data[T](self, outData.unsafeImmutable);
  densematrix_set_numcols[T](self, newCols)
}""")
      infix ("trim") (MethodSignature(Nil, MUnit), effect = write(0)) implements single("""{
  val data: this.Rep[ForgeArray[T]] = densematrix_raw_data[T](self);
  __ifThenElse(self.size().$less(array_length[T](data)), {
    val d: this.Rep[ForgeArray[T]] = array_empty[T](self.size());
    array_copy[T](data, unit(0), d, unit(0), self.size());
    densematrix_set_raw_data[T](self, d.unsafeImmutable)
  }, unit(()))
}""")
      infix ("removeRow") (MethodSignature(List(Arg("pos", MInt, None)), MUnit), effect = write(0)) implements composite("self.removeRows(pos, unit(1))")
      infix ("removeCol") (MethodSignature(List(Arg("pos", MInt, None)), MUnit), effect = write(0)) implements composite("self.removeCols(pos, unit(1))")
      infix ("removeRows") (MethodSignature(List(Arg("pos", MInt, None), Arg("num", MInt, None)), MUnit), effect = write(0)) implements single("""{
  val idx: this.Rep[Int] = pos.$times(self.numCols());
  val len: this.Rep[Int] = num.$times(self.numCols());
  val data: this.Rep[ForgeArray[T]] = densematrix_raw_data[T](self);
  array_copy[T](data, idx.$plus(len), data, idx, self.size().$minus(idx.$plus(len)));
  densematrix_set_numrows[T](self, self.numRows().$minus(num))
}""")
      infix ("removeCols") (MethodSignature(List(Arg("pos", MInt, None), Arg("num", MInt, None)), MUnit), effect = write(0)) implements single("""{
  val newCols: this.Rep[Int] = self.numCols().$minus(num);
  val outData: this.Rep[ForgeArray[T]] = array_empty[T](self.numRows().$times(newCols));
  unit(0).until(self.numRows()).foreach(((i: this.Rep[Int]) => {
    var col: this.Rep[Int] = __newVar(unit(0));
    unit(0).until(self.numCols()).foreach(((j: this.Rep[Int]) => __ifThenElse(j.$less(pos).$bar$bar(j.$greater$eq(pos.$plus(num))), {
      outData.update(i.$times(newCols).$plus(col), self.apply(i, j));
      __assign(col, col.$plus(unit(1)))
    }, unit(()))))
  }));
  densematrix_set_raw_data[T](self, outData.unsafeImmutable);
  densematrix_set_numcols[T](self, newCols)
}""")
      infix ("+") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), tpePar("T")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$plus(b))")
      infix ("+") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.$plus(__arg0))")
      infix ("+=") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("""{
  val indices: this.Rep[IndexVector] = IndexVector(unit(0), self.size());
  indices.foreach(((i: this.Rep[Int]) => densematrix_raw_update[T](self, i, densematrix_raw_apply[T](self, i).$plus(densematrix_raw_apply[T](__arg0, i)))))
}""")
      infix ("+=") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("""{
  val indices: this.Rep[IndexVector] = IndexVector(unit(0), self.size());
  indices.foreach(((i: this.Rep[Int]) => densematrix_raw_update[T](self, i, densematrix_raw_apply[T](self, i).$plus(__arg0))))
}""")
      infix ("-") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), tpePar("T")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$minus(b))")
      infix ("-") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.$minus(__arg0))")
      infix ("-=") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("""{
  val indices: this.Rep[IndexVector] = IndexVector(unit(0), self.size());
  indices.foreach(((i: this.Rep[Int]) => densematrix_raw_update[T](self, i, densematrix_raw_apply[T](self, i).$minus(densematrix_raw_apply[T](__arg0, i)))))
}""")
      infix ("-=") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("""{
  val indices: this.Rep[IndexVector] = IndexVector(unit(0), self.size());
  indices.foreach(((i: this.Rep[Int]) => densematrix_raw_update[T](self, i, densematrix_raw_apply[T](self, i).$minus(__arg0))))
}""")
      infix ("*:*") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), tpePar("T")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$times(b))")
      infix ("*") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.$times(__arg0))")
      infix ("*=") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("""{
  val indices: this.Rep[IndexVector] = IndexVector(unit(0), self.size());
  indices.foreach(((i: this.Rep[Int]) => densematrix_raw_update[T](self, i, densematrix_raw_apply[T](self, i).$times(densematrix_raw_apply[T](__arg0, i)))))
}""")
      infix ("*=") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("""{
  val indices: this.Rep[IndexVector] = IndexVector(unit(0), self.size());
  indices.foreach(((i: this.Rep[Int]) => densematrix_raw_update[T](self, i, densematrix_raw_apply[T](self, i).$times(__arg0))))
}""")
      infix ("*") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements single("""{
  __ifThenElse(infix_$bang$eq(self.numCols(), __arg0.numRows()), fatal(unit("dimension mismatch: matrix multiply")), unit(()));
  val yT: this.Rep[DenseMatrix[T]] = __arg0.t();
  val out: this.Rep[DenseMatrix[T]] = DenseMatrix[T](self.numRows(), __arg0.numCols());
  unit(0).until(self.numRows()).foreach(((rowIdx: this.Rep[Int]) => unit(0).until(__arg0.numCols()).foreach(((i: this.Rep[Int]) => {
    var acc: this.Rep[T] = __newVar(self.apply(rowIdx, unit(0)).$times(yT.apply(i, unit(0))));
    unit(1).until(yT.numCols()).foreach(((j: this.Rep[Int]) => __assign(acc, acc.$plus(self.apply(rowIdx, j).$times(yT.apply(i, j))))));
    out.update(rowIdx, i, acc)
  }))));
  out.unsafeImmutable
}""")
      infix ("*") (MethodSignature(List(Arg("__arg0", DenseVector(tpePar("T")), None)), DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements single("""{
  __ifThenElse(infix_$bang$eq(self.numCols(), __arg0.length()).$bar$bar(__arg0.isRow()), fatal(unit("dimension mismatch: matrix * vector")), unit(()));
  val out: this.Rep[DenseVector[T]] = DenseVector[T](self.numRows(), unit(false));
  unit(0).until(self.numRows()).foreach(((rowIdx: this.Rep[Int]) => out.update(rowIdx, self.apply(rowIdx).$times$colon$times(__arg0))));
  out.unsafeImmutable
}""")
      infix ("/") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), tpePar("T")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$div(b))")
      infix ("/") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.$div(__arg0))")
      infix ("/=") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("""{
  val indices: this.Rep[IndexVector] = IndexVector(unit(0), self.size());
  indices.foreach(((i: this.Rep[Int]) => densematrix_raw_update[T](self, i, densematrix_raw_apply[T](self, i).$div(densematrix_raw_apply[T](__arg0, i)))))
}""")
      infix ("/=") (MethodSignature(List(Arg("__arg0", tpePar("T"), None)), MUnit), implicitArgs = List(TArith(tpePar("T"))), effect = write(0)) implements composite("""{
  val indices: this.Rep[IndexVector] = IndexVector(unit(0), self.size());
  indices.foreach(((i: this.Rep[Int]) => densematrix_raw_update[T](self, i, densematrix_raw_apply[T](self, i).$div(__arg0))))
}""")
      infix ("sum") (MethodSignature(Nil, tpePar("T")), implicitArgs = List(TArith(tpePar("T")))) implements reduce(tpePar("T"), 0, "implicitly[Arith[T]].empty", "((a: this.Rep[T], b: this.Rep[T]) => a.$plus(b))")
      infix ("mean") (MethodSignature(Nil, MDouble), implicitArgs = List(("conv", (tpePar("T")) ==> MDouble))) implements composite("self.map[Double](conv).sum().$div(self.size())")
      infix ("abs") (MethodSignature(Nil, DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.abs())")
      infix ("exp") (MethodSignature(Nil, DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.exp())")
      infix ("log") (MethodSignature(Nil, DenseMatrix(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.log())")
      infix ("sumRows") (MethodSignature(Nil, DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements composite("self.mapRowsToVector[T](((row: this.Rep[DenseVectorView[T]]) => row.sum()))")
      infix ("sumCols") (MethodSignature(Nil, DenseVector(tpePar("T"))), implicitArgs = List(TArith(tpePar("T")))) implements composite("self.mapColsToVector[T](((col: this.Rep[DenseVectorView[T]]) => col.sum()))")
      infix ("minRows") (MethodSignature(Nil, DenseVector(tpePar("T"))), implicitArgs = List(TOrdering(tpePar("T")))) implements composite("self.mapRowsToVector[T](((row: this.Rep[DenseVectorView[T]]) => row.min()))")
      infix ("minCols") (MethodSignature(Nil, DenseVector(tpePar("T"))), implicitArgs = List(TOrdering(tpePar("T")))) implements composite("self.mapColsToVector[T](((col: this.Rep[DenseVectorView[T]]) => col.min()))")
      infix ("maxRows") (MethodSignature(Nil, DenseVector(tpePar("T"))), implicitArgs = List(TOrdering(tpePar("T")))) implements composite("self.mapRowsToVector[T](((row: this.Rep[DenseVectorView[T]]) => row.max()))")
      infix ("maxCols") (MethodSignature(Nil, DenseVector(tpePar("T"))), implicitArgs = List(TOrdering(tpePar("T")))) implements composite("self.mapColsToVector[T](((col: this.Rep[DenseVectorView[T]]) => col.max()))")
      infix ("min") (MethodSignature(Nil, tpePar("T")), implicitArgs = List(TOrdering(tpePar("T")))) implements reduce(tpePar("T"), 0, "self.apply(unit(0), unit(0))", "((a: this.Rep[T], b: this.Rep[T]) => __ifThenElse(a.$less(b), a, b))")
      infix ("max") (MethodSignature(Nil, tpePar("T")), implicitArgs = List(TOrdering(tpePar("T")))) implements reduce(tpePar("T"), 0, "self.apply(unit(0), unit(0))", "((a: this.Rep[T], b: this.Rep[T]) => __ifThenElse(a.$greater(b), a, b))")
      infix ("minIndex") (MethodSignature(Nil, lookupTpe("Tup2")(MInt, MInt)), implicitArgs = List(TOrdering(tpePar("T")))) implements composite("""{
  var min: this.Rep[T] = __newVar(self.apply(unit(0), unit(0)));
  var minRow: this.Rep[Int] = __newVar(unit(0));
  var minCol: this.Rep[Int] = __newVar(unit(0));
  unit(0).until(self.numRows()).foreach(((i: this.Rep[Int]) => unit(0).until(self.numCols()).foreach(((j: this.Rep[Int]) => __ifThenElse(self.apply(i, j).$less(min), {
    __assign(min, self.apply(i, j));
    __assign(minRow, i);
    __assign(minCol, j)
  }, unit(()))))));
  scala.Tuple2.apply(minRow, minCol)
}""")
      infix ("maxIndex") (MethodSignature(Nil, lookupTpe("Tup2")(MInt, MInt)), implicitArgs = List(TOrdering(tpePar("T")))) implements composite("""{
  var max: this.Rep[T] = __newVar(self.apply(unit(0), unit(0)));
  var maxRow: this.Rep[Int] = __newVar(unit(0));
  var maxCol: this.Rep[Int] = __newVar(unit(0));
  unit(0).until(self.numRows()).foreach(((i: this.Rep[Int]) => unit(0).until(self.numCols()).foreach(((j: this.Rep[Int]) => __ifThenElse(self.apply(i, j).$greater(max), {
    __assign(max, self.apply(i, j));
    __assign(maxRow, i);
    __assign(maxCol, j)
  }, unit(()))))));
  scala.Tuple2.apply(maxRow, maxCol)
}""")
      infix (":>") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), DenseMatrix(MBoolean)), implicitArgs = List(TOrdering(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), MBoolean), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$greater(b))")
      infix (":<") (MethodSignature(List(Arg("__arg0", DenseMatrix(tpePar("T")), None)), DenseMatrix(MBoolean)), implicitArgs = List(TOrdering(tpePar("T")))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), MBoolean), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$less(b))")
      infix ("map") (MethodSignature(List(Arg("__arg0", (tpePar("T")) ==> tpePar("R"), None)), DenseMatrix(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements map(scala.Tuple2(tpePar("T"), tpePar("R")), 0, "((e: this.Rep[T]) => __arg0.apply(e))")
      infix ("mapRowsToVector") (MethodSignature(List(Arg("__arg0", (DenseVectorView(tpePar("T"))) ==> tpePar("R"), None)), DenseVector(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements composite("self.rowIndices().map[R](((i: this.Rep[Int]) => __arg0.apply(self.apply(i))))")
      infix ("mapColsToVector") (MethodSignature(List(Arg("__arg0", (DenseVectorView(tpePar("T"))) ==> tpePar("R"), None)), DenseVector(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements composite("self.colIndices().map[R](((i: this.Rep[Int]) => __arg0.apply(self.getCol(i))))")
      infix ("zip") (CurriedMethodSignature(List(List(Arg("__arg0", DenseMatrix(tpePar("B")), None)), List(Arg("__arg1", (tpePar("T"),tpePar("B")) ==> tpePar("R"), None))), DenseMatrix(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("B"), tpePar("R"))) implements zip(scala.Tuple3(tpePar("T"), tpePar("B"), tpePar("R")), scala.Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[B]) => __arg1.apply(a, b))")
      infix ("foreach") (MethodSignature(List(Arg("__arg0", (tpePar("T")) ==> MUnit, None)), MUnit)) implements foreach(tpePar("T"), 0, "((e: this.Rep[T]) => __arg0.apply(e))")
    }
  }
}