package vector.ident

import ppl.dsl.forge.core.{ForgeApplication,ForgeApplicationRunner}

object SVectorDSLRunner extends ForgeApplicationRunner with SVectorDSL

trait SVectorDSL extends ForgeApplication { this: ppl.dsl.forge.core.ForgeOpsExp => 
  /* The name of your DSL. This is the name that will be used in generated files, package declarations, etc. */
  def dslName = "SVector"

  /* The specification is the DSL definition (types, data structures, ops) */
  def specification() = {
    importScalaOps()
    importVectorOps()
  }
  def importVectorOps() {
    val T = tpePar("T")
    val Vector = tpe("Vector", List(T))

    data(Vector, ("_length", MInt), ("_data", MArray(T)))

    val VectorOps = withTpe (Vector)

    compiler (Vector) ("vector_copy", List(T), MethodSignature(List(Arg("self", Vector, None), Arg("__arg1", MInt, None), Arg("__arg2", Vector(T), None), Arg("__arg3", MInt, None), Arg("__arg4", MInt, None)), MUnit), implicitArgs = List(), effect = write(2)) implements single("{\n  val src: this.Rep[ForgeArray[T]] = this.vector_raw_data[T](self);\n  val dest: this.Rep[ForgeArray[T]] = this.vector_raw_data[T](__arg2);\n  array_copy[T](src, __arg1, dest, __arg3, __arg4)\n}")
    compiler (Vector) ("vector_appendable", List(T), MethodSignature(List(Arg("self", Vector, None), Arg("__arg1", MInt, None), Arg("__arg2", T, None)), MBoolean), implicitArgs = List()) implements single("unit(true)")
    compiler (Vector) ("vector_raw_alloc", List(T, tpePar("R")), MethodSignature(List(Arg("self", Vector, None), Arg("__arg1", MInt, None)), Vector(tpePar("R"))), implicitArgs = List()) implements single("Vector[R](__arg1)")
    compiler (Vector) ("vector_realloc", List(T), MethodSignature(List(Arg("self", Vector, None), Arg("minLen", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("{\n  val data: this.Rep[ForgeArray[T]] = this.vector_raw_data[T](self);\n  var n: this.Rep[Int] = __newVar(Math.max(unit(4), array_length[T](data).$times(unit(2))).toInt);\n  __whileDo(n.$less(minLen), __assign(n, n.$times(unit(2))));\n  val d: this.Rep[ForgeArray[T]] = array_empty[T](n);\n  array_copy[T](data, unit(0), d, unit(0), self.length());\n  this.vector_set_raw_data[T](self, d.unsafeImmutable)\n}")
    compiler (Vector) ("vector_ensureextra", List(T), MethodSignature(List(Arg("self", Vector, None), Arg("extra", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("{\n  val data: this.Rep[ForgeArray[T]] = this.vector_raw_data[T](self);\n  __ifThenElse(array_length[T](data).$minus(self.length()).$less(extra), this.vector_realloc[T](self, self.length().$plus(extra)), unit(()))\n}")
    compiler (Vector) ("vector_insertspace", List(T), MethodSignature(List(Arg("self", Vector, None), Arg("pos", MInt, None), Arg("len", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("{\n  this.vector_ensureextra[T](self, len);\n  val data: this.Rep[ForgeArray[T]] = this.vector_raw_data[T](self);\n  array_copy[T](data, pos, data, pos.$plus(len), self.length().$minus(pos));\n  this.vector_set_length[T](self, self.length().$plus(len))\n}")
    compiler (Vector) ("vector_set_length", List(T), MethodSignature(List(Arg("self", Vector, None), Arg("__arg1", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements setter(0, "_length", "__arg1")
    compiler (Vector) ("vector_set_raw_data", List(T), MethodSignature(List(Arg("self", Vector, None), Arg("__arg1", MArray(T), None)), MUnit), implicitArgs = List(), effect = write(0)) implements setter(0, "_data", "__arg1")
    compiler (Vector) ("vector_raw_data", List(T), MethodSignature(List(Arg("self", Vector, None)), MArray(T)), implicitArgs = List()) implements getter(0, "_data")
    static (Vector) ("apply", List(T), MethodSignature(List(Arg("__arg0", MInt, None)), Vector(T)), implicitArgs = List(), effect = mutable) implements allocates(Vector, "__arg0", "array_empty[T](__arg0)")
    VectorOps {
      infix ("pprint") (MethodSignature(Nil, MUnit), effect = simple) implements foreach(tpePar("T"), 0, "((a: this.Rep[T]) => println(a))")
      infix ("hashreduce") (MethodSignature(List(Arg("__arg0", (T) ==> tpePar("K"), None), Arg("__arg1", (T) ==> tpePar("V"), None), Arg("__arg2", (tpePar("V"),tpePar("V")) ==> tpePar("V"), None)), Vector(tpePar("V"))), implicitArgs = List(TNumeric(tpePar("V"))), addTpePars = List(tpePar("K"), tpePar("V"))) implements hashFilterReduce(scala.Tuple3(tpePar("T"), tpePar("K"), tpePar("V")), 0, "((e: this.Rep[T]) => unit(true))", "((e: this.Rep[T]) => __arg0.apply(e))", "((e: this.Rep[T]) => __arg1.apply(e))", "numeric_zero[V]", "((a: this.Rep[V], b: this.Rep[V]) => __arg2.apply(a, b))")
      infix ("mapreduce") (MethodSignature(List(Arg("__arg0", (T) ==> T, None), Arg("__arg1", (T,T) ==> T, None)), T), implicitArgs = List(TNumeric(T))) implements mapReduce(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => __arg0.apply(e))", "numeric_zero[T]", "((a: this.Rep[T], b: this.Rep[T]) => __arg1.apply(a, b))")
      infix ("filter") (MethodSignature(List(Arg("__arg0", (T) ==> MBoolean, None)), Vector(T))) implements filter(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => __arg0.apply(e))", "((e: this.Rep[T]) => e)")
      infix ("reduce") (MethodSignature(List(Arg("__arg0", (T,T) ==> T, None)), T), implicitArgs = List(TNumeric(T))) implements reduce(tpePar("T"), 0, "numeric_zero[T]", "((a: this.Rep[T], b: this.Rep[T]) => __arg0.apply(a, b))")
      infix ("map") (MethodSignature(List(Arg("__arg0", (T) ==> tpePar("R"), None)), Vector(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements map(scala.Tuple2(tpePar("T"), tpePar("R")), 0, "((e: this.Rep[T]) => __arg0.apply(e))")
      infix ("sum") (MethodSignature(Nil, T), implicitArgs = List(TNumeric(T))) implements reduce(tpePar("T"), 0, "numeric_zero[T]", "((a: this.Rep[T], b: this.Rep[T]) => a.$plus(b))")
      infix ("$times") (MethodSignature(List(Arg("__arg0", T, None)), Vector(T)), implicitArgs = List(TNumeric(T))) implements map(scala.Tuple2(tpePar("T"), tpePar("T")), 0, "((e: this.Rep[T]) => e.$times(__arg0))")
      infix ("$plus") (MethodSignature(List(Arg("__arg0", Vector(T), None)), Vector(T)), implicitArgs = List(TNumeric(T))) implements zip(scala.Tuple3(tpePar("T"), tpePar("T"), tpePar("T")), Tuple2(0, 1), "((a: this.Rep[T], b: this.Rep[T]) => a.$plus(b))")
      infix ("append") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", T, None)), MUnit), effect = write(0)) implements single("self.insert(self.length(), __arg1)")
      infix ("insert") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", T, None)), MUnit), effect = write(0)) implements single("{\n  vector_insertspace[T](self, __arg0, unit(1));\n  self.update(__arg0, __arg1)\n}")
      infix ("slice") (MethodSignature(List(Arg("start", MInt, None), Arg("end", MInt, None)), Vector(T))) implements single("{\n  val out: this.Rep[Vector[T]] = Vector[T](end.$minus(start));\n  var i: this.Rep[Int] = __newVar(start);\n  __whileDo(i.$less(end), {\n    out.update(i.$minus(start), self.apply(i));\n    __assign(i, i.$plus(unit(1)))\n  });\n  out\n}")
      infix ("update") (MethodSignature(List(Arg("i", MInt, None), Arg("e", T, None)), MUnit), effect = write(0)) implements composite("array_update[T](vector_raw_data[T](self), i, e)")
      infix ("apply") (MethodSignature(List(Arg("__arg0", MInt, None)), T)) implements composite("array_apply[T](vector_raw_data[T](self), __arg0)")
      infix ("length") (MethodSignature(Nil, MInt)) implements getter(0, "_length")

      parallelize as ParallelCollectionBuffer(T, lookupOp("vector_raw_alloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("update"), lookupOp("vector_set_length"), lookupOp("vector_appendable"), lookupOp("append"), lookupOp("vector_copy"))
    }
    val z = direct (Vector) ("foo", T, List(MInt ==> T, MInt, MThunk(MInt), (MInt,MInt) ==> MInt, MDouble, MDouble ==> MDouble) :: T) implements codegen($cala, ${
      var i = 0
      val a = new Array[$t[T]](100)

      while (i < 100) {
        a(i) = $b[0]($b[3]($b[2],$b[2]))
        a(i) = $b[0]($b[3]($1,$1))
        i += 1
      }
      println("a(5) = " + a(5))
      val z = $b[5]($b[4])
      val y = $b[2]+$1
      println("z = " + z)
      println("y = " + y)
      a(5)
    })
  }


}