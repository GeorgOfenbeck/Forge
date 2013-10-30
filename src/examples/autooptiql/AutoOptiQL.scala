package ppl.dsl.forge.examples.autooptiql

import ppl.dsl.forge.core.{ForgeApplication,ForgeApplicationRunner}

object AutoOptiQLDSLRunner extends ForgeApplicationRunner with AutoOptiQLDSL

trait AutoOptiQLDSL extends ForgeApplication { this: ppl.dsl.forge.core.ForgeOpsExp => 
  /* The name of your DSL. This is the name that will be used in generated files, package declarations, etc. */
  def dslName = "AutoOptiQL"

  /* The specification is the DSL definition (types, data structures, ops) */
  def specification() = {
    extern(grp("Rewrite"))
    importScalaOps()
    importTableOps()
    extern(grp("Date"))
    importTuple2Extras()
  }
  def importTableOps() {
    val A = tpePar("A")
    val Table = tpe("Table", List(A))

    data(Table, ("size", MInt), ("data", MArray(A)))

    val TableOps = withTpe (Table)

    compiler (Table) ("table_copy", List(A), MethodSignature(List(Arg("self", Table, None), Arg("__arg1", MInt, None), Arg("__arg2", Table(A), None), Arg("__arg3", MInt, None), Arg("__arg4", MInt, None)), MUnit), implicitArgs = List(), effect = write(2)) implements single("{\n  val src: this.Rep[ForgeArray[A]] = this.table_raw_data[A](self);\n  val dest: this.Rep[ForgeArray[A]] = this.table_raw_data[A](__arg2);\n  array_copy[A](src, __arg1, dest, __arg3, __arg4)\n}")
    compiler (Table) ("table_dc_append", List(A), MethodSignature(List(Arg("self", Table, None), Arg("__arg1", MInt, None), Arg("__arg2", A, None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("self.append(__arg2)")
    compiler (Table) ("table_appendable", List(A), MethodSignature(List(Arg("self", Table, None), Arg("__arg1", MInt, None), Arg("__arg2", A, None)), MBoolean), implicitArgs = List()) implements single("unit(true)")
    compiler (Table) ("table_realloc", List(A), MethodSignature(List(Arg("self", Table, None), Arg("minLen", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("{\n  val data: this.Rep[ForgeArray[A]] = this.table_raw_data[A](self);\n  var n: this.Rep[Int] = __newVar(Math.max(unit(4), array_length[A](data).$times(unit(2))).toInt);\n  __whileDo(n.$less(minLen), __assign(n, n.$times(unit(2))));\n  val d: this.Rep[ForgeArray[A]] = array_empty[A](n);\n  array_copy[A](data, unit(0), d, unit(0), this.table_size[A](self));\n  this.table_set_raw_data[A](self, d.unsafeImmutable)\n}")
    compiler (Table) ("table_ensureextra", List(A), MethodSignature(List(Arg("self", Table, None), Arg("extra", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("{\n  val data: this.Rep[ForgeArray[A]] = this.table_raw_data[A](self);\n  __ifThenElse(array_length[A](data).$minus(this.table_size[A](self)).$less(extra), this.table_realloc[A](self, this.table_size[A](self).$plus(extra)), unit(()))\n}")
    compiler (Table) ("table_insertspace", List(A), MethodSignature(List(Arg("self", Table, None), Arg("pos", MInt, None), Arg("len", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements single("{\n  this.table_ensureextra[A](self, len);\n  val data: this.Rep[ForgeArray[A]] = this.table_raw_data[A](self);\n  array_copy[A](data, pos, data, pos.$plus(len), this.table_size[A](self).$minus(pos));\n  this.table_set_size[A](self, this.table_size[A](self).$plus(len))\n}")
    compiler (Table) ("table_alloc", List(A, tpePar("R")), MethodSignature(List(Arg("self", Table, None), Arg("__arg1", MInt, None)), Table(tpePar("R"))), implicitArgs = List(), effect = mutable) implements composite("Table[R](__arg1)")
    compiler (Table) ("table_update", List(A), MethodSignature(List(Arg("self", Table, None), Arg("i", MInt, None), Arg("e", A, None)), MUnit), implicitArgs = List(), effect = write(0)) implements composite("array_update[A](this.table_raw_data[A](self), i, e)")
    compiler (Table) ("table_set_size", List(A), MethodSignature(List(Arg("self", Table, None), Arg("__arg1", MInt, None)), MUnit), implicitArgs = List(), effect = write(0)) implements setter(0, "size", "__arg1")
    compiler (Table) ("table_set_raw_data", List(A), MethodSignature(List(Arg("self", Table, None), Arg("__arg1", MArray(A), None)), MUnit), implicitArgs = List(), effect = write(0)) implements setter(0, "data", "__arg1")
    compiler (Table) ("table_apply", List(A), MethodSignature(List(Arg("self", Table, None), Arg("__arg1", MInt, None)), A), implicitArgs = List()) implements composite("array_apply[A](this.table_raw_data[A](self), __arg1)")
    compiler (Table) ("table_size", List(A), MethodSignature(List(Arg("self", Table, None)), MInt), implicitArgs = List()) implements getter(0, "size")
    compiler (Table) ("table_raw_data", List(A), MethodSignature(List(Arg("self", Table, None)), MArray(A)), implicitArgs = List()) implements getter(0, "data")
    compiler (Table) ("bulkDivide", List(A), MethodSignature(List(Arg("self", Table, None), Arg("counts", Table(MInt), None), Arg("avgFunc", (A,MInt) ==> A, None)), Table(A)), implicitArgs = List()) implements zip(scala.Tuple3(tpePar("A"), MInt, tpePar("A")), Tuple2(0, 1), "avgFunc")
    compiler (Table) ("groupByReduce", List(A, tpePar("K"), tpePar("V")), MethodSignature(List(Arg("self", Table, None), Arg("keySelector", (A) ==> tpePar("K"), None), Arg("valueSelector", (A) ==> tpePar("V"), None), Arg("reducer", (tpePar("V"),tpePar("V")) ==> tpePar("V"), None), Arg("condition", (A) ==> MBoolean, None)), Table(tpePar("V"))), implicitArgs = List()) implements hashFilterReduce(scala.Tuple3(tpePar("A"), tpePar("K"), tpePar("V")), 0, "condition", "keySelector", "valueSelector", "this.zeroType[V]", "reducer")
    static (Table) ("fromFile", List(A), MethodSignature(List(Arg("__arg0", tpePar("String"), None), Arg("__arg1", (tpePar("String")) ==> A, None)), Table(A)), implicitArgs = List()) implements single("{\n  val a: this.Rep[ForgeArray[A]] = ForgeFileReader.readLines[A](__arg0)(__arg1);\n  Table[A](a)\n}")
    static (Table) ("apply", List(A), MethodSignature(List(Arg("__arg0", varArgs(A), None)), Table(A)), implicitArgs = List()) implements allocates(Table, "array_length[A](array_fromseq[A](__arg0))", "array_fromseq[A](__arg0)")
    static (Table) ("apply", List(A), MethodSignature(List(Arg("__arg0", MArray(A), None)), Table(A)), implicitArgs = List()) implements allocates(Table, "array_length[A](__arg0)", "__arg0")
    static (Table) ("apply", List(A), MethodSignature(List(Arg("__arg0", MInt, None)), Table(A)), implicitArgs = List(), effect = mutable) implements allocates(Table, "__arg0", "array_empty[A](__arg0)")
    TableOps {
      infix ("append") (MethodSignature(List(Arg("__arg0", A, None)), MUnit), effect = write(0)) implements single("self.insert(table_size[A](self), __arg0)")
      infix ("insert") (MethodSignature(List(Arg("__arg0", MInt, None), Arg("__arg1", A, None)), MUnit), effect = write(0)) implements single("{\n  table_insertspace[A](self, __arg0, unit(1));\n  table_update[A](self, __arg0, __arg1)\n}")
      infix ("ThenBy") (MethodSignature(List(Arg("selector", (A) ==> tpePar("R"), None)), Table(A)), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements single("self")
      infix ("OrderBy") (MethodSignature(List(Arg("selector", (A) ==> tpePar("R"), None)), Table(A)), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements single("self")
      infix ("Last") (MethodSignature(Nil, A)) implements single("table_apply[A](self, table_size[A](self).$minus(unit(1)))")
      infix ("First") (MethodSignature(Nil, A)) implements single("table_apply[A](self, unit(0))")
      infix ("Count") (MethodSignature(Nil, MInt)) implements single("table_size[A](self)")
      infix ("Average") (MethodSignature(List(Arg("selector", (A) ==> tpePar("R"), None)), tpePar("R")), implicitArgs = List(TFractional(tpePar("R")), TNumeric(tpePar("R"))), addTpePars = List(tpePar("R"))) implements single("self.Sum[R](selector).$div(upgradeInt[R](self.Count()))")
      infix ("Sum") (MethodSignature(List(Arg("selector", (A) ==> tpePar("R"), None)), tpePar("R")), implicitArgs = List(TNumeric(tpePar("R"))), addTpePars = List(tpePar("R"))) implements mapReduce(scala.Tuple2(tpePar("A"), tpePar("R")), 0, "selector", "zeroType[R]", "((a: this.Rep[R], b: this.Rep[R]) => a.$plus(b))")
      infix ("GroupBy") (MethodSignature(List(Arg("keySelector", (A) ==> tpePar("K"), None)), Table(lookupTpe("Tup2")(tpePar("K"), Table(A)))), implicitArgs = List(), addTpePars = List(tpePar("K"))) implements single("groupByHackImpl[K, A](self, keySelector)")
      infix ("Where") (MethodSignature(List(Arg("predicate", (A) ==> MBoolean, None)), Table(A))) implements filter(scala.Tuple2(tpePar("A"), tpePar("A")), 0, "predicate", "((e: this.Rep[A]) => e)")
      infix ("Select") (MethodSignature(List(Arg("selector", (A) ==> tpePar("R"), None)), Table(tpePar("R"))), implicitArgs = List(), addTpePars = List(tpePar("R"))) implements map(scala.Tuple2(tpePar("A"), tpePar("R")), 0, "selector")

      parallelize as ParallelCollectionBuffer(A, lookupOp("table_alloc"), lookupOp("table_size"), lookupOp("table_apply"), lookupOp("table_update"), lookupOp("table_set_size"), lookupOp("table_appendable"), lookupOp("table_dc_append"), lookupOp("table_copy"))
    }
  }

  def importTuple2Extras() {
    val K = tpePar("K")
    val V = tpePar("V")
    val Tuple2 = lookupTpe("Tup2")
    val Table = lookupTpe("Table")
    fimplicit (Tuple2) ("pair_to_value", (K,V), Tuple2(K,Table(V)) :: Table(V)) implements composite ${ tup2__2($0) } //TODO: this isn't kicking in
    infix (Tuple2) ("key", (K,V), Tuple2(K,Table(V)) :: K) implements composite ${ tup2__1($0) }
    infix (Tuple2) ("value", (K,V), Tuple2(K,Table(V)) :: Table(V)) implements composite ${ tup2__2($0) }
  }
}