package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object SimpleVectorDSLRunner extends ForgeApplicationRunner with SimpleVectorDSL

trait SimpleVectorDSL extends ForgeApplication {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "SimpleVector"

  /**
   * The specification is the DSL definition (types, data structures, ops)
   */
  def specification() = {
    /**
     * Include Scala ops
     */
    importScalaOps()


    /**
     * The main portion of our DSL
     */
    importVectorOps()
  }


  def importVectorOps() {
    // generic type parameters we will use
    val T = tpePar("T")
    val R = tpePar("R")

    val Vector = tpe("Vector", T)

    // data fields
    data(Vector, ("_length", MInt), ("_data", MArray(T)))

    // allocation
    static (Vector) ("apply", T, MInt :: Vector(T), effect = mutable) implements allocates(Vector, {
  val arg1 = quotedArg(0)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(0)
  s"""array_empty[T]($arg1)"""
})

    // doesn't rewrite correctly if we use "withTpe (Vector) {", but works if we use:
    val VectorOps = withTpe (Vector)

    VectorOps {
      // getters and setters
      compiler ("vector_raw_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("vector_set_raw_data") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      compiler ("vector_set_length") (MInt :: MUnit, effect = write(0)) implements setter(0, "_length", quotedArg(1))


      // data ops
      infix ("apply") (MInt :: T) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_apply(vector_raw_data($self), $arg1)"""
      }
      // example named arg
      infix ("update") ((("i",MInt),("e",T)) :: MUnit, effect = write(0)) implements composite {
          val self = quotedArg("self")
          val i = quotedArg("i")
          val e = quotedArg("e")
          s"""array_update(vector_raw_data($self), $i, $e)"""
        }

      // example named, default arg. 'MethodSignature' is currently explicitly needed when mixing arg types.
      infix ("slice") (MethodSignature(List(("start",MInt,"unit(0)"),("end",MInt)), Vector(T))) implements single {
          val end = quotedArg("end")
          val start = quotedArg("start")
          val self = quotedArg("self")
          s"""val out = Vector[T]($end - $start)
var i = $start
while (i < $end) {
  out(i-$start) = $self(i)
  i += 1
}
out"""
        }

      infix ("insert") ((MInt,T) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""vector_insertspace($self,$arg1,1)
$self($arg1) = $arg2"""
        }

      infix ("append") ((MInt,T) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(2)
          s"""$self.insert($self.length, $arg1)"""
        }

      compiler ("vector_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val len = quotedArg("len")
          val pos = quotedArg("pos")
          s"""vector_ensureextra($self,$len)
val data = vector_raw_data($self)
array_copy(data,$pos,data,$pos+$len,$self.length-$pos)
vector_set_length($self,$self.length+$len)"""
        }

      compiler ("vector_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val extra = quotedArg("extra")
          s"""val data = vector_raw_data($self)
if (array_length(data) - $self.length < $extra) {
  vector_realloc($self, $self.length+$extra)
}"""
        }

      compiler ("vector_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val minLen = quotedArg("minLen")
          s"""val data = vector_raw_data($self)
var n = unit(4) max (array_length(data)*2)
while (n < $minLen) n = n*2
val d = array_empty[T](n)
array_copy(data, 0, d, 0, $self.length)
vector_set_raw_data($self, d.unsafeImmutable)"""
        }


      // math
      infix ("+") (Vector(T) :: Vector(T), TNumeric(T)) implements zip((T,T,T), (0,1), {
        s"""(a,b) => a+b"""
      })
      infix ("*") (T :: Vector(T), TNumeric(T)) implements map((T,T), 0, "e => e*"+quotedArg(1))
      infix ("sum") (Nil :: T, TNumeric(T)) implements reduce(T, 0, {
  s"""numeric_zero[T]"""
}, {
  s"""(a,b) => a+b"""
})

      // bulk
      infix ("map") ((T ==> R) :: Vector(R), addTpePars = R) implements map((T,R), 0, {
  val arg1 = quotedArg(1)
  s"""e => $arg1(e)"""
})

      infix ("reduce") (((T,T) ==> T) :: T, TNumeric(T)) implements reduce(T, 0, {
          s"""numeric_zero[T]"""
        }, {
          val arg1 = quotedArg(1)
          s"""(a,b) => $arg1(a,b)"""
        })

      infix ("filter") ((T ==> MBoolean) :: Vector(T)) implements filter((T,T), 0, {
  val arg1 = quotedArg(1)
  s"""e => $arg1(e)"""
}, {
  s"""e => e"""
})

      infix ("mapreduce") ((T ==> T,(T,T) ==> T) :: T, TNumeric(T)) implements mapReduce((T,T), 0, {
  val arg1 = quotedArg(1)
  s"""e => $arg1(e)"""
}, {
  s"""numeric_zero[T]"""
}, {
  val arg1 = quotedArg(2)
  s"""(a,b) => $arg1(a,b)"""
})

      infix ("flatMap") ((T ==> Vector(R)) :: Vector(R), addTpePars = R) implements flatMap((T,R), 0, {
  val arg1 = quotedArg(1)
  s"""e => $arg1(e)"""
})

      val K = tpePar("K")
      val V = tpePar("V")

      // the Forge 'groupBy' pattern is currently limited to returning an MHashMap(K,MArrayBuffer(V)), so we need to convert that to a DSL type to return
      compiler ("groupby_helper") ((T ==> K,T ==> V) :: MHashMap(K, MArrayBuffer(V)), addTpePars = (K,V)) implements groupBy((T,K,V), 0, {
  val arg1 = quotedArg(1)
  s"""e => $arg1(e)"""
}, {
  val arg1 = quotedArg(2)
  s"""e => $arg1(e)"""
})

      infix ("groupBy") ((T ==> K,T ==> V) :: Vector(Vector(V)), addTpePars = (K,V)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""val map = groupby_helper($self, $arg1, $arg2)
val groups = fhashmap_values(map)
val out = Vector[Vector[V]](array_length(groups))
var i = 0
while (i < array_length(groups)) {
  val inGroup = groups(i)
  val outGroup = Vector[V](array_buffer_length(inGroup))
  var j = 0
  while (j < array_buffer_length(inGroup)) {
    outGroup(j) = array_buffer_apply(inGroup, j)
    j += 1
  }
  out(i) = outGroup.unsafeImmutable
  i += 1
}
out.unsafeImmutable"""
        }

      infix ("groupByReduce") ((T ==> K,T ==> V,(V,V) ==> V) :: MHashMap(K,V), TNumeric(V), addTpePars = (K,V)) implements groupByReduce((T,K,V), 0, {
  val arg1 = quotedArg(1)
  s"""e => $arg1(e)"""
}, {
  val arg1 = quotedArg(2)
  s"""e => $arg1(e)"""
}, {
  s"""numeric_zero[V]"""
}, {
  val arg1 = quotedArg(3)
  s"""(a,b) => $arg1(a,b)"""
})

      // misc
      // will print out of order in parallel, but hey
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(T, 0, {
  s"""a => println(a)"""
})


      // parallel collectionification
      // This enables a tpe to be passed in as the collection type of a Delite op

      // by convention, the return tpe of alloc must be its last tpe parameter, if it has any
      compiler ("vector_raw_alloc") (MInt :: Vector(R), addTpePars = R, effect = mutable) implements composite {
          val arg1 = quotedArg(1)
          s"""Vector[R]($arg1)"""
        }
      compiler ("vector_appendable") ((MInt,T) :: MBoolean) implements single("true")
      compiler ("vector_copy") ((MInt,Vector(T),MInt,MInt) :: MUnit, effect = write(2)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(2)
          val arg2 = quotedArg(1)
          val arg3 = quotedArg(3)
          val arg4 = quotedArg(4)
          s"""val src = vector_raw_data($self)
val dest = vector_raw_data($arg1)
array_copy(src, $arg2, dest, $arg3, $arg4)"""
        }

      parallelize as ParallelCollectionBuffer(T, lookupOp("vector_raw_alloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("update"), lookupOp("vector_set_length"), lookupOp("vector_appendable"), lookupOp("append"), lookupOp("vector_copy"))
    }

    /* Testing: some codegen op that takes a block with arguments */
    val z = direct (Vector) ("foo", T, List(MInt ==> T, MInt, MThunk(MInt), (MInt,MInt) ==> MInt, MDouble, MDouble ==> MDouble) :: T) implements codegen($cala, {
        val arg1 = quotedBlock(2, lookupOp(Vector,"foo"), List())
        val arg2 = quotedBlock(3, lookupOp(Vector,"foo"), List(s"""$arg1""",s"""$arg1"""))
        val arg3 = quotedBlock(0, lookupOp(Vector,"foo"), List(s"""$arg2"""))
        val arg4 = quotedArg(1)
        val arg5 = quotedBlock(3, lookupOp(Vector,"foo"), List(s"""$arg4""",s"""$arg4"""))
        val arg6 = quotedBlock(0, lookupOp(Vector,"foo"), List(s"""$arg5"""))
        val arg7 = quotedBlock(4, lookupOp(Vector,"foo"), List())
        val arg8 = quotedBlock(5, lookupOp(Vector,"foo"), List(s"""$arg7"""))
        val tpeT = quotedTpe("T", lookupOp(Vector,"foo"))
        s"""var i = 0
val a = new Array[$tpeT](100)

while (i < 100) {
  a(i) = $arg3
  a(i) = $arg6
  i += 1
}
println("a(5) = " + a(5))
val z = $arg8
val y = $arg1+$arg4
println("z = " + z)
println("y = " + y)
a(5)"""
      })

    /* Test for C++ supporting a block with a return value to be input of other blocks.
       Currently 'foo' above cannot be generated for C++ target because the outer-most block has return (e.g., $b[0]).
       TODO: enable the outermost blocks with a return.
    */
    val foo2 = direct (Vector) ("foo2", T, List(MInt ==> MUnit, MInt, MThunk(MInt), (MInt,T) ==> MInt) :: MUnit)

    impl (foo2) (codegen($cala, {
        val arg1 = quotedArg(1)
        val arg2 = quotedBlock(3, foo2, List(s"""$arg1""",s"""a(i)"""))
        val arg3 = quotedBlock(0, foo2, List(s"""$arg2"""))
        val tpeT = quotedTpe("T", foo2)
        s"""var i = 0
val a = new Array[$tpeT](5)

while (i < 5) {
  $arg3
  i += 1
}
println("i = " + i)"""
      }))

    impl (foo2) (codegen(cpp, {
        val arg1 = quotedArg(1)
        val arg2 = quotedBlock(3, foo2, List(s"""$arg1""",s"""a[i]"""))
        val arg3 = quotedBlock(0, foo2, List(s"""$arg2"""))
        val tpeT = quotedTpe("T", foo2)
        s"""int i = 0;
$tpeT *a = new $tpeT();
while (i < 5) {
  $arg3;
  i += 1;
}
delete[] a;
std::cout << "i = " << i << std::endl;"""
      }))

    ()
  }
}

