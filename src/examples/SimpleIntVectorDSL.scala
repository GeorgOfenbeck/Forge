package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * This DSL is here to make sure that things still work without type parameters.
 * It's a sad replacement for actual unit tests, which are still a big TODO.
 */
object SimpleIntVectorDSLRunner extends ForgeApplicationRunner with SimpleIntVectorDSL

trait SimpleIntVectorDSL extends ForgeApplication {
  def dslName = "SimpleIntVector"

  def specification() = {
    importScalaOps()
    importSimpleVectorOps()
  }

  def importSimpleVectorOps() {
    val Vector = tpe("Vector")
    data(Vector, ("_length", MInt), ("_data", MArray(MInt)))
    static (Vector) ("apply", Nil, MInt :: Vector, effect = mutable) implements allocates(Vector, {
  val arg1 = quotedArg(0)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(0)
  s"""array_empty[Int]($arg1)"""
})

    val VectorOps = withTpe (Vector)
    VectorOps {
      compiler ("vector_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_data")
      compiler ("vector_set_raw_data") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      compiler ("vector_set_length") (MInt :: MUnit, effect = write(0)) implements setter(0, "_length", quotedArg(1))

      infix ("apply") (MInt :: MInt) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_apply(vector_raw_data($self), $arg1)"""
      }
      infix ("update") ((("i",MInt),("e",MInt)) :: MUnit, effect = write(0)) implements composite {
          val self = quotedArg("self")
          val i = quotedArg("i")
          val e = quotedArg("e")
          s"""array_update(vector_raw_data($self), $i, $e)"""
        }

      infix ("slice") (MethodSignature(List(("start",MInt,"unit(0)"),("end",MInt)), Vector)) implements single {
          val end = quotedArg("end")
          val start = quotedArg("start")
          val self = quotedArg("self")
          s"""val out = Vector($end - $start)
var i = $start
while (i < $end) {
  out(i-$start) = $self(i)
  i += 1
}
out"""
        }

      infix ("insert") ((MInt,MInt) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""vector_insertspace($self,$arg1,1)
$self($arg1) = $arg2"""
        }

      infix ("append") ((MInt,MInt) :: MUnit, effect = write(0)) implements single {
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
var n = Math.max(unit(4), array_length(data)*2).toInt
while (n < $minLen) n = n*2
val d = array_empty[Int](n)
array_copy(data, 0, d, 0, $self.length)
vector_set_raw_data($self, d.unsafeImmutable)"""
        }


      // math
      infix ("+") (Vector :: Vector) implements zip((MInt,MInt,MInt), (0,1), {
        s"""(a,b) => forge_int_plus(a,b)"""
      }) // unfortunate conflict with Delite PrimitiveOps unless we specify (TODO: need more disambiguations)
      infix ("*") (MInt :: Vector) implements map((MInt,MInt), 0, "e => e*"+quotedArg(1))
      infix ("sum") (Nil :: MInt) implements reduce(MInt, 0, {
  s"""0"""
}, {
  s"""(a,b) => forge_int_plus(a,b)"""
})

      // bulk
      infix ("map") ((MInt ==> MInt) :: Vector) implements map((MInt,MInt), 0, {
  val arg1 = quotedArg(1)
  s"""e => $arg1(e)"""
})

      infix ("reduce") (((MInt,MInt) ==> MInt) :: MInt) implements reduce(MInt, 0, {
          s"""0"""
        }, {
          val arg1 = quotedArg(1)
          s"""(a,b) => $arg1(a,b)"""
        })

      infix ("filter") ((MInt ==> MBoolean) :: Vector) implements filter((MInt,MInt), 0, {
  val arg1 = quotedArg(1)
  s"""e => $arg1(e)"""
}, {
  s"""e => e"""
})

      infix ("mapreduce") ((MInt ==> MInt,(MInt,MInt) ==> MInt) :: MInt) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""$self.map($arg1).reduce($arg2)"""
        }

      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(MInt, 0, {
  s"""a => println(a)"""
})


      // parallel collectionification
      compiler ("vector_raw_alloc") (MInt :: Vector) implements single {
          val arg1 = quotedArg(1)
          s"""Vector($arg1)"""
        }
      compiler ("vector_appendable") ((MInt,MInt) :: MBoolean) implements single("true")
      compiler ("vector_copy") ((MInt,Vector,MInt,MInt) :: MUnit, effect = write(2)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(2)
          val arg2 = quotedArg(1)
          val arg3 = quotedArg(3)
          val arg4 = quotedArg(4)
          s"""val src = vector_raw_data($self)
val dest = vector_raw_data($arg1)
array_copy(src, $arg2, dest, $arg3, $arg4)"""
        }

      parallelize as ParallelCollectionBuffer(MInt, lookupOp("vector_raw_alloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("update"), lookupOp("vector_set_length"), lookupOp("vector_appendable"), lookupOp("append"), lookupOp("vector_copy"))
    }

    ()
  }
}

