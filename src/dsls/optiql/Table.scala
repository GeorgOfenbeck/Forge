package ppl.dsl.forge
package dsls
package optiql

import core.{ForgeApplication,ForgeApplicationRunner}

trait TableOps {
  this: OptiQLDSL =>

  def importTableOps() {
    val A = tpePar("A") //input types
    val B = tpePar("B")
    val C = tpePar("C")
    val K = tpePar("K") //key tpe
    val V = tpePar("V") //value tpe
    val R = tpePar("R") //result tpe

    val Table = tpe("Table", A)

    //internal data format
    //TODO: this is equivalent to a DeliteArrayBuffer, we should be able to just use that
    data(Table, "size" -> MInt, "data" -> MArray(A))

    //static constructors
    static (Table) ("apply", A, MInt :: Table(A), effect = mutable) implements allocates(Table, {
      val arg1 = quotedArg(0)
      s"""$arg1"""
    }, {
      val arg1 = quotedArg(0)
      s"""array_empty[A]($arg1)"""
    })
    static (Table) ("apply", A, MArray(A) :: Table(A)) implements allocates(Table, {
      val arg1 = quotedArg(0)
      s"""array_length($arg1)"""
    }, {
      val arg1 = quotedArg(0)
      s"""$arg1"""
    })
    static (Table) ("apply", A, (MArray(A), MInt) :: Table(A)) implements allocates(Table, {
      val arg1 = quotedArg(1)
      s"""$arg1"""
    }, {
      val arg1 = quotedArg(0)
      s"""$arg1"""
    })
    static (Table) ("apply", A, varArgs(A) :: Table(A)) implements allocates(Table, {
      val arg1 = quotedArg(0)
      s"""unit($arg1.length)"""
    }, {
      val arg1 = quotedArg(0)
      s"""array_fromseq($arg1)"""
    })
    static (Table) ("range", Nil, (MInt, MInt) :: Table(MInt)) implements composite {
        val arg1 = quotedArg(1)
        val arg2 = quotedArg(0)
        s"""val a = array_fromfunction($arg1-$arg2, i => i + $arg2)
Table[Int](a)"""
      }

    static (Table) ("fromFile", A, CurriedMethodSignature(List(List(MString), List(MString ==> A)), Table(A))) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""val a = ForgeFileReader.readLines($arg1)($arg2)
Table[A](a)"""
      }

    static (Table) ("fromFile", A, ("path"->MString, "separator"->MString) :: Table(A)) implements composite {
        val path = quotedArg("path")
        val separator = quotedArg("separator")
        s"""val a = ForgeFileReader.readLines($path)(line => createRecord[A](array_string_split(line, $separator, -1)))
Table[A](a)"""
      }

    static (Table) ("fromString", A, ("data"->MString, "rowSeparator"->MString, "columnSeparator"->MString) :: Table(A)) implements composite {
        val data = quotedArg("data")
        val rowSeparator = quotedArg("rowSeparator")
        val columnSeparator = quotedArg("columnSeparator")
        s"""val a = array_map(array_string_split($data, $rowSeparator), (s:Rep[String]) => createRecord[A](array_string_split(s, $columnSeparator, -1)))
Table[A](a)"""
      }

    //(K,V) pairs for Tables
    val Tuple2 = lookupTpe("Tup2")
    fimplicit (Tuple2) ("pair_to_value", (K,V), Tuple2(K,Table(V)) :: Table(V)) implements composite {
      val arg1 = quotedArg(0)
      s"""tup2__2($arg1)"""
    } //TODO: this isn't kicking in
    infix (Tuple2) ("key", (K,V), Tuple2(K,Table(V)) :: K) implements composite {
      val arg1 = quotedArg(0)
      s"""tup2__1($arg1)"""
    }
    infix (Tuple2) ("values", (K,V), Tuple2(K,Table(V)) :: Table(V)) implements composite {
  val arg1 = quotedArg(0)
  s"""tup2__2($arg1)"""
}


    //sorting convenience method on Orderables
    direct (Table) ("asc", (A,K), "keySelector" -> (A ==> K) :: ((A,A) ==> MInt), TOrdering(K)) implements composite {
        s"""(a,b) => compareHackImpl(keySelector(a), keySelector(b))"""
      }

    direct (Table) ("desc", (A,K), "keySelector" -> (A ==> K) :: ((A,A) ==> MInt), TOrdering(K)) implements composite {
        s"""(a,b) => compareHackImpl(keySelector(b), keySelector(a))"""
      }


    val TableOps = withTpe (Table)
    TableOps {

      // bulk collect ops
      infix ("Select") ("selector" -> (A ==> R) :: Table(R), addTpePars = R) implements map((A,R), 0, {
        val selector = quotedArg("selector")
        s"""$selector"""
      })
      infix ("SelectMany") ("selector" -> (A ==> Table(R)) :: Table(R), addTpePars = R) implements flatMap((A,R), 0, {
        val selector = quotedArg("selector")
        s"""$selector"""
      })
      infix ("Where") ("predicate" -> (A ==> MBoolean) :: Table(A)) implements filter((A,A), 0, {
  val predicate = quotedArg("predicate")
  s"""$predicate"""
}, {
  s"""e => e"""
})

      //bulk bucketReduce ops
      //TODO: composite op
      //TODO: what are the semantics of GroupBy? does it return a Map or a Table(Pairs), or ...?
      infix ("GroupBy") ("keySelector" -> (A ==> K) :: Table(Tuple2(K,Table(A))), addTpePars = K) implements single {
          val self = quotedArg("self")
          val keySelector = quotedArg("keySelector")
          s"""groupByHackImpl($self, $keySelector)"""
        }

      infix ("Distinct") ("keySelector" -> (A ==> K) :: Table(A), addTpePars = K) implements composite {
          val self = quotedArg("self")
          val keySelector = quotedArg("keySelector")
          s"""val map = groupByReduceOp($self, $keySelector, (e:Rep[A]) => e, (a:Rep[A],b:Rep[A]) => a, (e:Rep[A]) => unit(true))
Table[A](fhashmap_values(map))"""
        }
      infix ("Distinct") (Nil :: Table(A)) implements composite {
  val arg1 = quotedArg(0)
  s"""$arg1.Distinct((k:Rep[A]) => k)"""
}

      //bulk reduce ops
      infix ("Sum") ("selector" -> (A ==> R) :: R, addTpePars = R withBound TNumeric) implements mapReduce((A,R), 0, {
  val selector = quotedArg("selector")
  s"""$selector"""
}, {
  s"""zeroType[R]"""
}, {
  s"""(a,b) => a + b"""
})

      //TODO: composite op
      infix ("Average") ("selector" -> (A ==> R) :: R, addTpePars = R withBound TNumeric withBound TFractional) implements single {
          val self = quotedArg("self")
          val selector = quotedArg("selector")
          s"""$self.Sum($selector) / upgradeInt[R](table_count($self))"""
        }

      infix ("Max") ("selector" -> (A ==> R) :: R, addTpePars = R withBound TOrdering) implements mapReduce((A,R), 0, {
        val selector = quotedArg("selector")
        s"""$selector"""
      }, {
        s"""minValue[R]"""
      }, {
        s"""(a,b) => a max b"""
      })
      infix ("Min") ("selector" -> (A ==> R) :: R, addTpePars = R withBound TOrdering) implements mapReduce((A,R), 0, {
  val selector = quotedArg("selector")
  s"""$selector"""
}, {
  s"""maxValue[R]"""
}, {
  s"""(a,b) => a min b"""
})

      infix ("Count") ("predicate" -> (A ==> MBoolean) :: MInt) implements mapReduce((A,MInt), 0, {
        s"""e => unit(1)"""
      }, {
        s"""unit(0)"""
      }, {
        s"""(a,b) => a + b"""
      }, Some({
        val predicate = quotedArg("predicate")
        s"""$predicate"""
      }))
      //TODO: these create a scalac typer bug in mirroring definition (see Ops.scala:634)
      //infix ("First") ("predicate" -> (A ==> MBoolean) :: A) implements mapReduce((A,A), 0, ${e => e}, ${zeroType[A]}, ${(a,b) => a}, Some(${$predicate}))
      //infix ("Last") ("predicate" -> (A ==> MBoolean) :: A) implements mapReduce((A,A), 0, ${e => e}, ${zeroType[A]}, ${(a,b) => b}, Some(${$predicate}))

      //simple ops that can also be implemented as a reduce
      infix ("Count") (Nil :: MInt) implements single {
        val self = quotedArg("self")
        s"""table_size($self)"""
      }
      infix ("First") (Nil :: A) implements single {
        val self = quotedArg("self")
        s"""table_apply($self, 0)"""
      }
      infix ("Last") (Nil :: A) implements single {
  val self = quotedArg("self")
  s"""table_apply($self, table_size($self)-1)"""
}

      //sorting ops
      infix ("OrderBy") ("sorts" -> varArgs((A,A) ==> MInt) :: Table(A)) implements composite {
          val sorts = quotedArg("sorts")
          val self = quotedArg("self")
          s"""Predef.assert($sorts.length > 0, "[OPTIQL ERROR]: OrderBy requires at least one argument")
val buf = new scala.collection.mutable.ArrayBuffer[(Rep[A],Rep[A])=>Rep[Int]]
buf += $sorts(0)
for (i <- 1 until $sorts.length) {
  val compound = (a:Rep[A],b:Rep[A]) => {
    val prev = buf(i-1)(a,b)
    if (prev == unit(0)) $sorts(i)(a,b)
    else prev
  }
  buf += compound
}

sortHackImpl($self, buf(buf.size-1))"""
        }
      noInfixList :::= List("OrderBy") //varArgs(Function) only resolves properly with implicits; move into Forge codegen logic?


      //multi-table ops
      infix ("Join") (CurriedMethodSignature(List(List("t2"->Table(B)), List("k1"->(A ==> K), "k2"->(B ==> K)), List("result"->((A,B) ==> R))), Table(R)), addTpePars = (B,K,R)) implements composite {
          val self = quotedArg("self")
          val k1 = quotedArg("k1")
          val t2 = quotedArg("t2")
          val k2 = quotedArg("k2")
          val result = quotedArg("result")
          s"""join2($self, $k1, $t2, $k2, $result)"""
        }

      compiler ("join2")(("k1" -> (A ==> K), "t2" -> Table(B), "k2" -> (B ==> K), "result" -> ((A,B) ==> R)) :: Table(R), addTpePars = (B,K,R)) implements composite {
          val self = quotedArg("self")
          val k1 = quotedArg("k1")
          val t2 = quotedArg("t2")
          val k2 = quotedArg("k2")
          val result = quotedArg("result")
          s"""val grouped = array_buffer_groupBy(array_buffer_new_imm(table_raw_data($self), array_length(table_raw_data($self))), $k1) 
val empty = Table(array_empty_imm[R](unit(0))) 
$t2.SelectMany(e2 => {
  if (fhashmap_contains(grouped, $k2(e2))) {
    val buf = fhashmap_get(grouped, $k2(e2))
    Table(array_buffer_unsafe_result(buf), array_buffer_length(buf)).Select(e1 => $result(e1,e2))
  }
  else empty
})"""
        }

      /*compiler ("join3")(("t1" -> Table(A), "k1" -> (A ==> K), "t2" -> Table(B), "k2" -> (B ==> K), "t3" -> Table(C), "k3" -> (C ==> K), "result" -> ((A,B,C) ==> R)) :: Table(R), addTpePars = (B,C,K,R)) implements composite ${
        ???
      }*/

      infix ("toArray") (Nil :: MArray(A)) implements composite {
  val self = quotedArg("self")
  s"""array_fromfunction($self.size, i => $self.apply(i))"""
}

      //printing ops
      //see extern files, currently manual Scala implementations

      //internal transformed ops
      compiler("groupByReduce")(("keySelector" -> (A ==> K), "valueSelector" -> (A ==> V), "reducer" -> ((V,V) ==> V), "condition" -> (A ==> MBoolean)) :: Table(V), addTpePars = (K,V)) implements composite {
          val self = quotedArg("self")
          val keySelector = quotedArg("keySelector")
          val valueSelector = quotedArg("valueSelector")
          val reducer = quotedArg("reducer")
          val condition = quotedArg("condition")
          s"""val map = groupByReduceOp($self, $keySelector, $valueSelector, $reducer, $condition)
Table[V](fhashmap_values(map))"""
        }
      compiler("groupByReduceOp")(("keySelector" -> (A ==> K), "valueSelector" -> (A ==> V), "reducer" -> ((V,V) ==> V), "condition" -> (A ==> MBoolean)) :: MHashMap(K,V), addTpePars = (K,V)) implements groupByReduce((A,K,V), 0, {
        val keySelector = quotedArg("keySelector")
        s"""$keySelector"""
      }, {
        val valueSelector = quotedArg("valueSelector")
        s"""$valueSelector"""
      }, {
        s"""zeroType[V]"""
      }, {
        val reducer = quotedArg("reducer")
        s"""$reducer"""
      }, Some({
        s"""condition"""
      }))
      compiler("bulkDivide") (("counts" -> Table(MInt), "avgFunc" -> ((A,MInt) ==> A)) :: Table(A)) implements zip((A,MInt,A), (0,1), {
  val avgFunc = quotedArg("avgFunc")
  s"""$avgFunc"""
})


      //methods needed to implement Table as a Delite ParallelCollectionBufer

      //accessors
      infix ("apply") (MInt :: A) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""table_apply_internal($self, $arg1)"""
      }
      infix ("size") (Nil :: MInt) implements composite {
  val self = quotedArg("self")
  s"""table_size_internal($self)"""
}

      compiler ("table_raw_data") (Nil :: MArray(A)) implements getter(0, "data")
      compiler ("table_size_internal") (Nil :: MInt) implements getter(0, "size")
      compiler ("table_apply_internal") (MInt :: A) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""array_apply(table_raw_data($self), $arg1)"""
        }

      //mutators
      compiler ("table_set_raw_data") (MArray(A) :: MUnit, effect = write(0)) implements setter(0, "data", quotedArg(1))
      compiler ("table_set_size") (MInt :: MUnit, effect = write(0)) implements setter(0, "size", quotedArg(1))
      compiler ("table_update") (("i"->MInt,"e"->A) :: MUnit, effect = write(0)) implements composite {
          val self = quotedArg("self")
          val i = quotedArg("i")
          val e = quotedArg("e")
          s"""array_update(table_raw_data($self), $i, $e)"""
        }

      compiler ("table_alloc") (MInt :: Table(R), addTpePars = R, effect = mutable) implements composite {
          val arg1 = quotedArg(1)
          s"""Table[R]($arg1)"""
        }

      infix ("insert") ((MInt,A) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""table_insertspace($self,$arg1,1)
table_update($self, $arg1, $arg2)"""
        }

      infix ("append") (A :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""table_insert($self, table_size($self), $arg1)"""
        }

      compiler ("table_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val len = quotedArg("len")
          val pos = quotedArg("pos")
          s"""table_ensureextra($self,$len)
val data = table_raw_data($self)
array_copy(data,$pos,data,$pos+$len,table_size($self)-$pos)
table_set_size($self,table_size($self)+$len)"""
        }

      compiler ("table_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val extra = quotedArg("extra")
          s"""val data = table_raw_data($self)
if (array_length(data) - table_size($self) < $extra) {
  table_realloc($self, table_size($self)+$extra)
}"""
        }

      compiler ("table_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val minLen = quotedArg("minLen")
          s"""val data = table_raw_data($self)
var n = unit(4) max (array_length(data)*2)
while (n < $minLen) n = n*2
val d = array_empty[A](n)
array_copy(data, 0, d, 0, table_size($self))
table_set_raw_data($self, d.unsafeImmutable)"""
        }

      compiler ("table_appendable") ((MInt,A) :: MBoolean) implements single("true")
      compiler ("table_dc_append") ((MInt,A) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(2)
          s"""table_append($self, $arg1)"""
        }
      compiler ("table_copy") ((MInt,Table(A),MInt,MInt) :: MUnit, effect = write(2)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(2)
          val arg2 = quotedArg(1)
          val arg3 = quotedArg(3)
          val arg4 = quotedArg(4)
          s"""val src = table_raw_data($self)
val dest = table_raw_data($arg1)
array_copy(src, $arg2, dest, $arg3, $arg4)"""
        }

      parallelize as ParallelCollectionBuffer(A, lookupOp("table_alloc"), lookupOp("table_size_internal"), lookupOp("table_apply_internal"), lookupOp("table_update"), lookupOp("table_set_size"), lookupOp("table_appendable"), lookupOp("table_dc_append"), lookupOp("table_copy"))
    }
  }
}
