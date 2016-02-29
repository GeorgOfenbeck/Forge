/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Stores data asscoicated with nodes in an array
buffer indexed by internal node IDs
*///////////////////////////////////////////////////////////////

package ppl.dsl.forge
package dsls
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait NodeDataOps {
  this: OptiGraphDSL =>
  def importNodeDataOps() {
    val Tuple2 = lookupTpe("Tup2")
    val T = tpePar("T")
    val K = tpePar("K")
    val V = tpePar("V")
    val R = tpePar("R")
    val Node
     = lookupTpe("Node")
    val NodeData = tpe("NodeData", T)

    data(NodeData,("_data",MArrayBuffer(T)))
    static(NodeData)("apply", T, MInt :: NodeData(T)) implements allocates(NodeData,{
      val arg1 = quotedArg(0)
      s"""array_buffer_strict_empty[T]($arg1)"""
    })
    static(NodeData)("apply", T, MArray(T) :: NodeData(T)) implements allocates(NodeData,{
      val arg1 = quotedArg(0)
      s"""array_buffer_new_imm($arg1, array_length($arg1))"""
    })
    static(NodeData)("apply", T, MArrayBuffer(T) :: NodeData(T)) implements allocates(NodeData,{
      val arg1 = quotedArg(0)
      s"""array_buffer_immutable($arg1)"""
    })
    static(NodeData)("fromFunction", T, (MInt,(MInt ==> T)) :: NodeData(T)) implements allocates(NodeData,{
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""array_buffer_new_imm(array_fromfunction($arg1,$arg2), $arg1)"""
})

    val NodeDataOps = withTpe(NodeData)
    NodeDataOps{
      //////////////basic accessors//////////////////////////////
      infix("apply")(MInt :: T) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_buffer_apply(nd_raw_data($self),$arg1)"""
      }
      infix("apply")(Node :: T) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_buffer_apply(nd_raw_data($self),$arg1.id)"""
      }
      infix("update")( (("id",MInt),("n",T)) :: MUnit, effect=write(0)) implements composite {
        val self = quotedArg("self")
        val id = quotedArg("id")
        val n = quotedArg("n")
        s"""array_buffer_update(nd_raw_data($self),$id,$n)"""
      }
      infix ("length")(Nil :: MInt) implements single {
        val self = quotedArg("self")
        s"""array_buffer_length(nd_raw_data($self))"""
      }
      infix ("append") (T :: MUnit, effect = write(0)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""nd_append($self,$self.length, $arg1)"""
      }
      //method to get an array of data to outside world
      infix ("getRawArray") (Nil :: MArray(T)) implements single {
        val self = quotedArg("self")
        s"""array_buffer_result(nd_raw_data($self))"""
      }
      infix ("getRawArrayBuffer") (Nil :: MArrayBuffer(T)) implements single {
  val self = quotedArg("self")
  s"""nd_raw_data($self)"""
}

      //allows arrays to be set to proper size, useful in file I/O not necessary after groupby
      infix("resize")(MInt :: MUnit, effect = write(0)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val data = nd_raw_data($self)
val d = array_buffer_empty[T]($arg1)
array_buffer_copy(data, 0, d, 0, $arg1)
nd_set_raw_data($self, d.unsafeImmutable)
nd_set_length($self,$arg1)"""
        }
      infix("concat")(NodeData(T) :: NodeData(T)) implements composite {
          val arg1 = quotedArg(0)
          val arg2 = quotedArg(1)
          s"""val result = array_empty[T]($arg1.length+$arg2.length)
array_copy($arg1.getRawArray,0,result,0,$arg1.length)
array_copy($arg2.getRawArray,0,result,$arg1.length,$arg2.length)
NodeData(result)"""
        }

      ///////////parallel operations////////////////////////////
      infix ("-") (NodeData(T) :: NodeData(T), TNumeric(T)) implements zip((T,T,T), (0,1), {
        s"""(a,b) => a-b"""
      })
      infix ("+") (NodeData(T) :: NodeData(T), TNumeric(T)) implements zip((T,T,T), (0,1), {
        s"""(a,b) => a+b"""
      })
      infix ("pack") (NodeData(T) :: NodeData(Tuple2(T,T))) implements zip( (T,T,Tuple2(T,T)), (0,1), {
        s"""(a,b) => pack(a,b)"""
      })
      infix ("map") ((T ==> R) :: NodeData(R), addTpePars=R) implements map((T,R), 0, {
        val arg1 = quotedArg(1)
        s"""e => $arg1(e)"""
      })
      infix ("flatMap") ((T ==> NodeData(R)) :: NodeData(R), addTpePars = R) implements flatMap((T,R), 0, {
        val arg1 = quotedArg(1)
        s"""e => $arg1(e)"""
      })
      infix ("filter") ( ((T ==> MBoolean),(T ==> R)) :: NodeData(R), addTpePars = R) implements filter((T,R), 0, {
        val arg1 = quotedArg(1)
        s"""w => $arg1(w)"""
      }, {
        val arg1 = quotedArg(2)
        s"""e => $arg1(e)"""
      })
      infix ("foreach") ((T ==> MUnit) :: MUnit, effect = simple) implements foreach(T, 0, {
        val arg1 = quotedArg(1)
        s"""e => $arg1(e)"""
      })
      infix ("reduce") (((T,T) ==> T) :: T, TNumeric(T)) implements reduce(T, 0, {
        s"""numeric_zero[T]"""
      }, {
        val arg1 = quotedArg(1)
        s"""(a,b) => $arg1(a,b)"""
      })
      infix ("reduceNested") ( (((T,T) ==> T),R):: T,addTpePars=R) implements reduce(T, 0, {
        val arg1 = quotedArg(2)
        s"""$arg1.asInstanceOf[Rep[T]]"""
      }, {
        val arg1 = quotedArg(1)
        s"""(a,b) => $arg1(a,b)"""
      })
      infix ("groupBy") ((T ==> K,T ==> V) :: MHashMap(K, MArrayBuffer(V)), addTpePars = (K,V)) implements groupBy((T,K,V), 0, {
        val arg1 = quotedArg(1)
        s"""e => $arg1(e)"""
      }, {
        val arg1 = quotedArg(2)
        s"""e => $arg1(e)"""
      })
      infix ("groupByReduce") ((T ==> K,T ==> V,(V,V) ==> V) :: MHashMap(K, V), TNumeric(V), addTpePars = (K,V)) implements groupByReduce((T,K,V), 0, {
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
      infix ("mapreduce") ( (T ==> R,(R,R) ==> R, T==>MBoolean) :: R, TNumeric(R), addTpePars=(R)) implements mapReduce((T,R), 0, {
        val arg1 = quotedArg(1)
        s"""e => $arg1(e)"""
      }, {
        s"""numeric_zero[R]"""
      }, {
        val arg1 = quotedArg(2)
        s"""(a,b) => $arg1(a,b)"""
      }, Some({
        val arg1 = quotedArg(3)
        s"""c => $arg1(c)"""
      }))
      infix ("distinct") (Nil :: NodeData(T)) implements composite {
        val arg1 = quotedArg(0)
        s"""NodeData(fhashmap_keys($arg1.groupByReduce[T,Int](e => e, e=>0,(a,b)=>0)))"""
      }
      infix("sort")(Nil :: NodeData(T),TNumeric(T)) implements composite {
        val self = quotedArg("self")
        s"""NodeData(array_sort($self.getRawArray))"""
      }
      infix ("sortBy") ((MInt ==> R) :: NodeData(T), TOrdering(R), addTpePars=R) implements composite {
            val self = quotedArg("self")
            val arg1 = quotedArg(1)
            s"""NodeData[Int](array_sortIndices($self.length,$arg1)).map[T](i => $self(i))"""
          }
      infix ("sortIndicesBy") ((MInt ==> R) :: NodeData(MInt), TOrdering(R), addTpePars=R) implements single {
            val self = quotedArg("self")
            val arg1 = quotedArg(1)
            s"""NodeData[Int](array_sortIndices($self.length,$arg1))"""
          }

      /////////////////////////debug operations (print serial & parallel)///////////////////////
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(T, 0, {
        s"""a => println("NodeData: " + a)"""
      })
      infix ("forindicies") ((MInt ==> MUnit) :: MUnit, effect = simple) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""array_buffer_forIndices($self.getRawArrayBuffer,$arg1)"""
        }
      infix ("forloop") ((T ==> MUnit) :: MUnit, effect = simple) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""var i = 0
while(i<$self.length){
  $arg1($self(i))
  i = i+1
}"""
        }
      infix ("print") (Nil :: MUnit, effect = simple) implements composite {
          val self = quotedArg("self")
          s"""var i = 0
while(i<$self.length){
  println("NodeData -- Index: " + i + " Data: " + $self(i))
  i = i+1
}"""
        }

      ///////////////methods for parallel collection buffer declaration/////////////////////////
      compiler ("nd_raw_data") (Nil :: MArrayBuffer(T)) implements getter(0, "_data")
      compiler("nd_raw_alloc")(MInt :: NodeData(R), addTpePars = R, effect=mutable) implements composite {
        val arg1 = quotedArg(1)
        s"""NodeData[R]($arg1)"""
      }
      compiler ("nd_apply") (MInt :: T) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_buffer_apply(nd_raw_data($self), $arg1)"""
      }
      compiler("nd_update")( (("id",MInt),("n",T)) :: MUnit, effect=write(0)) implements composite {
        val self = quotedArg("self")
        val id = quotedArg("id")
        val n = quotedArg("n")
        s"""array_buffer_update(nd_raw_data($self),$id,$n)"""
      }
      compiler ("nd_set_length")(MInt :: MUnit, effect = write(0)) implements single {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_buffer_set_length(nd_raw_data($self),$arg1)"""
      }
      compiler ("nd_set_raw_data") (MArrayBuffer(T) :: MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))
      compiler ("nd_appendable") ((MInt,T) :: MBoolean) implements single("true")
      compiler ("nd_append") ((MInt,T) :: MUnit, effect = write(0)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(2)
        s"""array_buffer_append(nd_raw_data($self),$arg1)"""
      }
      compiler("nd_copy") ((MInt,NodeData(T),MInt,MInt) :: MUnit, effect = write(2)) implements single {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  val arg2 = quotedArg(2)
  val arg3 = quotedArg(3)
  val arg4 = quotedArg(4)
  s"""array_buffer_copy(nd_raw_data($self),$arg1,nd_raw_data($arg2),$arg3,$arg4)"""
}

      parallelize as ParallelCollectionBuffer(T,lookupOp("nd_raw_alloc"),lookupOp("length"),lookupOp("nd_apply"),lookupOp("nd_update"),lookupOp("nd_set_length"),lookupOp("nd_appendable"),lookupOp("nd_append"),lookupOp("nd_copy"))
    }
    compiler (NodeData) ("nd_fake_alloc", R, Nil :: NodeData(R)) implements single {
    s"""NodeData[R](0)"""
  }
  }
}
