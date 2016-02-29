/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all DirectedGraph operations.  Glues 
togther all structures and declares DirectedGraph operations visible
to user.  Inherits graph common ups.

Data is stored as follows.  Internal ID #'s map to external ID's
in the hashmap that is stored.  Internal ID's are 0 to # of nodes
so that data can be mapped in an array effeciently.  No restrictions
on external ID"s except they cannot be 0.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait DirectedGraphOps{
  this: OptiGraphDSL =>

  def importDirectedGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NeighborView = lookupTpe("NeighborView")
    val NodeIdView = lookupTpe("NodeIdView")
    //Actual DirectedGraph declaration
    val DirectedGraph = tpe("DirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    data(DirectedGraph,("_numNodes",MInt),("_externalIDs",MArray(MInt)),("_outNodes",MArray(MInt)),("_outEdges",MArray(MInt)),("_inNodes",MArray(MInt)),("_inEdges",MArray(MInt))) 
    static(DirectedGraph)("apply", Nil, (MethodSignature(List(("numNodes",MInt),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt)),("inNodes",MArray(MInt)),("inEdges",MArray(MInt))), DirectedGraph))) implements allocates(DirectedGraph,{
  val numNodes = quotedArg("numNodes")
  s"""$numNodes"""
},{
  val exID = quotedArg("exID")
  s"""$exID"""
}, {
  val outNodes = quotedArg("outNodes")
  s"""$outNodes"""
}, {
  s"""outEdges"""
},{
  val inNodes = quotedArg("inNodes")
  s"""$inNodes"""
},{
  val inEdges = quotedArg("inEdges")
  s"""$inEdges"""
})

    val DirectedGraphOps = withTpe(DirectedGraph)     
    DirectedGraphOps{
      infix ("numEdges")(Nil :: MInt) implements composite {
        val self = quotedArg("self")
        s"""array_length(in_edge_raw_data($self)) + array_length(out_edge_raw_data($self))"""
      }
      infix ("isDirected") (Nil :: MBoolean) implements composite {
  s"""true"""
}

      //get out neighbors
      infix ("outNeighbors") (Node :: NeighborView(MInt)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val start = out_node_apply($self,$arg1.id)
val end = if( ($arg1.id+1) < array_length(out_node_raw_data($self)) ) out_node_apply($self,($arg1.id+1))
  else array_length(out_edge_raw_data($self))
NeighborView[Int](out_edge_raw_data($self),start,end-start)"""
        }
      //get in neighbors   
      infix ("inNeighbors") (Node :: NeighborView(MInt)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val start = in_node_apply($self,$arg1.id)
val end = if( ($arg1.id+1) < array_length(in_node_raw_data($self)) ) in_node_apply($self,($arg1.id+1)) 
    else array_length(in_edge_raw_data($self)) 
NeighborView[Int](in_edge_raw_data($self),start,end-start)"""
        }
      infix ("outDegree") (Node :: MInt) implements composite {
          val arg1 = quotedArg(1)
          val self = quotedArg("self")
          s"""val end  = if( ($arg1.id+1) < array_length(out_node_raw_data($self)) ) out_node_apply($self,($arg1.id+1)) 
  else array_length(out_edge_raw_data($self))
end - out_node_apply($self,$arg1.id)"""
        }
      infix ("inDegree") (Node :: MInt) implements composite {
          val arg1 = quotedArg(1)
          val self = quotedArg("self")
          s"""val end = if( ($arg1.id+1) < array_length(in_node_raw_data($self)) ) in_node_apply($self,($arg1.id+1)) 
    else array_length(in_edge_raw_data($self))
end - in_node_apply($self,$arg1.id)"""
        }
      infix ("sumDownNeighbors") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",Node==>R)),R), TFractional(R), addTpePars=R) implements composite {
          val self = quotedArg("self")
          s"""sumOverNeighborsC($self.outNeighbors(n))(data){e => (level(e.id)==(level(n.id)+1))}"""
        }
      infix ("sumUpNeighbors") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",Node==>R)),R), TFractional(R), addTpePars=R) implements composite {
          val self = quotedArg("self")
          s"""sumOverNeighborsC($self.inNeighbors(n))(data){e => level(e.id)==(level(n.id)-1)}"""
        }
      //Input node ids
      infix ("hasEdge") ((MInt,MInt) :: MBoolean) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        val arg2 = quotedArg(2)
        s"""$self.hasEdge(Node($arg1),Node($arg2))"""
      }
      infix ("hasEdge") ((Node,Node) :: MBoolean) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""val inNeighbors = NodeData($self.inNeighbors($arg1).getRawArray).groupByReduce[Int,Int](e => e, e => e, (a,b) => a)
val outNeighbors = NodeData($self.outNeighbors($arg1).getRawArray).groupByReduce[Int,Int](e => e, e => e, (a,b) => a)
if(fhashmap_contains[Int,Int](inNeighbors,$arg2.id) || fhashmap_contains[Int,Int](outNeighbors,$arg2.id)) true 
else false"""
        }
      //Out Node Accessors
      compiler ("out_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_outNodes")
      compiler("out_node_apply")(MInt :: MInt) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_apply(out_node_raw_data($self),$arg1)"""
      }
      compiler ("out_edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_outEdges")
      compiler("out_edge_apply")(MInt :: MInt) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""array_apply(out_edge_raw_data($self),$arg1)"""
}

      //In Node Accessors
      compiler ("in_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_inNodes")
      compiler("in_node_apply")(MInt :: MInt) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_apply(in_node_raw_data($self),$arg1)"""
      }
      compiler ("in_edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_inEdges")
      compiler("in_edge_apply")(MInt :: MInt) implements composite {
      val self = quotedArg("self")
      val arg1 = quotedArg(1)
      s"""array_apply(in_edge_raw_data($self),$arg1)"""
    }
    }
    addGraphCommonOps(DirectedGraph)
  } 
}
