/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all UndirectedGraph operations.  Glues 
togther all structures and declares UndirectedGraph operations visible
to user. Inherits from Graph.scala

Data is stored the same as in a directed graph but we only store
out edges. In an undirected graph in=out edges.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait UndirectedGraphOps{
  this: OptiGraphDSL =>

  def importUndirectedGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NeighborView = lookupTpe("NeighborView")
    val NodeIdView = lookupTpe("NodeIdView")
    
    //Actual UndirectedGraph declaration
    val UndirectedGraph = tpe("UndirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val Tuple2 = lookupTpe("Tup2")

    data(UndirectedGraph,("_numNodes",MInt),("_externalIDs",MArray(MInt)),("_nodes",MArray(MInt)),("_edges",MArray(MInt)),("_weights",MArray(MDouble)))
    static(UndirectedGraph)("apply", Nil, (MethodSignature(List(("count",MInt),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt))), UndirectedGraph))) implements allocates(UndirectedGraph,{
      val count = quotedArg("count")
      s"""$count"""
    },{
      val exID = quotedArg("exID")
      s"""$exID"""
    },{
      val outNodes = quotedArg("outNodes")
      s"""$outNodes"""
    },{
      s"""outEdges"""
    },{
      s"""array_empty[Double](unit(0))"""
    })
    static(UndirectedGraph)("apply", Nil, (MethodSignature(List(("count",MInt),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt)),("weights",MArray(MDouble))), UndirectedGraph))) implements allocates(UndirectedGraph,{
  val count = quotedArg("count")
  s"""$count"""
},{
  val exID = quotedArg("exID")
  s"""$exID"""
},{
  val outNodes = quotedArg("outNodes")
  s"""$outNodes"""
},{
  s"""outEdges"""
},{
  s"""weights"""
})

    val UndirectedGraphOps = withTpe(UndirectedGraph)     
    UndirectedGraphOps{
      infix ("numEdges")(Nil :: MInt) implements composite {
  val self = quotedArg("self")
  s"""array_length($self.getCSREdges)"""
}

      //UndirectedGraph directed or not?
      infix ("isDirected") (Nil :: MBoolean) implements composite {
  s"""false"""
}

      infix ("commonNeighbors") ((Node,Node) :: MLong) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""val neigh1 = $self.neighbors($arg1)
val neigh2 = $self.neighbors($arg2)
val max = if($arg1 > $arg2) $arg2.id else $arg1.id

neigh1.intersectInRange(neigh2,max)"""
        }

      infix ("sumDownNeighbors") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",Node==>R)),R), TNumeric(R), addTpePars=R) implements composite {
          val self = quotedArg("self")
          s"""sumOverNeighborsC($self.outNeighbors(n))(data){e => (level(e.id)==(level(n.id)+1))}"""
        }

      infix ("sumUpNeighbors") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",Node==>R)),R), TNumeric(R), addTpePars=R) implements composite {
          val self = quotedArg("self")
          s"""sumOverNeighborsC($self.inNeighbors(n))(data){e => (level(e.id)==(level(n.id)-1))}"""
        }

      infix ("totalWeight") (Nil :: MDouble) implements composite {
          val self = quotedArg("self")
          s"""array_reduce[Double](edge_weights($self),{(a,b) => a+b},unit(0d))"""
        }
      infix ("weightedDegree") (Node :: MDouble) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""get_edge_weights($self,$arg1).reduce({(a,b)=>a+b})"""
        }
      infix ("numSelfLoops") (Node :: MDouble) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""var degree = 0d
var i = 0
val start = node_apply($self,$arg1.id)
val neighbors = $self.neighbors($arg1)
while(i < neighbors.length){
  if(neighbors(i) == $arg1.id){
    degree = array_apply(edge_weights($self),start+i)
    i = neighbors.length
  }
  i += 1
}
degree"""
        }
      infix ("degree") (Node :: MInt) implements composite {
          val arg1 = quotedArg(1)
          val self = quotedArg("self")
          s"""val end  = if( ($arg1.id+1) < array_length($self.getCSRNodes) ) node_apply($self,($arg1.id+1)) 
  else array_length($self.getCSREdges)
end - node_apply($self,$arg1.id)"""
        }
      infix ("outDegree") (Node :: MInt) implements composite {
          val arg1 = quotedArg(1)
          val self = quotedArg("self")
          s"""val end  = if( ($arg1.id+1) < array_length($self.getCSRNodes) ) node_apply($self,($arg1.id+1)) 
  else array_length($self.getCSREdges)
end - node_apply($self,$arg1.id)"""
        }
      infix ("getNeighborsAndWeights") (Node :: Tuple2(NeighborView(MInt),NeighborView(MDouble))) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val start = node_apply($self,$arg1.id)
val end = if( ($arg1.id+1) < array_length($self.getCSRNodes) ) node_apply($self,($arg1.id+1))
  else array_length($self.getCSREdges)
pack(NeighborView[Int]($self.getCSREdges,start,end-start),NeighborView[Double](edge_weights($self),start,end-start))"""
        }
      infix ("inDegree") (Node :: MInt) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self.outDegree($arg1)"""
      }
      infix ("outNeighbors") (Node :: NeighborView(MInt)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""get_neighbors($self,$arg1)"""
      } 
      infix ("inNeighbors") (Node :: NeighborView(MInt)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""get_neighbors($self,$arg1)"""
      }
      infix ("neighbors") (Node :: NeighborView(MInt)) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""get_neighbors($self,$arg1)"""
      }
      compiler ("get_neighbors") (Node :: NeighborView(MInt)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val start = node_apply($self,$arg1.id)
val end = if( ($arg1.id+1) < array_length($self.getCSRNodes) ) node_apply($self,($arg1.id+1))
  else array_length($self.getCSREdges)
NeighborView[Int]($self.getCSREdges,start,end-start)"""
        }
      compiler ("get_edge_weights") (Node :: NeighborView(MDouble)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val start = node_apply($self,$arg1.id)
val end = if( ($arg1.id+1) < array_length($self.getCSRNodes) ) node_apply($self,($arg1.id+1))
  else array_length(edge_weights($self))
NeighborView[Double](edge_weights($self),start,end-start)"""
        }
      infix ("getCSREdgeWeights") (Nil :: MArray(MDouble)) implements getter(0, "_weights")
      compiler ("edge_weights") (Nil :: MArray(MDouble)) implements getter(0, "_weights")
      infix ("getCSRNodes") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
      compiler("node_apply")(MInt :: MInt) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_apply($self.getCSRNodes,$arg1)"""
      }
      infix ("getCSREdges") (Nil :: MArray(MInt)) implements getter(0, "_edges")
      compiler("edge_apply")(MInt :: MInt) implements composite {
      val self = quotedArg("self")
      val arg1 = quotedArg(1)
      s"""array_apply($self.getCSREdges,$arg1)"""
    }
    }
    addGraphCommonOps(UndirectedGraph) 
  } 
}
