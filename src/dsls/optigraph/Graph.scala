/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all Graph operations.  Glues 
togther all structures and declares Graph operations visible
to user.

Common operations for both directed and undirected graphs.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait GraphOps{
  this: OptiGraphDSL =>

  def addGraphCommonOps(g: Rep[DSLType]) {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NeighborView = lookupTpe("NeighborView")
    val NodeIdView = lookupTpe("NodeIdView")

    //Actual Graph declaration
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))
    val Graph = g
    val GraphCommonOps = withTpe(Graph)
    GraphCommonOps{
      infix ("numNodes")(Nil :: MInt) implements getter(0,"_numNodes")

      infix ("nodes")(Nil :: NodeIdView) implements composite {
  val self = quotedArg("self")
  s"""NodeIdView($self.numNodes)"""
}

      //given an ID return a node
      infix("getNodeFromID")(MInt :: Node) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val result = NodeIdView($self.numNodes).mapreduce[Int]( i => i, (a,b) => a+b, i => $self.getExternalID(Node(i))==$arg1)
if(result >= $self.numNodes() || result < 0) fatal("ERROR. ID: " + $arg1 + " does not exist in this UndirectedGraph!")
Node(result)"""
        }
      infix ("foreachNode") ((Node ==> MUnit) :: MUnit, effect = simple) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""NodeData(array_fromfunction($self.numNodes,{n => n})).foreach{ i =>
  $arg1(Node(i))
}"""
        }
      infix("mapNodes")( (Node==>R) :: NodeData(R), addTpePars=R) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""NodeData[R](array_fromfunction($self.numNodes,{n => $arg1(Node(n))}))"""
        }

      infix ("getExternalIDs") (Nil :: MArray(MInt)) implements getter(0, "_externalIDs")
      infix ("getExternalID") (Node :: MInt) implements single {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_apply($self.getExternalIDs,$arg1.id)"""
      }
      //perform BF traversal
      infix ("inBFOrder") ( CurriedMethodSignature(List(Node,((Node,NodeData(R),NodeData(MInt)) ==> R),((Node,NodeData(R),NodeData(R),NodeData(MInt)) ==> R)),NodeData(R)), TFractional(R), addTpePars=R, effect=simple) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          val arg3 = quotedArg(3)
          s"""val levelArray = NodeData[Int]($self.numNodes)
val bitMap = AtomicIntArray($self.numNodes)
val nodes = NodeIdView($self.numNodes) 
val forwardComp = NodeData[R]($self.numNodes)
val reverseComp = NodeData[R]($self.numNodes)

levelArray($arg1.id) = 1
set(bitMap,$arg1.id,1)



var finished = false

var level = 1
 
while(!finished){
  finished = true
  nodes.foreach{n =>  
    if(levelArray(n) == level){
      val neighbor = $self.outNeighbors(Node(n))
      neighbor.foreach{nghbr =>
if(testAtomic(bitMap,nghbr,0)){
  if(testAndSetAtomic(bitMap,nghbr,0,1)){
    levelArray(nghbr) = level+1
    finished = false
      }}}
      forwardComp(n) = $arg2(Node(n),forwardComp,levelArray)
    }
  }
  level += 1
}

val rBFS = true

while( level>=1 ){
  nodes.foreach{n =>
    if(levelArray(n) == level){
      reverseComp(n) = $arg3(Node(n),forwardComp,reverseComp,levelArray)
    }
  }
  level -= 1
}
NodeData(reverseComp.getRawArrayBuffer)"""
        }
    }
  }
  //have to split this up from 
  def importGraphAggregateOps(){
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NeighborView = lookupTpe("NeighborView")
    val NodeIdView = lookupTpe("NodeIdView")

    //Actual Graph declaration
    val T = tpePar("T")
    val R = tpePar("R")
    val Graph = tpePar("Graph")
    //math_object_abs only works for a type of Double
    direct(Graph) ("abs", Nil, MDouble :: MDouble) implements single {
      val arg1 = quotedArg(0)
      s"""math_object_abs($arg1)"""
    }
    direct(Graph) ("abs", Nil, NodeData(MDouble) :: NodeData(MDouble)) implements composite {
      val arg1 = quotedArg(0)
      s"""$arg1.map(e => math_object_abs(e))"""
    }
    direct(Graph) ("abs", Nil, MFloat :: MFloat) implements single {
      val arg1 = quotedArg(0)
      s"""if($arg1 > 0) $arg1 else $arg1 * -1"""
    }
    direct(Graph) ("abs", Nil, NodeData(MFloat) :: NodeData(MFloat)) implements composite {
  val arg1 = quotedArg(0)
  s"""$arg1.map(e => abs(e))"""
}

    //a couple of sum methods, with condition provided
    direct(Graph) ("sumOverNeighborsC", R, CurriedMethodSignature(List(("nd_view",NeighborView(MInt)), ("data",Node==>R) ,("cond",Node==>MBoolean,"n=>unit(true)")),R), TNumeric(R)) implements composite {
        s"""nd_view.mapreduce[R]({n => data(Node(n))},{(a,b) => a+b},n=>cond(Node(n)))"""
      }
    direct(Graph) ("sumOverNodesC", R, CurriedMethodSignature(List(("nodes",NodeIdView), ("data",Node==>R) ,("cond",Node==>MBoolean,"n=>unit(true)")),R), TNumeric(R)) implements composite {
        s"""nodes.mapreduce[R]({n => data(Node(n))},{(a,b) => a+b},n=>cond(Node(n)))"""
      }
    direct(Graph) ("sumOverNeighbors", R, CurriedMethodSignature(List(("nd_view",NeighborView(MInt)), ("data",Node==>R)),R), TNumeric(R)) implements composite {
        s"""nd_view.mapreduce[R]({n => data(Node(n))},{(a,b) => a+b}, {n => true})"""
      }
    direct(Graph) ("sumOverNodes", R, CurriedMethodSignature(List(("nodes",NodeIdView), ("data",Node==>R)),R), TNumeric(R)) implements composite {
        s"""nodes.mapreduce[R]({n => data(Node(n))},{(a,b) => a+b},{n => true})"""
      }

    direct(Graph) ("sum", R, NodeData(R) :: R, TNumeric(R)) implements composite {
      val arg1 = quotedArg(0)
      s"""$arg1.reduce((a,b) => a+b)"""
    }
    direct(Graph) ("sum", R, NodeData(NodeData(R)) :: NodeData(R), TFractional(R)) implements composite {
  val arg1 = quotedArg(0)
  s"""$arg1.reduceNested( ((a,b) => a+b),NodeData[R]($arg1.length))"""
}

    // "block" should not mutate the input, but always produce a new copy. in this version, block can change the structure of the input across iterations (e.g. increase its size)
    direct (Graph) ("untilconverged", T, CurriedMethodSignature(List(List(("x", T), ("tol", MDouble, "unit(.0001)"), ("minIter", MInt, "unit(1)"), ("maxIter", MInt, "unit(100)")), ("block", T ==> T), ("diff", (T,T) ==> MDouble)), T)) implements composite {
        s"""var delta = scala.Double.MaxValue
var cur = x
var iter = 0

while ((math_object_abs(delta) > tol && iter < maxIter) || iter < minIter) {
  val prev = cur
  val next = block(cur)
  iter += 1
  delta = diff(prev,next)
  cur = next
}
println("Number of Iterations: " + iter)
if (iter == maxIter){
  println("Maximum iterations exceeded")
}
cur"""
      }
  } 
}
