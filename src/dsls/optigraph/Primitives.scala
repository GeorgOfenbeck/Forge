/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Simply store an ID.  That's all a node is!
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait NodeOps {
  this: OptiGraphDSL =>
  def importNodeOps() {
    val Node = tpe("Node")
    data(Node, ("_id", MInt))
    static (Node) ("apply", Nil, MInt :: Node) implements allocates(Node, {
      val arg1 = quotedArg(0)
      s"""$arg1"""
    })
    val NodeOps = withTpe(Node)
    NodeOps {
      infix("id") (Nil :: MInt) implements getter(0,"_id")
      infix(">") (Node :: MBoolean) implements single {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1.id>$arg2.id"""
      }
      infix("<") (Node :: MBoolean) implements single {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1.id<$arg2.id"""
      }
      infix("<=") (Node :: MBoolean) implements single {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1.id<=$arg2.id"""
      }  
      infix(">=") (Node :: MBoolean) implements single {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1.id>=$arg2.id"""
      } 
      infix("==") (Node :: MBoolean) implements single {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""$arg1.id==$arg2.id"""
    }
    }
  }
}

trait EdgeOps {
  this: OptiGraphDSL =>    
  def importEdgeOps() {
    val Node = lookupTpe("Node")
    val Edge = tpe("Edge")
    data(Edge, ("_nodeFrom", Node),("_nodeTo",Node))
    static (Edge) ("apply", Nil, (Node,Node) :: Edge) implements allocates(Edge, {
      val arg1 = quotedArg(0)
      s"""$arg1"""
    }, {
      val arg1 = quotedArg(1)
      s"""$arg1"""
    })
    val EdgeOps = withTpe(Edge)
    EdgeOps{
      infix("fromNode") (Nil :: Node) implements getter(0,"_nodeFrom")
      infix("toNode") (Nil :: Node) implements getter(0,"_nodeTo")
    }
  }
}