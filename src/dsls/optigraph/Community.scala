/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Stores data asscoicated with nodes in an array 
buffer indexed by internal node IDs
*///////////////////////////////////////////////////////////////

package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait CommunityOps {
  this: OptiGraphDSL =>
  def importCommunityOps() {
    val UndirectedGraph = lookupTpe("UndirectedGraph")
    val NodeData = lookupTpe("NodeData")
    val Node = lookupTpe("Node")
    val NodeIdView = lookupTpe("NodeIdView")
    val Tuple2 = lookupTpe("Tup2")
    val Tuple3 = lookupTpe("Tup3")
    val Tuple5 = lookupTpe("Tup5")
    val Community = tpe("Community")
    val T = tpePar("T")
    val K = tpePar("K")
    val V = tpePar("V")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    data(Community,("_size",MInt),("_precision",MDouble),("_modularity",MDouble),("_canImprove",MBoolean),("_totalWeight",MDouble),("_graph",UndirectedGraph),("_n2c",MArray(MInt)),("_tot",MArray(MDouble)),("_in",MArray(MDouble)),("_n2o",MArray(MInt)))
    static(Community)("apply", Nil, (("g",UndirectedGraph),("precision",MDouble),("n2o",MArray(MInt))) :: Community) implements allocates(Community,{
      s"""alloc_size(g)"""
    },{
      s"""precision"""
    },{
      s"""unit(0d)"""
    },{
      s"""unit(true)"""
    },{
      val arg1 = quotedArg(0)
      s"""alloc_total_weight($arg1)"""
    },{
      val arg1 = quotedArg(0)
      s"""$arg1"""
    },{
      s"""alloc_ints(alloc_size(g),{e => e})"""
    },{
      s"""alloc_weights(g)"""
    },{
      s"""alloc_selfs(g)"""
    },{
      s"""n2o"""
    })
    static(Community)("apply", Nil, (("g",UndirectedGraph),("precision",MDouble)) :: Community) implements composite {
      val g = quotedArg("g")
      val precision = quotedArg("precision")
      s"""Community($g, $precision, array_fromfunction[Int]($g.numNodes,e=>e))"""
    }
    static(Community)("apply", Nil, MethodSignature(List(("size",MInt),("precision",MDouble),("modularity",MDouble),("canImprove",MBoolean),("totalWeight",MDouble),("g",UndirectedGraph),("n2c",MArray(MInt)),("tot",MArray(MDouble)),("in",MArray(MDouble)),("n2o",MArray(MInt))),Community)) implements allocates(Community,{
  s"""size"""
},{
  s"""precision"""
},{
  s"""modularity"""
},{
  s"""canImprove"""
},{
  s"""totalWeight"""
},{
  s"""g"""
},{
  s"""n2c"""
},{
  s"""tot"""
},{
  s"""in"""
},{
  s"""n2o"""
})

    /*
      n2c -> size == numNodes, indexed by nodeID gives the community a node belongs to
      in,tot -> size == # of communities, used for modularity calculation
        tot == total weighted degree of the community
        in == sum of degree of links strictly within the community (divided by 2)
    */

    val CommunityOps = withTpe(Community)
    CommunityOps{  
      infix("modularity")(Nil :: MDouble) implements composite {
          val self = quotedArg("self")
          s"""val g = $self.graph
val tot = $self.tot
val in = $self.in
var m2 = $self.totalWeight

sumOverNodes(g.nodes)({ n => 
  if(tot(n.id) > 0)
    (in(n.id)/m2) - (tot(n.id)/m2)*(tot(n.id)/m2)
  else 
    0d
})"""
        }
      infix("modularity")( (("in",MArray(MDouble)),("tot",MArray(MDouble))) :: MDouble) implements composite {
          val self = quotedArg("self")
          s"""val g = $self.graph
var m2 = $self.totalWeight

sumOverNodes(g.nodes)({ n => 
  if(tot(n.id) > 0)
    (in(n.id)/m2) - (tot(n.id)/m2)*(tot(n.id)/m2)
  else 
    0d
})"""
        }
      infix("modularityGain")( (("totc",MDouble),("dnodecomm",MDouble),("w_degree",MDouble)) :: MDouble) implements composite {
          val self = quotedArg("self")
          s"""val degc = w_degree 
val m2 = $self.totalWeight  
val dnc = dnodecomm 

(dnc - totc*degc/m2)"""
        }
      infix("display")(Nil :: MUnit, effect=simple) implements single {
          val self = quotedArg("self")
          s"""var i = 0
while(i < $self.size){
  println(" " + i + "/" + $self.n2c(i) + "/" + $self.in(i) + "/" + $self.tot(i))
  i += 1
}"""
        }
      infix("display")((("n2c",MArray(MInt)),("in",MArray(MDouble)),("tot",MArray(MDouble))) :: MUnit, effect=simple) implements single {
          val self = quotedArg("self")
          s"""var i = 0
while(i < $self.size){
  println(" " + i + "/" + n2c(i) + "/" + in(i) + "/" + tot(i))
  i += 1
}"""
        }
      infix("generateNewGraph")(Nil :: Tuple2(UndirectedGraph,MArray(MInt))) implements composite {
          val self = quotedArg("self")
          s"""val g = $self.graph
val n2c = $self.n2c
val size = $self.size

val originalNodeIds = NodeData.fromFunction(size,i=>i)
val groupedComms = originalNodeIds.groupBy(k => n2c(k),v => v)

val newSize = fhashmap_size(groupedComms)
val newComms = NodeData.fromFunction(newSize,i => i)
val oldComms = NodeData(fhashmap_keys(groupedComms))

val old2newHash = fhashmap_from_arrays[Int,Int](oldComms.getRawArray,newComms.getRawArray)



val n2new = array_map[Int,Int]($self.n2o, {o =>
  val oldcomm = n2c(o)
  fhashmap_get(old2newHash,oldcomm)
})

  
  
    
  
    


val newGraph = newComms.map({ src =>
  
  val oldComm = oldComms(src)
  val nodeHash = SHashMap[Int,Double]()

  val nodesInComm = NodeData(fhashmap_get(groupedComms,oldComm))
  nodesInComm.foreach({ n => 
    val (neighbors,nbrWeights) = unpack(g.getNeighborsAndWeights(Node(n)))
    var i = 0

    while(i < neighbors.length){
      val dst = old2newHash(n2c(neighbors(i)))
      if(nodeHash.contains(dst)){
nodeHash.update(dst,nodeHash(dst)+nbrWeights(i))
      }
      else{
nodeHash.update(dst,nbrWeights(i))
      }
      i += 1
    }
  })
  nodeHash
})
val numEdges = newGraph.mapreduce[Int](a => array_length(a.keys), (a,b) => a+b, a => true)
val serial_out = $self.assignUndirectedIndicies(newSize,numEdges,newGraph.getRawArray)

pack(UndirectedGraph(newSize,oldComms.getRawArray,serial_out._1,serial_out._2,serial_out._3),n2new)"""
        }

      infix("assignUndirectedIndicies")((("numNodes",MInt),("numEdges",MInt),("src_groups",MArray(SHashMap(MInt,MDouble)))) :: Tuple3(MArray(MInt),MArray(MInt),MArray(MDouble))) implements single {
          s"""val src_edge_array = NodeData[Int](numEdges)
val src_edge_weight_array = NodeData[Double](numEdges)
val src_node_array = NodeData[Int](numNodes)
var i = 0
var j = 0

while(i < numNodes){
  val neighborhash = src_groups(i)
  val neighborhood = NodeData(neighborhash.keys).sort
  val neighWeights = neighborhood.map(e => neighborhash(e))
  var k = 0
  while(k < neighborhood.length){
    src_edge_array(j) = neighborhood(k)
    src_edge_weight_array(j) = neighWeights(k)
    j += 1
    k += 1
  }
  if(i < numNodes-1){
    src_node_array(i+1) = neighborhood.length + src_node_array(i)
  }
  i += 1
}
pack(src_node_array.getRawArray,src_edge_array.getRawArray,src_edge_weight_array.getRawArray)"""
        }
      //Why do I have to mark the community as mutable still?
      infix("buildNeighboringCommunities")((("n",Node),("n2c",MArray(MInt))) :: SHashMap(MInt,MDouble)) implements single {
          val self = quotedArg("self")
          s"""val g = $self.graph
val commWeights = SHashMap[Int,Double]()
commWeights.update(n2c(n.id),0d) 
val (neighbors,nbrWeights) = unpack(g.getNeighborsAndWeights(n))
var i = 0
while(i < neighbors.length){
  val neigh_comm = n2c(neighbors(i))
  if(neighbors(i) != n.id){
    if(commWeights.contains(neigh_comm)){
      commWeights.update(neigh_comm,commWeights(neigh_comm)+nbrWeights(i))
    }
    else{
      commWeights.update(neigh_comm,nbrWeights(i))
    }
  }
  i += 1
}
commWeights"""
        }
      infix("findBestCommunityMove")((("n",Node),("n2c",MArray(MInt)),("tot",MArray(MDouble)),("commWeights",SHashMap(MInt,MDouble))) :: Tuple2(MInt,MDouble)) implements single {
          val self = quotedArg("self")
          s"""val g = $self.graph
val node_comm = n2c(n.id) 
val w_degree = g.weightedDegree(n)


var best_comm = node_comm
var best_nblinks = commWeights(node_comm)
var best_increase = $self.modularityGain(tot(node_comm)-g.weightedDegree(n),best_nblinks,w_degree) 

val comms = commWeights.keys
var i = 0
while(i < array_length(comms)){
  val neigh_comm = comms(i)
  if(neigh_comm != node_comm){
    val weight = commWeights(neigh_comm)
    val increase = $self.modularityGain(tot(neigh_comm),weight,w_degree) 
    if(increase > best_increase){
      best_comm = neigh_comm
      best_nblinks = weight
      best_increase = increase
    }
  }
  i += 1    
}
pack(best_comm,best_nblinks)"""
        }
      infix("parallelArrayCopy")((("dst",MArray(T)),("src",MArray(T))) :: MUnit, effect=write(1),aliasHint = copies(2), addTpePars = T ) implements composite {
          s"""NodeIdView(array_length(src)).foreach({i =>
  dst(i) = src(i)
})"""
        }
      infix("louvain")(Nil :: Community) implements single {
          val self = quotedArg("self")
          s"""val g = $self.graph
val totalWeight = $self.totalWeight
val size = $self.size 

val tot = array_empty[Double](array_length($self.tot)) 
val in = array_empty[Double](array_length($self.in)) 
val n2c = array_empty[Int](array_length($self.n2c)) 

$self.parallelArrayCopy(tot,$self.tot)  
$self.parallelArrayCopy(in,$self.in) 
$self.parallelArrayCopy(n2c,$self.n2c) 




val min_modularity = $self.precision
var nb_moves = 0
var nb_pass_done = 0
var new_mod = $self.modularity
var cur_mod = new_mod

var continue = true
while(continue){
  cur_mod = new_mod
  nb_moves = 0
  nb_pass_done += 1
  
  g.foreachNode{ n =>
    val node_comm = n2c(n.id) 
    val commWeights = $self.buildNeighboringCommunities(n,n2c)
    val (best_comm,best_nblinks) = unpack($self.findBestCommunityMove(n,n2c,tot,commWeights))

    if(best_comm != node_comm){
      
      
      $self.insert(n2c,in,tot,n.id,node_comm,commWeights(node_comm),best_comm,best_nblinks) 
      
    }
  }
  
  
  
  

  

  new_mod = $self.modularity(in,tot)
  
  continue = (new_mod-cur_mod) > min_modularity
}
val improvement = (nb_pass_done > 1) || (new_mod != cur_mod)
println("Number of passes: " + nb_pass_done + " improvement: " + improvement)
Community(size,$self.precision,new_mod,improvement,totalWeight,g,n2c,tot,in,$self.n2o)"""
        }

      infix("insert")( MethodSignature(List(("n2c",MArray(MInt)),("in",MArray(MDouble)),("tot",MArray(MDouble)),("node",MInt),("old_comm",MInt),("olddnodecomm",MDouble),("comm",MInt),("dnodecomm",MDouble)),MUnit), effect = write(1,2,3)) implements single {
          val self = quotedArg("self")
          s"""array_update(tot,old_comm,tot(old_comm)-$self.graph.weightedDegree(Node(node)))
array_update(tot,comm,tot(comm)+$self.graph.weightedDegree(Node(node)))

array_update(in,old_comm,in(old_comm)-(2*olddnodecomm+$self.graph.numSelfLoops(Node(node))))
array_update(in,comm,in(comm)+(2*dnodecomm+$self.graph.numSelfLoops(Node(node))))

array_update(n2c,node,comm)"""
        }

      infix ("size") (Nil :: MInt) implements getter(0, "_size")
      infix ("precision") (Nil :: MDouble) implements getter(0, "_precision")
      infix ("totalWeight") (Nil :: MDouble) implements getter(0, "_totalWeight")
      infix ("storedModularity") (Nil :: MDouble) implements getter(0, "_modularity")
      infix ("canImprove") (Nil :: MBoolean) implements getter(0, "_canImprove")
      infix ("graph") (Nil :: UndirectedGraph) implements getter(0, "_graph")
      infix ("n2c") (Nil :: MArray(MInt)) implements getter(0, "_n2c")
      infix ("in") (Nil :: MArray(MDouble)) implements getter(0, "_in")
      infix ("tot") (Nil :: MArray(MDouble)) implements getter(0, "_tot")   
      infix ("n2o") (Nil :: MArray(MInt)) implements getter(0, "_n2o")

      infix ("tot") (MInt :: MDouble) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_apply($self.tot, $arg1)"""
      }
      infix ("in") (MInt :: MDouble) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_apply($self.in, $arg1)"""
      }
      infix ("n2c") (MInt :: MInt) implements composite {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""array_apply($self.n2c, $arg1)"""
      }
      infix ("n2o") (MInt :: MInt) implements composite {
  val self = quotedArg("self")
  val arg1 = quotedArg(1)
  s"""array_apply($self.n2o, $arg1)"""
}

      /*
      We might need these when parallelism is added in.
      infix ("updateTot") ( (MInt,MDouble) :: MUnit, effect = write(0)) implements composite ${ array_update($self.tot,$1,$2)}
      infix ("updateIn") ( (MInt,MDouble) :: MUnit, effect = write(0)) implements composite ${ array_update($self.in,$1,$2)}
      infix ("updateN2c") ( (MInt,MInt) :: MUnit, effect = write(0)) implements composite ${ array_update($self.n2c,$1,$2)}

      infix ("setTot") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_tot", quotedArg(1))
      infix ("setIn") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_in", quotedArg(1))
      infix ("setN2c") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_n2c", quotedArg(1))
      */
    }
    compiler (Community) ("alloc_total_weight", Nil, UndirectedGraph :: MDouble) implements single {
      val arg1 = quotedArg(0)
      s"""$arg1.totalWeight"""
    }
    compiler (Community) ("alloc_size", Nil, UndirectedGraph :: MInt) implements single {
      val arg1 = quotedArg(0)
      s"""$arg1.numNodes"""
    }
    compiler (Community) ("alloc_doubles", Nil, (MInt,(MInt ==> MDouble)) :: MArray(MDouble)) implements single {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""array_fromfunction[Double]($arg1,$arg2)"""
    }
    compiler (Community) ("alloc_ints", Nil, (MInt,(MInt ==> MInt)) :: MArray(MInt)) implements single {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""array_fromfunction[Int]($arg1,$arg2)"""
    }
    compiler (Community) ("alloc_weights", Nil, UndirectedGraph :: MArray(MDouble)) implements single {
      val arg1 = quotedArg(0)
      s"""array_fromfunction[Double](alloc_size($arg1),{n => $arg1.weightedDegree(Node(n))})"""
    }
    compiler (Community) ("alloc_selfs", Nil, UndirectedGraph :: MArray(MDouble)) implements single {
    val arg1 = quotedArg(0)
    s"""array_fromfunction[Double](alloc_size($arg1),{n => $arg1.numSelfLoops(Node(n))})"""
  }
  } 
}