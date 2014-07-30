import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object PageRankCSRCompiler extends OptiGraphApplicationCompiler with PageRankCSR

// This object lets us run the Scala library version of the code
object PageRankCSRInterpreter extends OptiGraphApplicationInterpreter with PageRankCSR

trait PageRankCSR extends OptiGraphApplication {
  
  def hackyReduce(a: Rep[NodeData[Double]], b: Rep[NodeData[Double]]): Rep[Double] = {
    array_reduce(array_fromfunction(a.length, i => abs(a(i)-b(i))), (x:Rep[Double],y:Rep[Double]) => x+y, 0.0)
  }

  def main() = {
    println("PageRank")
  
    if (args.length < 2) printUsage

    val nodesArray = ForgeFileReader.readLines(args(0)){ _.toInt }
    val edgesArray = ForgeFileReader.readLines(args(1)){ _.toInt }
    val g = undirectedGraphFromCSR(nodesArray, edgesArray)
    tic(g)

    //matches parameters from snap
    //initalize array to 1/numNodes
    val prInit = NodeData.fromFunction[Double](g.numNodes,{e => 1.0/g.numNodes})
    val threshold = 0.01 
    val damp = 0.15
    val maxItr = if(args.length == 3) args(2).toInt else 100
    
    val pr =
     untilconverged(prInit, tol=threshold,maxIter=maxItr){ oldPr =>
      g.mapNodes{ n =>
        damp + (1.0-damp)*sumOverNeighbors(g.inNeighbors(n)){ w =>
          oldPr(w) / g.outDegree(w)}
      }
    }{(curPr,oldPr) => hackyReduce(curPr,oldPr)}
    
    toc(pr)
    println(pr(0))
  }
  def printUsage = {
    println("Usage: PageRank <path to nodes> <path to edges> <OPTIONAL: max # of iterations>")
    exit(-1)
  }
}
