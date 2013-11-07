import optiwrangler.compiler._
import optiwrangler.library._
import optiwrangler.shared._

object TripleCutCompiler extends OptiWranglerApplicationCompiler with TripleCut 
object TripleCutInterpreter extends OptiWranglerApplicationInterpreter with TripleCut 

trait TripleCut extends OptiWranglerApplication { 
  def main() = {
    println("hello world")    
    //println(args(0))
    //val col = Table(0, "").t1FromFile("/afs/cs.stanford.edu/u/gibbons4/data/singleCol.txt")
    var now = clock(3) // why does clock() go away
    val t1 = Table(args(0))
    println("Loaded from file : " + ((clock(t1) - now) / 1e3.toDouble))

    now = clock(t1)
    val t2 = t1.cut("1")
    println("Cut 1   : " + ((clock(t2) - now) / 1e3.toDouble))

    now = clock(t2)
    val t3 = t2.cut("2").cut("3")
    println("Cut 2/3 : " + ((clock(t3) - now) / 1e3.toDouble))

    now = clock(t3)
    val t4 = t3.cut("4")
    println("Cut 4   : " + ((clock(t4) - now) / 1e3.toDouble))

    t4.toFile("~/data/")

  /*
    val col = Table(0, "").t1FromFile(args(0))
    println("Loaded from file : " + ((clock() - now) / 1e3.toDouble))
   // col(0) = Array("1")
    //col(2) = Array("3")
  
    val now2 = clock()
    val x = col.cutAll("\"").cut("1")
    println("Cut : " + ((clock() - now2) / 1e3.toDouble))
    
    val now3 = clock()
    x.t1ToFile("/home/gibbons4/data/")
    println("Written to file : " + ((clock() - now3) / 1e3.toDouble))

*/
/*
    println(cutcol(0,0))
    println(cutcol(1, 0))

    println(cutcol.getColumn(1))
    println(cutcol.getColumn("2"))
*/
/*
    val splitcol = col.split(1, null)
    println(cutcol(0, 0) + "," + cutcol(0, 1))
    println(cutcol(1, 0) + "," + cutcol(1, 1))

    cutcol.putHeader("one", 1)
    cutcol.putHeader("two", 1)
    
    println(cutcol.getHeader("one"))
*/
  }
}