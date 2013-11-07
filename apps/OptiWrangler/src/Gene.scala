import optiwrangler.compiler._
import optiwrangler.library._
import optiwrangler.shared._

object GeneCompiler extends OptiWranglerApplicationCompiler with Gene 
object GeneInterpreter extends OptiWranglerApplicationInterpreter with Gene 

trait Gene extends OptiWranglerApplication { 
  def main() = {
    println("hello world")    
    val t1 = Table(args(0))
    val now = clock(t1)
    val t2 =  t1
 //     .promote
 //     .partition(_.substring(4,8), 1)
      .delete(clip, 1)
      .cutBefore(13, 1)
      .force
    println("Total time : " + ((clock(t2)-now) / 1e3))
  }
}