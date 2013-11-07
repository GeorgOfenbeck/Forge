import optiwrangler.compiler._
import optiwrangler.library._
import optiwrangler.shared._

object HelloWranglerCompiler extends OptiWranglerApplicationCompiler with HelloWrangler 
object HelloWranglerInterpreter extends OptiWranglerApplicationInterpreter with HelloWrangler 

trait HelloWrangler extends OptiWranglerApplication { 
  def main() = {
    println("hello world")    
    val t1 = Table(args(0))
    val t2 = t1.cut("1").cut("2", 1).cutRight("3").cutRight("4", 1).cutAll("\"").cutAll("0")
      .cut(2).cut(1, 0)//.cut({cell => cell.indexOf("6")})
      .split("2").split(1, 10)
    t2.toFile("~/data/")
  }
}