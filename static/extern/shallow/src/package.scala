package LOWERCASE_DSL_NAME

package object shallow /* extends FractionalImplicits */ {
  type ForgeArray[T] = scala.Array[T]
  type ForgeArrayBuffer[T] = scala.collection.mutable.ArrayBuffer[T]
  type ForgeHashMap[K,V] = scala.collection.mutable.HashMap[K,V]
  type SHashMap[K,V] = scala.collection.mutable.HashMap[K,V]
  type SByteBuffer = java.nio.ByteBuffer
  type FString = String
  type Tup2[A,B] = scala.Tuple2[A,B] // TODO fix general Tup - Tuple translation in shallow

  class FractionalOps[T: Fractional](lhs: T){
    def /(rhs: T) = implicitly[Fractional[T]].div(lhs, rhs)
  }
  implicit def infixFractionalOps[T: Fractional](x: T): FractionalOps[T] = new FractionalOps(x)

  implicit def toForgeExtras[T](obj: T): ForgeExtras[T] = new ForgeExtras(obj)

  def fatal(msg: String) = sys.error(msg)
}
