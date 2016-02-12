package optiql.shallow.classes

import optiql.shallow._
import scala.reflect.RefinedManifest
import Record._

//TODO: this object is basically a misc. grab bag of features, but most of it should be pushed directly into Forge
object Rewrite {

  type Rep[+T] = T
  def unit[T:Manifest](x: T) = x

  def groupByHackImpl[K:Manifest,V:Manifest](self: Table[V], keySelector: V => K): Table[Tuple2[K,Table[V]]] = {
    val arr = self.data.take(self.size)
    val keys = arr.map(keySelector).distinct //DeliteMap is order-preserving on keys, be consistent for sanity
    val map = arr.groupBy(keySelector)

    val pairs = for (key <- keys) yield {
      val v = map(key)
      new Tuple2(key, new Table(v.length, v))
    }

    new Table(pairs.length, pairs)
  }

  def sortHackImpl[A:Manifest](self: Table[A], comparator: (A,A) => Int): Table[A] = {
    val arr = self.data.take(self.size)
    val ord = new Ordering[A] {
      def compare(x: A, y: A): Int = comparator(x,y)
    }
    val sorted = arr.sorted(ord)
    new Table(self.size, sorted)
  }

  def compareHackImpl[A:Manifest:Ordering](lhs: A, rhs: A): Int = {
    implicitly[Ordering[A]].compare(lhs, rhs)
  }

  ///////

  def table_printastable[A:Manifest](self: Table[A],maxRows: Int = 100) = {
    TablePrinter.printAsTable(self, maxRows)
  }
  
  def table_writeasjson[A:Manifest](self: Table[A],path: String) = {
    TablePrinter.writeAsJSON(self, path)
  }

  ////////////

  def zeroType[T:Manifest]: T = (manifest[T] match { //need a more robust solution, e.g. type class
    //case StructType(tag,elems) => struct[T](tag, elems.map(e => (e._1, zeroType(e._2))))
    case v if v == manifest[Int] => 0
    case v if v == manifest[Long] => 0L
    case v if v == manifest[Double] => 0.0
    case v if v == manifest[Float] => 0.0f
    case _ => null
  }).asInstanceOf[T]

  def minValue[T:Manifest]: T = (manifest[T] match {
    case v if v == manifest[Int] => scala.Int.MinValue
    case v if v == manifest[Long] => scala.Long.MinValue
    case v if v == manifest[Double] => scala.Double.MinValue
    case v if v == manifest[Float] => scala.Float.MinValue
    case v if v == manifest[Char] => scala.Char.MinValue
    case _ => null //cast_asinstanceof[Null,T](null)) //shouldn't be used for reference types
  }).asInstanceOf[T]

  def maxValue[T:Manifest]: T = (manifest[T] match {
    case v if v == manifest[Int] => scala.Int.MaxValue
    case v if v == manifest[Long] => scala.Long.MaxValue
    case v if v == manifest[Double] => scala.Double.MaxValue
    case v if v == manifest[Float] => scala.Float.MaxValue
    case v if v == manifest[Char] => scala.Char.MaxValue
    case _ => null //cast_asinstanceof[Null,T](null)) //shouldn't be used for reference types
  }).asInstanceOf[T] 

  def upgradeInt[T:Manifest](value: Int): T = (manifest[T] match {
    case v if v == manifest[Int] => value
    case v if v == manifest[Long] => value.toLong
    case v if v == manifest[Double] => value.toDouble
    case v if v == manifest[Float] => value.toFloat
    case _ => throw new RuntimeException("ERROR: don't know how to average type " + manifest[T].toString)
  }).asInstanceOf[T]

  def createRecord[T:Manifest](record: ForgeArray[String]): T = {
    val (elems, isRecord) = manifest[T] match {
      case rm: RefinedManifest[T] => (rm.fields, true)
      case m => (List(("",m)), false)
    }

    val fields = Range(0,elems.length) map { i =>
      val (field, tp) = elems(i)
      tp.toString match {
        case s if s.contains("String") => (field, false, (r:T) => record(i))
        case "Double" => (field, false, (r:T) => record(i).toDouble)
        case "Float" => (field, false, (r:T) => record(i).toFloat)
        case "Boolean" => (field, false, (r:T) => record(i) == "true")
        case "Int" => (field, false, (r:T) => record(i).toInt)
        case "Long" => (field, false, (r:T) => record(i).toLong)
        // case "Char" => (field, false, (r:T) => infix_fcharAt(record(i), 0))
        case d if d.contains("Date") => (field, false, (r:T) => Date(record(i)))
        case _ => throw new RuntimeException("Don't know hot to automatically parse type " + tp.toString + ". Try passing in your own parsing function instead.")
      }
    }
    
    if (isRecord) record_new[T](fields)
    else fields(0)._3(null.asInstanceOf[T]).asInstanceOf[T]
  }
}