package LOWERCASE_DSL_NAME.shallow

object ForgeArray {

  def farray_from_sarray[T:Manifest](__arg0: Array[T]): ForgeArray[T]
    = __arg0
  def array_empty[T:Manifest](__arg0: Int): ForgeArray[T]
    = new ForgeArray[T](__arg0)
  def array_empty_imm[T:Manifest](__arg0: Int): ForgeArray[T]
    = array_empty[T](__arg0)
  def array_copy[T:Manifest](__arg0: ForgeArray[T],__arg1: Int,__arg2: ForgeArray[T],__arg3: Int,__arg4: Int): Unit
    = System.arraycopy(__arg0,__arg1,__arg2,__arg3,__arg4)
  def array_update[T:Manifest](__arg0: ForgeArray[T],__arg1: Int,__arg2: T): Unit
    = __arg0(__arg1) = __arg2
  def array_apply[T:Manifest](__arg0: ForgeArray[T],__arg1: Int): T
    = __arg0(__arg1)
  def array_length[T:Manifest](__arg0: ForgeArray[T]): Int
    = __arg0.length
  def array_clone[T:Manifest](__arg0: ForgeArray[T]): ForgeArray[T]
    = __arg0.clone
  def array_soft_clone[T:Manifest](__arg0: ForgeArray[T]): ForgeArray[T]
    = array_clone(__arg0)
  def array_take[T:Manifest](__arg0: ForgeArray[T],__arg1: Int): ForgeArray[T]
    = __arg0.take(__arg1)
  def array_mkstring[A:Manifest](__arg0: ForgeArray[A],__arg1: String): String
    = __arg0.mkString(__arg1)
  def array_map[T:Manifest,R:Manifest](__arg0: ForgeArray[T], __arg1: T => R): ForgeArray[R]
    = __arg0.map(__arg1)
  def array_flatmap[T:Manifest,R:Manifest](__arg0: ForgeArray[T], __arg1: T => ForgeArray[R]): ForgeArray[R]
    = __arg0.flatMap(e => __arg1(e))
  def array_zip[T:Manifest,B:Manifest,R:Manifest](__arg0: ForgeArray[T],__arg1: ForgeArray[B], __arg2: (T,B) => R): ForgeArray[R]
    = __arg0.zip(__arg1).map(t => __arg2(t._1,t._2))
  def array_reduce[T:Manifest](__arg0: ForgeArray[T],__arg1: (T,T) => T,__arg2: T): T
    = if (array_length(__arg0) == 0) __arg2 else __arg0.reduce(__arg1)
  def array_groupByReduce[T:Manifest,K:Manifest,V:Manifest](__arg0: ForgeArray[T],__arg1: T => K, __arg2: T => V, __arg3: (V,V) => V): ForgeHashMap[K,V] = {
    val grp = __arg0.groupBy[K](__arg1)
    val m = scala.collection.mutable.HashMap[K,V]()
    grp.foreach{ a =>
      m.put(a._1,array_map(a._2,__arg2).reduce(__arg3))
    }
    m
  }
  def array_filter[T:Manifest](__arg0: ForgeArray[T],__arg1: T => Boolean): ForgeArray[T]
    = __arg0.filter(__arg1)
  def array_sort[T:Manifest:Ordering](__arg0: ForgeArray[T]): ForgeArray[T] = {
    val d = array_empty[T](__arg0.length)
    array_copy(__arg0,0,d,0,__arg0.length)
    scala.util.Sorting.quickSort(d)
    d
  }
  def array_sortIndices[R:Manifest:Ordering](__arg0: Int, __arg1: (Int => R)): ForgeArray[Int]
    = array_empty[Int](__arg0).indices.toArray.sortBy(__arg1)
  def array_fromfunction[T:Manifest](__arg0: Int,__arg1: Int => T): ForgeArray[T] = {
    Array.tabulate[T](__arg0)(__arg1)
  }
  def array_fromseq[T:Manifest](__arg0: Seq[T]): ForgeArray[T]
    = __arg0.toArray
  def array_string_split(__arg0: String, __arg1: String, __arg2: Int = 0): ForgeArray[String]
    = __arg0.split(__arg1, __arg2)

  def scala_array_apply[T:Manifest](__arg0: Array[T],__arg1: Int): T
    = array_apply(__arg0,__arg1)
  def scala_array_length[T:Manifest](__arg0: Array[T]): Int
    = array_length(__arg0)
}

object ForgeArrayBuffer {

  def array_buffer_empty[T:Manifest](__arg0: Int): ForgeArrayBuffer[T]
    = new scala.collection.mutable.ArrayBuffer[T]()
  def array_buffer_immutable[T:Manifest](__arg0: ForgeArrayBuffer[T]): ForgeArrayBuffer[T]
    = __arg0
  def array_buffer_strict_empty[T:Manifest](__arg0: Int): ForgeArrayBuffer[T]
    = (new scala.collection.mutable.ArrayBuffer[T]()) ++ (new Array[T](__arg0))
  def array_buffer_new_imm[T:Manifest](__arg0: ForgeArray[T], __arg1: Int): ForgeArrayBuffer[T] = {
    assert(__arg0.length >= __arg1, "array_buffer_new_imm requires the initial array to be at least as large as the array buffer size")
    val out = new scala.collection.mutable.ArrayBuffer[T](__arg1)
    for (i <- 0 until __arg1) {
      out += __arg0(i)
    }
    out
  }
  def array_buffer_copy[T:Manifest](src: ForgeArrayBuffer[T], srcPos: Int, dest: ForgeArrayBuffer[T], destPos: Int, length: Int): Unit = {
    for (i <- 0 until length) {
      dest(destPos+i) = src(srcPos+i)
    }
  }
  def array_buffer_update[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: Int,__arg2: T): Unit
    = __arg0(__arg1) = __arg2
  def array_buffer_apply[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: Int): T
    = __arg0(__arg1)
  def array_buffer_length[T:Manifest](__arg0: ForgeArrayBuffer[T]): Int
    = __arg0.length
  def array_buffer_set_length[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: Int): Unit
    = __arg0.slice(0,__arg1)
  def array_buffer_append[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: T): Unit
    = { __arg0 += __arg1 }
  def array_buffer_indexof[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: T): Int
    = __arg0.indexOf(__arg1)
  def array_buffer_result[T:Manifest](__arg0: ForgeArrayBuffer[T]): ForgeArray[T]
    = __arg0.toArray
  def array_buffer_unsafe_result[T:Manifest](__arg0: ForgeArrayBuffer[T]): ForgeArray[T]
    = __arg0.toArray
  def array_buffer_map[T:Manifest,R:Manifest](__arg0: ForgeArrayBuffer[T], __arg1: T => R): ForgeArrayBuffer[R]
    = __arg0.map(__arg1)
  def array_buffer_flatmap[T:Manifest,R:Manifest](__arg0: ForgeArrayBuffer[T], __arg1: T => ForgeArrayBuffer[R]): ForgeArrayBuffer[R]
    = __arg0.flatMap(__arg1)
  def array_buffer_groupBy[T:Manifest,K:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: T => K): ForgeHashMap[K,ForgeArrayBuffer[T]] ={
    val grp = __arg0.groupBy[K](__arg1)
    (new scala.collection.mutable.HashMap[K,ForgeArrayBuffer[T]]()) ++ (grp)
  }
  def array_buffer_zip[T:Manifest,B:Manifest,R:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: ForgeArrayBuffer[B], __arg2: (T,B) => R): ForgeArrayBuffer[R]
    = __arg0.zip(__arg1).map(t => __arg2(t._1,t._2))
  def array_buffer_reduce[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: (T,T) => T,__arg2: T): T
    = if (array_buffer_length(__arg0) == 0) __arg2 else __arg0.reduce(__arg1)
  def array_buffer_groupByReduce[T:Manifest,K:Manifest,V:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: T => K, __arg2: T => V, __arg3: (V,V) => V): ForgeHashMap[K,V] = {
    val grp = __arg0.groupBy[K](__arg1)
    val hm = scala.collection.mutable.HashMap[K,V]()
    grp.foreach{ a =>
      hm.put(a._1,array_buffer_map(a._2,__arg2).reduce(__arg3))
    }
    hm
  }
  def array_buffer_filter[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: T => Boolean): ForgeArrayBuffer[T]
    = __arg0.filter(__arg1)
  def array_buffer_foreach[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: T => Unit): Unit
    = __arg0.foreach(__arg1)
  def array_buffer_forIndices[T:Manifest](__arg0: ForgeArrayBuffer[T],__arg1: Int => Unit): Unit = {
    var i = 0
    while(i < array_buffer_length(__arg0)){
      __arg1(i)
      i += 1
    }
  }
  def array_buffer_fromfunction[T:Manifest](__arg0: Int, __arg1: Int => T): ForgeArrayBuffer[T] = {
    scala.collection.mutable.ArrayBuffer.tabulate[T](__arg0)(__arg1)
  }
}