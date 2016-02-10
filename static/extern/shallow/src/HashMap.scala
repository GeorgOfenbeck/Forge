package LOWERCASE_DSL_NAME.shallow

object ForgeHashMap {

  def fhashmap_from_shashmap[K:Manifest,V:Manifest](m: scala.collection.mutable.HashMap[K,V]): ForgeHashMap[K,V]
    = m
  def fhashmap_from_arrays[K:Manifest,V:Manifest](keys: ForgeArray[K], values: ForgeArray[V]): ForgeHashMap[K,V]
    = scala.collection.mutable.HashMap(keys.zip(values): _*)
  def fhashmap_size[K:Manifest,V:Manifest](m: ForgeHashMap[K,V]): Int
    = m.size
  def fhashmap_get[K:Manifest,V:Manifest](m: ForgeHashMap[K,V], key: K): V
    = m(key)
  def fhashmap_contains[K:Manifest,V:Manifest](m: ForgeHashMap[K,V], key: K): Boolean
    = m.contains(key)
  def fhashmap_keys[K:Manifest,V:Manifest](m: ForgeHashMap[K,V]): ForgeArray[K]
    = m.keys.toArray
  def fhashmap_values[K:Manifest,V:Manifest](m: ForgeHashMap[K,V]): ForgeArray[V]
    = m.values.toArray
  //def fhashmap_toArray[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]]): Rep[ForgeArray[(K,V)]]
}


