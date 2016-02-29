package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * Stream provides basic iterator functionality over a file or computation too
 * large to fit in memory.
 *
 * Note: HashStream's mapValues and FileStream's map/mapRows will both skip writing empty
 *       lines / rows to file, which allows for in-line filtering of bad records.
 *
 * Current limitations:
 *   1) HashStream is single-box, Scala-only (uses an embedded DB via codegen methods, lambda for deserialization)
 *   2) FileStream groupRowsBy uses a logical key naming scheme that depends on LevelDB's sorted key storage for efficiency.
 */
trait StreamOps {
  this: OptiMLDSL =>

  /* we internally have a Key -> Coll[Value] store, implemented as a key := LogicalKey_Sep_UniqueId -> Value
   * This is done so that a logical append corresponds to only a physical put rather than a get-concat-put
   * LevelDB sorts data by key, so using the logical key as a prefix makes LevelDB keep all Coll[Value] adjacent on disk
   */
  val HASH_LOGICAL_KEY_SEPARATOR = "_LK_"

  def importStreamOps() {
    importHashStreamOps()
    importFileStreamOps()
    importComputeStreamOps()
  }

  def importHashStreamOps() {
    val HashStream = lookupTpe("HashStream")
    val FileStream = lookupTpe("FileStream")
    val DenseVector = lookupTpe("DenseVector")
    val Tup2 = lookupTpe("Tup2")
    val V = tpePar("V")
    val R = tpePar("R")

    val LevelDB = tpe("org.iq80.leveldb.DB")
    primitiveTpePrefix ::= "org.iq80"

    data(HashStream, ("_table", MString), ("_db", LevelDB), ("_deserialize", MLambda(Tup2(HashStream(V),MString), V)))

    static (HashStream) ("apply", V, (("table", MString), ("deserialize", (HashStream(V),MString) ==> V)) :: HashStream(V), effect = mutable) implements composite {
        s"""val hash = hash_alloc_raw[V](table, deserialize)
hash.open()
hash"""
      }

    compiler (HashStream) ("hash_alloc_raw", V, (("table", MString), ("deserialize", (HashStream(V),MString) ==> V)) :: HashStream(V), effect = mutable) implements
      allocates(HashStream, {
  val arg1 = quotedArg(0)
  s"""$arg1"""
}, "unit(null.asInstanceOf[org.iq80.leveldb.DB])", {
  s"""doLambda((t: Rep[Tup2[HashStream[V],String]]) => deserialize(t._1, t._2))"""
})


    // -- code generated internal methods interface with the embedded db

    // We use simple effects in lieu of read / write effects because these are codegen nodes,
    // so we cannot pass the struct to them (a limitation of Forge at the moment).

    compiler (HashStream) ("hash_open_internal", Nil, MString :: LevelDB, effect = simple) implements codegen($cala, {
        val arg1 = quotedArg(0)
        s"""import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory._
val options = new Options()
options.createIfMissing(true)

val db = factory.open(new java.io.File($arg1), options)
db"""
      })

    compiler (HashStream) ("hash_contains_internal", Nil, (LevelDB, MArray(MByte)) :: MBoolean, effect = simple) implements codegen($cala, {
        val arg1 = quotedArg(1)
        val arg2 = quotedArg(0)
        s"""val key = $arg1
val iterator = $arg2.iterator()
iterator.seek(key)
val res = iterator.hasNext && {
  val foundKey = iterator.next.getKey
  java.util.Arrays.equals(key, foundKey) || foundKey.startsWith(key ++ "$HASH_LOGICAL_KEY_SEPARATOR".getBytes)
}
iterator.close()
res"""
      })

    compiler (HashStream) ("hash_get_internal", Nil, (LevelDB, MArray(MByte)) :: MArray(MByte), effect = simple) implements codegen($cala, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""$arg1.get($arg2)"""
      })

    compiler (HashStream) ("hash_get_all_internal", Nil, (LevelDB, MString) :: MArray(MArray(MByte)), effect = simple) implements codegen($cala, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""val db = $arg1
val prefix = ($arg2 + "$HASH_LOGICAL_KEY_SEPARATOR").getBytes

val buf = scala.collection.mutable.ArrayBuffer[Array[Byte]]()
val iterator = db.iterator()
iterator.seek(prefix) 

var continue = true
while (iterator.hasNext && continue) {
  val pair = iterator.next()
  val key = pair.getKey
  if (key.startsWith(prefix)) {
    buf += pair.getValue
  } else {
    continue = false
  }
}

iterator.close()
buf.toArray"""
      })

    compiler (HashStream) ("hash_put_internal", Nil, (LevelDB, MArray(MByte), MArray(MByte)) :: MUnit, effect = simple) implements codegen($cala, {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        val arg3 = quotedArg(2)
        s"""$arg1.put($arg2, $arg3)"""
      })

    compiler (HashStream) ("hash_put_all_internal", Nil, (LevelDB, MArray(MArray(MByte)), MArray(MArray(MByte)), MInt) :: MUnit, effect = simple) implements codegen($cala, {
        val arg1 = quotedArg(1)
        val arg2 = quotedArg(3)
        val arg3 = quotedArg(2)
        val arg4 = quotedArg(0)
        s"""assert($arg1.length >= $arg2 && $arg3.length >= $arg2, "HashStream putAll called with too small arrays")
val batch = $arg4.createWriteBatch()
var i = 0
while (i < $arg2) {
  batch.put($arg1(i), $arg3(i))
  i += 1
}
$arg4.write(batch)
batch.close()"""
      })

    compiler (HashStream) ("hash_close_internal", Nil, LevelDB :: MUnit, effect = simple) implements codegen($cala, {
        val arg1 = quotedArg(0)
        s"""$arg1.close()"""
      })

    compiler (HashStream) ("hash_keys_internal", Nil, LevelDB :: MArray(MString)) implements codegen($cala, {
        val arg1 = quotedArg(0)
        s"""val buf = scala.collection.mutable.ArrayBuffer[String]()
val iterator = $arg1.iterator()
iterator.seekToFirst()
var lastKey: String = null

while (iterator.hasNext) {
  val dbKey = new String(iterator.next.getKey)
  if ((lastKey eq null) || !dbKey.startsWith(lastKey)) {
    val sepIndex = dbKey.indexOf("$HASH_LOGICAL_KEY_SEPARATOR")
    val key = if (sepIndex != -1) dbKey.substring(0, sepIndex) else dbKey
    lastKey = key + "$HASH_LOGICAL_KEY_SEPARATOR" 
    buf += key
  }
}

iterator.close()
buf.toArray"""
      })

    // --

    val HashStreamOps = withTpe(HashStream)
    HashStreamOps {
      compiler ("hash_deserialize") (Nil :: MLambda(Tup2(HashStream(V),MString), V)) implements getter(0, "_deserialize")
      compiler ("hash_table_name") (Nil :: MString) implements getter(0, "_table")
      compiler ("hash_get_db") (Nil :: LevelDB) implements getter(0, "_db")
      compiler ("hash_set_db") (LevelDB :: MUnit, effect = write(0)) implements setter(0, "_db", {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

      compiler ("hash_get_db_safe") (Nil :: LevelDB) implements composite {
          val self = quotedArg("self")
          s"""val db = hash_get_db($self)
fassert(db != null, "No DB opened in HashStream")
db"""
        }

      infix ("open") (Nil :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          s"""val table = hash_table_name($self)
val db = hash_open_internal(table)
hash_set_db($self, db)"""
        }

      infix ("apply") (MString :: V) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val lambda = hash_deserialize($self)
doApply(lambda, pack(($self,$arg1)))"""
        }

      // This may be too inefficient, since a subsequent get has to hit the hash again.
      // However, if it's cached, it should be fine.
      infix ("contains") (MString :: MBoolean) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""hash_contains_internal(hash_get_db_safe($self), $arg1.getBytes)"""
        }

      infix ("keys") (Nil :: MArray(MString)) implements single {
          val self = quotedArg("self")
          s"""hash_keys_internal(hash_get_db_safe($self))"""
        }

      infix ("get") (MString :: MArray(MByte)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""hash_get_internal(hash_get_db_safe($self), $arg1.getBytes)"""
        }

      infix ("put") ((MString, MArray(MByte)) :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""hash_put_internal(hash_get_db_safe($self), $arg1.getBytes, $arg2)"""
        }

      //get all values associated with the supplied logical key (prefix)
      infix ("getAll") (MString :: MArray(MArray(MByte))) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""hash_get_all_internal(hash_get_db_safe($self), $arg1)"""
        }

      infix ("putAll") ((MArray(MString), MArray(MArray(MByte)), MInt) :: MUnit, effect = write(0)) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          val arg3 = quotedArg(3)
          s"""hash_put_all_internal(hash_get_db_safe($self), $arg1.map(_.getBytes), $arg2, $arg3)"""
        }

      infix ("close") (Nil :: MUnit, effect = write(0)) implements single {
          val self = quotedArg("self")
          s"""hash_close_internal(hash_get_db_safe($self))
hash_set_db($self, unit(null.asInstanceOf[org.iq80.leveldb.DB]))"""
        }


      // -- bulk

      infix ("mapValues") (CurriedMethodSignature(List(
        List(
          ("outFile", MString),
          ("outDelim", MString, "unit(\"    \")") // output delimiter is an ordinary string
        ),
        List(
          ("func", (MString,V) ==> DenseVector(R))
        )), FileStream), TStringable(R), addTpePars = R) implements composite {
  val self = quotedArg("self")
  s"""deleteFile(outFile)

        
        
        val lambda = hash_deserialize($self)
        val keys = $self.keys

        
        
        
        val chunkSize = 10000000 
        

        val numChunks = ceil(keys.length.toDouble / chunkSize)
        var i = 0
        var keysProcessed = 0
        while (i < numChunks) {
          
          val processSize: Rep[Int] = if (i == numChunks - 1) keys.length - keysProcessed else chunkSize
          val vecs = array_fromfunction(processSize, i => func(keys(keysProcessed+i), doApply(lambda, pack(($self,keys(keysProcessed+i))))))
          val writeArray = array_filter(vecs, (e: Rep[DenseVector[R]]) => e.length > 0)
          ForgeFileWriter.writeLines(outFile, writeArray.length, append = true) { i =>
            val v = writeArray(i)
            array_mkstring(v.toArray, outDelim)
          }
          keysProcessed += chunkSize
          i += 1
        }

        FileStream(outFile)"""
}
    }
  }

  def importFileStreamOps() {
    val FileStream = lookupTpe("FileStream")
    val HashStream = lookupTpe("HashStream")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")

    data(FileStream, ("_path", MString))

    static (FileStream) ("apply", Nil, MString :: FileStream, effect = simple) implements allocates(FileStream, {
  val arg1 = quotedArg(0)
  s"""$arg1"""
})

    val T = tpePar("T")
    val R = tpePar("R")

    // These "hashMatrix" operations are factored out of groupRowsBy() so that end users can create a
    // HashStream directly from a persistent hash that was constructed from a FileStream.groupRowsBy() call.
    //     e.g. val hash = HashStream("test.hash", hashMatrixDeserializer)
    direct (FileStream) ("hashMatrixDeserializer", Nil, (("hash", HashStream(DenseMatrix(MDouble))), ("k", MString)) :: DenseMatrix(MDouble)) implements composite {
        s"""val rows = hash.getAll(k)
val numRows = rows.length
val numCols0 = ByteBufferWrap(rows(0)).getInt()

val out = DenseMatrix[Double](numRows, numCols0)

var i = 0
while (i < numRows) {
  val rowArray = rows(i)
  val rowBuffer = ByteBufferWrap(rowArray)
  val numCols = rowBuffer.getInt()
  fassert(numCols0 == numCols, "hashMatrixDeserializer: expected " + numCols0 + " cols, but found " + numCols)
  val dst = densematrix_raw_data(out).unsafeMutable 
  rowBuffer.unsafeImmutable.get(dst, i*numCols, numCols) 
  i += 1
}

out.unsafeImmutable"""
      }

    // Create a lexicographically ordered key with the given prefix; the key suffix will be unique for every call
    compiler (FileStream) ("hashMatrixNewKey", Nil, (MString, MInt) :: MString) implements codegen($cala, {
        val arg1 = quotedArg(0)
        s"""val uniqueId = new java.rmi.server.UID() 


($arg1 + "$HASH_LOGICAL_KEY_SEPARATOR" + uniqueId.toString)"""
      })

    // --

    direct (FileStream) ("getChunkByteSize", Nil, Nil :: MLong) implements composite {
        s"""val chunkSize: String = System.getProperty("optiml.stream.chunk.bytesize","1e9")
val chunkDouble: Double = chunkSize.toDouble
unit(chunkDouble.toLong)"""
      }

    // We need to read sequentially, but from potentially different data stores, so we use ForgeFileInputStream and ForgeFileOutputStream
    val FileStreamOps = withTpe(FileStream)
    FileStreamOps {
      infix ("path") (Nil :: MString) implements getter(0, "_path")

      // rows are loaded and executed sequentially
      infix ("foreach") ((MString ==> MUnit) :: MUnit, effect = simple) implements single {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val f = ForgeFileInputStream($self.path)
var line = f.readLine()
while (line != null) {
  $arg1(line)
  line = f.readLine()
}
f.close()"""
        }

      infix ("processFileChunks") (MethodSignature(List(("readFunc", MString ==> R), ("processFunc", MArray(R) ==> MUnit), ("chunkSize", MLong, "filestream_getchunkbytesize()")), MUnit), addTpePars = R) implements composite {
          val self = quotedArg("self")
          s"""val f = ForgeFileInputStream($self.path)
val totalSize = f.size
f.close()

val numChunks = ceil(totalSize.toDouble / chunkSize)
var totalBytesRead = 0L
var totalLinesRead = 0
var i = 0

while (i < numChunks) {
  
  val processSize: Rep[Long] = if (i == numChunks - 1) totalSize - totalBytesRead else chunkSize
  val a = ForgeFileReader.readLinesChunk($self.path)(totalBytesRead, processSize)(readFunc)
  processFunc(a)

  totalBytesRead += processSize
  totalLinesRead += a.length
  i += 1
}"""
        }

      // chunks are loaded in parallel, one chunk at a time
      infix ("map") (CurriedMethodSignature(List(
        List(
          ("outFile", MString),
          ("preserveOrder", MBoolean, "unit(false)"),
          ("chunkSize", MLong, "filestream_getchunkbytesize()")
        ),
        List(
          ("func", MString ==> MString)
        )), FileStream)) implements composite {
  val self = quotedArg("self")
  s"""deleteFile(outFile)

        $self.processFileChunks(func, { (a: Rep[ForgeArray[String]]) =>
          if (!preserveOrder) {
            
            
            
            val writeArray = array_filter(a, (e: Rep[String]) => e.length > 0)
            ForgeFileWriter.writeLines(outFile, writeArray.length, append = true)(i => writeArray(i))
          }
          else {
            
            val out = ForgeFileOutputStream(outFile, append = true)
            for (j <- 0 until a.length) {
              if (a(j).length > 0)
                out.writeLine(a(j))
            }
            out.close()
          }
        }, chunkSize)

        FileStream(outFile)"""
}

      infix ("mapRows") (CurriedMethodSignature(List(
        List(
          ("outFile", MString),
          ("inDelim", MString, "unit(\"\\s+\")"), // input delimiter is a regular expression
          ("outDelim", MString, "unit(\"    \")") // output delimiter is an ordinary string
        ),
        List(
          ("func", DenseVector(MString) ==> DenseVector(R))
        )), FileStream), TStringable(R), addTpePars = R) implements composite {
  val self = quotedArg("self")
  s"""$self.map(outFile, preserveOrder = true) { line =>
          val tokens: Rep[ForgeArray[String]] = line.trim.fsplit(inDelim, -1) 
          val tokenVector: Rep[DenseVector[String]] = densevector_fromarray(tokens, true)
          
          
          val outRow: Rep[DenseVector[R]] = func(tokenVector)
          
          if (outRow.length > 0) array_mkstring(outRow.toArray, outDelim) else ""
        }

        FileStream(outFile)"""
}

      // We currently limit this to (String, DenseVector(Double)) pairs, as we need to be able to serialize
      // the representation out to disk efficiently. We could allow the user to supply serializers and
      // deserializers (or use a serializable type class) but that may be more trouble than it is worth.
      // We could also fall back to just using strings, but that is painfully slow.
      infix ("groupRowsBy") (CurriedMethodSignature(List(
        List(
          ("outTable", MString),
          ("delim", MString, "unit(\"\\s+\")"),   // input delimiter is a regular expression
          ("appendToHash", MBoolean, "unit(false)")
        ),
        List(
          ("keyFunc", DenseVector(MString) ==> MString),
          ("valFunc", DenseVector(MString) ==> DenseVector(MDouble))
        )), HashStream(DenseMatrix(MDouble)))) implements composite {
  val self = quotedArg("self")
  s"""def serialize(v: Rep[DenseVector[Double]]): Rep[ForgeArray[Byte]] = {
          val x = ByteBuffer(4+8*v.length)
          x.putInt(v.length)
          x.put(v.toArray, 0, v.length)
          x.unsafeImmutable.array
        }

        

        if (!appendToHash) {
          deleteFile(outTable) 
        }
        val hash = HashStream[DenseMatrix[Double]](outTable, hashMatrixDeserializer)

        $self.processFileChunks({ line =>
          val tokens: Rep[ForgeArray[String]] = line.trim.fsplit(delim, -1) 
          val tokenVector: Rep[DenseVector[String]] = densevector_fromarray(tokens, true)
          val key: Rep[String] = keyFunc(tokenVector)
          val value: Rep[DenseVector[Double]] = valFunc(tokenVector)
          pack((key, value))
        },
        { (a: Rep[ForgeArray[Tup2[String,DenseVector[Double]]]]) =>
          
          
          
          
          
          

          
          
          val chunk = densevector_fromarray(a, true).filter(t => t._2.length > 0)

          
          val dbKeys: Rep[ForgeArray[String]] = chunk.indices.map(i => hashMatrixNewKey(chunk(i)._1, i)).toArray
          val dbValues: Rep[ForgeArray[ForgeArray[Byte]]] = chunk.map(t => serialize(t._2)).toArray

          
          
          
          hash.putAll(dbKeys, dbValues, dbKeys.length)
        })

        hash"""
}

      infix ("reduce") (CurriedMethodSignature(List(List(("zero", T)), List(("func", MString ==> T)), List(("rfunc", (T,T) ==> T))), T), addTpePars = T) implements composite {
          val self = quotedArg("self")
          s"""var acc = zero
$self.processFileChunks(line => func(line), { (a: Rep[ForgeArray[T]]) =>
  val reduced = array_reduce(a, rfunc, zero)
  acc = rfunc(acc, reduced)
})

acc"""
        }
    }
  }

  def importComputeStreamOps() {
    val T = tpePar("T")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val Tup2 = lookupTpe("Tup2")
    val ComputeStream = lookupTpe("ComputeStream")

    data(ComputeStream, ("_numRows", MInt), ("_numCols", MInt), ("_func", MLambda(Tup2(MInt,MInt), T)))

    static (ComputeStream) ("apply", T, (CurriedMethodSignature(List(
      List(
        ("numRows", MInt),
        ("numCols", MInt)
      ),
      List(
        ("func", (MInt,MInt) ==> T)
      )), ComputeStream(T)))) implements allocates(ComputeStream, {
  val arg1 = quotedArg(0)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(1)
  s"""$arg1"""
}, {
  s"""doLambda((t: Rep[Tup2[Int,Int]]) => func(t._1, t._2))"""
})

    val ComputeStreamOps = withTpe(ComputeStream)
    ComputeStreamOps {
      infix ("numRows") (Nil :: MInt) implements getter(0, "_numRows")
      infix ("numCols") (Nil :: MInt) implements getter(0, "_numCols")
      compiler ("stream_func") (Nil :: MLambda(Tup2(MInt,MInt), T)) implements getter(0, "_func")

      infix ("apply") ((MInt, MInt) :: T) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(2)
          s"""val lambda = stream_func($self)
doApply(lambda, pack(($arg1,$arg2)))"""
        }

      infix ("foreach") ((T ==> MUnit) :: MUnit) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""(0::$self.numRows) foreach { i =>
  
  
  IndexVector(0, $self.numCols) foreach { j =>
    $arg1($self(i,j))
  }
}"""
        }

      infix ("foreachRow") ((DenseVectorView(T) ==> MUnit) :: MUnit) implements composite {
          val self = quotedArg("self")
          val arg1 = quotedArg(1)
          s"""val chunkSize = ceil(1000000/$self.numCols) 
val buf = DenseMatrix[T](chunkSize, $self.numCols)
val numChunks = ceil($self.numRows / chunkSize.toDouble)

var chunkIdx = 0
while (chunkIdx < numChunks) {
  val remainingRows = $self.numRows - chunkIdx*chunkSize
  val leftover = if (remainingRows < 0) $self.numRows else remainingRows 
  val rowsToProcess = min(chunkSize, leftover)
  (0::rowsToProcess) foreach { i =>
    
    IndexVector(0, $self.numCols) foreach { j =>
      buf(i,j) = $self(i,j)
    }
    $arg1(buf(i))
  }

  chunkIdx += 1
}"""
        }
    }
  }
}
