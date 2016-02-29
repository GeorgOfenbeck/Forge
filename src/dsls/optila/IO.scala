package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait IOOps {
  this: OptiLADSL =>

  def importIOOps() {
    val IO = grp("LAio") // avoid conflict with IOOps in LMS

    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")

    // -- input

    compiler (IO) ("optila_todouble", Nil, MString :: MDouble) implements composite {
        val arg1 = quotedArg(0)
        s"""if ($arg1 == "Inf") INF
else if ($arg1 == "-Inf") nINF
else $arg1.toDouble"""
      }

    /**
     * For fusion and cluster execution, reads should be pure. however, in that case we need a different way to order them with respect to writes / deletes.
     * one solution would be to implicitly convert strings to mutable file objects, and (manually) CSE future conversions to return the original mutable object.
     *
     * Currently, reading and writing the same file in the same program is not supported, unless there is an alternate dependency chain from the output
     * being written to the input (e.g. the output vector or matrix explicitly depends on the one being read).
     */

    direct (IO) ("readVector", Nil, ("path",MString) :: DenseVector(MDouble)) implements composite {
  val path = quotedArg("path")
  s"""readVector[Double]($path, (s: Rep[String]) => optila_todouble(s))"""
}

    direct (IO) ("readMatrix", Nil, ("path", MString) :: DenseMatrix(MDouble)) implements composite {
      val path = quotedArg("path")
      s"""readMatrix[Double]($path, (s: Rep[String]) => optila_todouble(s))"""
    }
    direct (IO) ("readMatrix", Nil, (("path", MString), ("delim", MString)) :: DenseMatrix(MDouble)) implements composite {
  val path = quotedArg("path")
  val delim = quotedArg("delim")
  s"""readMatrix[Double]($path, (s: Rep[String]) => optila_todouble(s), $delim)"""
}

    val Elem = tpePar("Elem")

    // Simplest version simply passes the string to the schemaBldr function
    direct (IO) ("readVector", Elem, MethodSignature(List(("path",MString),("schemaBldr", MString ==> Elem)), DenseVector(Elem))) implements composite {
        val path = quotedArg("path")
        s"""val a = ForgeFileReader.readLines($path){ line => schemaBldr(line) }
densevector_fromarray(a, true)"""
      }

    // Lines can also be parsed with a custom delimiter (default whitespace)
    direct (IO) ("readVectorAndParse", Elem, MethodSignature(List(("path",MString),("schemaBldr",DenseVector(MString) ==> Elem),("delim",MString,"unit(\"\\s+\")")), DenseVector(Elem))) implements composite {
        val path = quotedArg("path")
        s"""val a = ForgeFileReader.readLines($path){ line =>
  val tokens = line.trim.fsplit(delim, -1) 
  val tokenVector = densevector_fromarray(tokens, true)
  schemaBldr(tokenVector)
}
densevector_fromarray(a, true)"""
      }

    // Simple version allows converting each element consistently
    direct (IO) ("readMatrix", Elem, MethodSignature(List(("path",MString),("schemaBldr",MString ==> Elem),("delim",MString,"unit(\"\\s+\")")), DenseMatrix(Elem))) implements composite {
        val path = quotedArg("path")
        s"""val a = ForgeFileReader.readLinesFlattened($path){ line:Rep[String] =>
  val tokens = line.trim.fsplit(delim, -1) 
  array_fromfunction(tokens.length, i => schemaBldr(tokens(i)))
}
val numCols = array_length(readFirstLine(path).trim.fsplit(delim, -1))
densematrix_fromarray(a, array_length(a) / numCols, numCols)"""
      }

    // This version allows parsing columns differently
    // Bad rows can be ignored by returning an empty vector (unless we get an exception during splitting -- we should handle that here)
    direct (IO) ("readMatrixAndParse", Elem, MethodSignature(List(("path",MString),("schemaBldr",DenseVector(MString) ==> DenseVector(Elem)),("delim",MString,"unit(\"\\s+\")")), DenseMatrix(Elem))) implements composite {
        val path = quotedArg("path")
        s"""val a = ForgeFileReader.readLinesFlattened($path){ line:Rep[String] =>
  val tokens = line.trim.fsplit(delim, -1) 
  val tokenVector = densevector_fromarray(tokens, true)
  val outRow = schemaBldr(tokenVector)
  outRow.toArray
}
val numCols = schemaBldr(densevector_fromarray(readFirstLine(path).trim.fsplit(delim, -1), true)).length
densematrix_fromarray(a, array_length(a) / numCols, numCols)"""
      }

    val readfirstline = compiler (IO) ("readFirstLine", Nil, ("path",MString) :: MString) implements composite {
        s"""val in = ForgeFileInputStream(path)
val line = in.readLine()
in.close()
line"""
      }

    // -- output

    direct (IO) ("writeVector", Elem withBound TStringable, (("v",DenseVector(Elem)),("path",MString)) :: MUnit, effect = simple) implements composite {
        s"""ForgeFileWriter.writeLines(path, v.length) { i =>
  v(i).makeStr
}"""
      }

    direct (IO) ("writeMatrix", Elem withBound TStringable, MethodSignature(List(("m",DenseMatrix(Elem)),("path",MString),("delim",MString,"unit(\"    \")")), MUnit), effect = simple) implements composite {
        s"""ForgeFileWriter.writeLines(path, m.numRows) { i =>
  

  
  
  array_mkstring(m(i).toArray, delim)
}"""
      }


    // -- utility

    val deleteFile = direct (IO) ("deleteFile", Nil, MString :: MUnit, effect = simple)
    impl (deleteFile) (codegen($cala, {
        val arg1 = quotedArg(0)
        s"""val f = new java.io.File($arg1)
if (f.exists) {
  if (f.isDirectory) org.apache.commons.io.FileUtils.deleteDirectory(f) 
  else f.delete()
}
()"""
      }))
    impl (deleteFile) (codegen(cpp, {
  val arg1 = quotedArg(0)
  s"""DeliteFileSystem::deleteRecursive($arg1)"""
}))

    direct (IO) ("fileExists", Nil, MString :: MBoolean, effect = simple) implements codegen($cala, {
        val arg1 = quotedArg(0)
        s"""val f = new java.io.File($arg1)
f.exists"""
      })

  }
}
