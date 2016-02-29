package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

trait MLIOOps {
  this: OptiMLDSL =>

  lazy val IO = grp("MLio")

  def importMLIOOps() {
    importFactorIOOps()
    importARFFOps()
  }

  def importFactorIOOps() {
    val T = tpePar("T")
    val DenseVector = lookupTpe("DenseVector")
    val Tup3 = lookupTpe("Tup3")
    val Tup4 = lookupTpe("Tup4")

    // -- temporary: use java.io.DataInputStream to read binary format, until Delite supports a fixed-length binary reader

    val DataInputStream = tpe("java.io.DataInputStream")
    compiler (IO) ("datainputstream_new", Nil, ("path",MString) :: DataInputStream, effect = simple) implements codegen ($cala, {
        val path = quotedArg("path")
        s"""new java.io.DataInputStream(new java.io.FileInputStream($path))"""
      })

    infix (IO) ("available", Nil, DataInputStream :: MInt, effect = simple) implements codegen ($cala, {
        val arg1 = quotedArg(0)
        s"""$arg1.available()"""
      })

    // infix close clashes with LMS IOOps
    infix (IO) ("fclose", Nil, DataInputStream :: MUnit, effect = simple) implements codegen ($cala, {
        val arg1 = quotedArg(0)
        s"""$arg1.close()"""
      })

    infix (IO) ("readShort", Nil, DataInputStream :: MShort, effect = simple) implements codegen ($cala, {
        val arg1 = quotedArg(0)
        s"""$arg1.readShort()"""
      })

    infix (IO) ("readInt", Nil, DataInputStream :: MInt, effect = simple) implements codegen ($cala, {
        val arg1 = quotedArg(0)
        s"""$arg1.readInt()"""
      })

    infix (IO) ("readLong", Nil, DataInputStream :: MLong, effect = simple) implements codegen ($cala, {
        val arg1 = quotedArg(0)
        s"""$arg1.readLong()"""
      })

    infix (IO) ("readDouble", Nil, DataInputStream :: MDouble, effect = simple) implements codegen ($cala, {
        val arg1 = quotedArg(0)
        s"""$arg1.readDouble()"""
      })

    infix (IO) ("readBoolean", Nil, DataInputStream :: MBoolean, effect = simple) implements codegen ($cala, {
        val arg1 = quotedArg(0)
        s"""$arg1.readBoolean()"""
      })

    ()
  }

  def importARFFOps() {
  	val Row = tpePar("Row")
  	val DenseVector = lookupTpe("DenseVector")

  	direct (IO) ("readARFF", Row, MethodSignature(List(("path",MString),("schemaBldr",DenseVector(MString) ==> Row)), DenseVector(Row)), effect = simple) implements composite {
    val path = quotedArg("path")
    s"""val lines = densevector_fromarray(ForgeFileReader.readLines($path){ line => line.trim }, true).mutable

	

	val start = lines find { _ == "@DATA" }
	if (start.length < 1) fatal("could not find @DATA tag in ARFF file: " + $path)
	val body = lines.drop(start(0)+1).filter(!_.startsWith("%")).mutable
	body map { s => schemaBldr(densevector_fromarray(s.fsplit(","), true)) }"""
  }
  }
}
