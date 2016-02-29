package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiLADSLRunner extends ForgeApplicationRunner with OptiLADSL

trait OptiLADSL extends ForgeApplication
  with ArithOps with HasMinMaxOps with StringableOps with ShapeOps
  with BasicMathOps with RandomOps with IOOps
  with VectorOps with DenseVectorOps with IndexVectorOps with DenseVectorViewOps with SparseVectorOps with SparseVectorViewOps
  with MatrixOps with DenseMatrixOps with DenseMatrixViewOps with SparseMatrixOps
  with ComplexOps with LinAlgOps {

  def dslName = "OptiLA"

  override def addREPLOverride = true

  def specification() = {
    // our selection of Scala ops
    // we don't use Numeric or Fractional, since they are replaced by Arith
    importPrimitives()
    importMisc()
    importCasts()
    importOrdering()
    importStrings()
    importMath()
    importTuples()
    importHashMap()
    importConcurrentHashMap()

    // OptiLA types
    // declare all tpes first, so that they are available to all ops (similar to Delite)
    val T = tpePar("T")
    val DenseVector = tpe("DenseVector", T)
    val DenseVectorView = tpe("DenseVectorView", T)
    val DenseMatrix = tpe("DenseMatrix", T)
    val DenseMatrixView = tpe("DenseMatrixView", T)
    val IndexVector = tpe("IndexVector")
    val IndexWildcard = tpe("IndexWildcard", stage = compile)
    identifier (IndexWildcard) ("*")
    val SparseVector = tpe("SparseVector", T)
    val SparseVectorView = tpe("SparseVectorView", T)
    val SparseMatrix = tpe("SparseMatrix", T)
    val SparseMatrixBuildable = tpe("SparseMatrixBuildable", T)

    // OptiLA ops
    // note that the order matters with respect to 'lookup' calls

    // sneak in a compiler-only range method
    val Range = tpe("Range")
    data(Range, ("start", MInt), ("end", MInt))
    compiler (Range) ("range_start", Nil, Range :: MInt) implements getter(0, "start")
    compiler (Range) ("range_end", Nil, Range :: MInt) implements getter(0, "end")

    noInfixList :::= List("infix_foreach")
    compiler (Range) ("infix_until", Nil, (MInt,MInt) :: Range) implements allocates(Range, quotedArg(0), quotedArg(1))

    // infix_foreach must be compiler only both so that it is not used improperly and to not interfere with other codegen nodes in the library
    // this is a little convoluted unfortunately (because of the restriction on passing structs to codegen nodes)
    compiler (Range) ("infix_foreach", Nil, (Range, MInt ==> MUnit) :: MUnit) implements composite {
      val arg1 = quotedArg(0)
      val arg2 = quotedArg(1)
      s"""range_foreach(range_start($arg1), range_end($arg1), $arg2)"""
    }
    val range_foreach = compiler (Range) ("range_foreach", Nil, (("start",MInt),("end",MInt),("func",MInt ==> MUnit)) :: MUnit)
    impl (range_foreach) (codegen($cala, {
        val start = quotedArg("start")
        val end = quotedArg("end")
        val func = quotedBlock("func", range_foreach, List(s"""i"""))
        s"""var i = $start
while (i < $end) {
  $func
  i += 1
}"""
      }))

    impl (range_foreach) (codegen(cpp, {
        val start = quotedArg("start")
        val end = quotedArg("end")
        val func = quotedBlock("func", range_foreach, List(s"""i"""))
        s"""for(int i=$start ; i<$end ; i++) {
  $func
}"""
      }))

    importArithOps()
    importBasicMathOps()
    importRandomOps()
    importHasMinMaxOps()
    importStringableOps()
    importComplexOps()

    // override default string formatting (numericPrecision is a global defined in extern)
    // we use "" + $a instead of $a.toString to avoid an NPE when explicitly calling toString inside the REPL
    val formatStr = {
      val a = quotedArg(0)
      val f = "(\"% .\"+Global.numericPrecision+\"g\")" // can't escape quotes inside string interpolation scope

s"""
def numericStr[A](x: A) = {
  val s = $f.format(x)
  val padPrefix = (Global.numericPrecision+6) - s.length
  if (padPrefix > 0) " "*padPrefix + s else s
}
if ($a.isInstanceOf[Double] || $a.isInstanceOf[Float]) numericStr($a) else ("" + $a)
"""
    }

    val fmt_str = direct (lookupGrp("FString")) ("optila_fmt_str", T, T :: MString)
    impl (fmt_str) (codegen($cala, formatStr))
    impl (fmt_str) (codegen(cpp, "convert_to_string<" +  unquotes("remapWithRef("+opArgPrefix+"0.tp)") + " >(" + quotedArg(0) + ")"))

    compiler (lookupGrp("FString")) ("optila_padspace", Nil, MString :: MString) implements composite {
        val arg1 = quotedArg(0)
        s""""  " + $arg1"""
      }

    importIndexVectorOps()
    importDenseVectorViewOps()
    importDenseVectorOps()
    importDenseMatrixOps()
    importDenseMatrixViewOps()
    importSparseVectorOps()
    importSparseVectorViewOps()
    importSparseMatrixOps()
    importVecMatConstructor()
    importIOOps()
    importLinAlgOps()
    importShapeOps()

    // native libs
    extern(grp("BLAS"))
    extern(grp("LAPACK"))

    // rewrites
    extern(grp("Rewrite"), targets = Nil)
    extern(grp("Distributed"), targets = List($cala))
  }

  def importVecMatConstructor() {
    val DenseVector = lookupTpe("DenseVector")
    val IndexVector = lookupTpe("IndexVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val T = tpePar("T")

    // vector constructor (0 :: end) { ... }
    noSourceContextList ::= "::" // surpress SourceContext implicit because it interferes with the 'apply' method being immediately callable
    infix (IndexVector) ("::", Nil, ((("end", MInt), ("start", MInt)) :: IndexVector)) implements composite {
  val start = quotedArg("start")
  val end = quotedArg("end")
  s"""IndexVector($start, $end)"""
}

    // should add apply directly to IndexVector, otherwise we have issues with ambiguous implicit conversions
    infix (IndexVector) ("apply", T, (IndexVector, MInt ==> T)  :: DenseVector(T)) implements composite {
  val arg1 = quotedArg(0)
  val arg2 = quotedArg(1)
  s"""$arg1.map($arg2)"""
}

    // matrix constructor (0::numRows,0::numCols) { ... }
    infix (IndexVector) ("apply", T, (CTuple2(IndexVector,IndexVector), (MInt,MInt) ==> T) :: DenseMatrix(T)) implements composite {
        val arg1 = quotedArg(0)
        val arg2 = quotedArg(1)
        s"""val (rowIndices, colIndices) = $arg1


val size = rowIndices.length*colIndices.length
val outData = array_fromfunction(size, i => {
  val (rowIndex, colIndex) = unpack(matrix_shapeindex(i, colIndices.length))
  $arg2(rowIndices(rowIndex),colIndices(colIndex))
})
densematrix_fromarray(outData,rowIndices.length,colIndices.length)"""
      }

    val IndexWildcard = lookupTpe("IndexWildcard", stage = compile)

    // specifying both DenseVector and DenseVectorViews as rhs arguments seems to make this ambiguous;
    // one alternative is to use an IsVector type class for the function return type, instead of overloading.
    // currently, we just let DenseVectorViews implicitly convert to DenseVector, which will cause overhead
    // in the lib implementation, but should fuse away in the Delite implementation.

    for (rhs <- List(DenseVector(T)/*, DenseVectorView(T))*/)) {
      infix (IndexVector) ("apply", T, (CTuple2(IndexVector,IndexWildcard), MInt ==> rhs) :: DenseMatrix(T)) implements composite {
          val arg1 = quotedArg(0)
          val arg2 = quotedArg(1)
          s"""val rowIndices = $arg1._1





fassert(rowIndices.length > 0, "error: matrix constructor with empty indices")
val rowVectors = rowIndices.map(i => $arg2(i))
val numCols = rowVectors(0).length
(0::rowVectors.length, 0::numCols) { (i,j) => rowVectors(i).apply(j) }"""
        }

      infix (IndexVector) ("apply", T, (CTuple2(IndexWildcard,IndexVector), MInt ==> rhs) :: DenseMatrix(T)) implements composite {
          val arg1 = quotedArg(0)
          val arg2 = quotedArg(1)
          s"""val colIndices = $arg1._2



fassert(colIndices.length > 0, "error: matrix constructor with empty indices")
val colVectors = colIndices.map(i => $arg2(i))
val numRows = colVectors(0).length
(0::numRows, 0::colVectors.length) { (i,j) => colVectors(j).apply(i) }"""
        }
    }
  }
}
