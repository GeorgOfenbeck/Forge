package ppl.dsl.forge
package dsls
package optiwrangler

import core.{ForgeApplication,ForgeApplicationRunner}

trait ConditionOps {
  this: OptiWranglerDSL =>

  def importConditionOps() {
    val Condition = tpe("Condition")

  	data (Condition, "isEmpty" -> MBoolean, "rowIndices" -> MArray(MInt), "colForStartsWith" -> MString, "valueForStartsWith" -> MString, "colForContains" -> MString, "valueForContains" -> MString)
  	static (Condition) ("apply", Nil, Nil :: Condition) implements allocates (Condition, "unit(true)", {
    s"""array_empty[Int](unit(0))"""
  }, "unit(\"\")", "unit(\"\")",  "unit(\"\")", "unit(\"\")")
  	static (Condition) ("apply", Nil, MethodSignature(List(MBoolean, MArray(MInt), MString, MString, MString, MString), Condition)) implements allocates (Condition, {
  val arg1 = quotedArg(0)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(1)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(2)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(3)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(4)
  s"""$arg1"""
}, {
  val arg1 = quotedArg(5)
  s"""$arg1"""
})

  	val ConditionOps = withTpe (Condition)
  	ConditionOps {
      compiler ("get_is_empty") (Nil :: MBoolean) implements getter(0, "isEmpty")
  		compiler ("get_col_startswith") (Nil :: MString) implements getter(0, "colForStartsWith")
  		compiler ("get_col_contains") (Nil :: MString) implements getter(0, "colForContains")
  		compiler ("get_value_startswith") (Nil :: MString) implements getter(0, "valueForStartsWith")
  		compiler ("get_value_contains") (Nil :: MString) implements getter(0, "valueForContains")
  		compiler ("get_row_indices") (Nil :: MArray(MInt)) implements getter(0, "rowIndices")
  	}
  }
}