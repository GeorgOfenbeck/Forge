package LOWERCASE_DSL_NAME.shallow

object Numeric {
  def numeric_zero[T](implicit num: Numeric[T]): T = num.zero
}
