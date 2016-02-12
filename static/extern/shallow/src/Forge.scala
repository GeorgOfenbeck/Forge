package LOWERCASE_DSL_NAME.shallow

class ForgeExtras[T](val underlying: T) extends ForgeAny {
	def unsafeImmutable: T = underlying //???
}

trait ForgeAny

object Forge {
  def getter[T](obj: ForgeAny, field: T): T = field
  def setter[T](obj: ForgeAny, field: T, value: T): Unit = ???
}