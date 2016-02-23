import ppl.tests.scalatest._

import optiql.shallow._
import optiql.shallow.classes._
import optiql.shallow.classes.Table._
import optiql.shallow.classes.Rewrite._

object QuickMacroTest extends QuickMacroTestTrait

trait QuickMacroTestTrait {

  @NRecord
  case class Region(key: Int, name: String, comment: String)
  
  def main(args: Array[String]): Unit = {
  }
}