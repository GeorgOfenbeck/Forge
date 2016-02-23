package LOWERCASE_DSL_NAME.shallow

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

/**
  INPUT:
  @NRecord
  case class Region(key: Int, name: String, comment: String)

  OUTPUT:
  case class Region = {
    val key: Int
    val name: String
    val comment: String
  }
  implicit val m:Manifest[Region] = new PimpedRefinedManifest[Region] {
    def create(fields: Seq[(String, Any)]): Region = {
          Region(
            fields(0)._2.asInstanceOf[Int],
            fields(1)._2.asInstanceOf[String],
            fields(2)._2.asInstanceOf[String]
          )
    }
    def fields: List[(String, Manifest[_])] = List(
        ("key",manifest[Int]),
        ("name",manifest[String]),
        ("comment",manifest[String])
      )
    def runtimeClass = classOf[Region]
  }
*/

@compileTimeOnly("Enable macro paradise to expand macro annotations")
final class NRecord extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro NRecord.impl
}

object NRecord {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val list = annottees.map(_.tree).toList // Get annottees and get a tree of this expression
    val tree = list.head // Only the first annottee is interesting, we do not process more than one per annottation
    tree match {
      case cd@ClassDef(mods, className: TypeName, tparams, impl@Template(parents, selfType, bodyList))
        if mods.hasFlag(Flag.CASE) => {
        val (fields, methods) = bodyList.partition{case _:ValDef => true case _ => false}
        if (fields.isEmpty) {
          return c.Expr(q""" throw new Exception("case classes need at least one field in order to be transformed into records")""") //defer Exception to runtime
        }
        if (methods.size != 1) {
          return c.Expr(q""" throw new Exception("case classes with a body (e.g. methods) cannot be transformed into records")""")
        }
        if (tparams.size != 0) {
          return c.Expr(q""" throw new Exception("Type parameters are not supported with record annotation macros")""")
        }

        val createMethodParams = fields.zipWithIndex.map {
          case (ValDef(_, termName, typeIdent, rhs), i) => {
            val index = Literal(Constant(i))
            q"fields($index)._2.asInstanceOf[$typeIdent]"
          }
        }
        val createMethod = q"def create(fields: Seq[(String, Any)]): $className = {${className.toTermName}(..$createMethodParams)}"
//        println(showCode(createMethod))


        val fieldsWithManifest = fields.map {
          case ValDef(_, termName, typeIdent, rhs) => {
            val termNameString = termName.toString
            q""" ($termNameString, manifest[$typeIdent]) """
          }
        }
        val fieldsMethod = q"def fields: List[(String, Manifest[_])] = $fieldsWithManifest"
//        println(showCode(fieldsMethod))

        val runtimeClassMethod = q"def runtimeClass = classOf[$className]"
//        println(showCode(runtimeClassMethod))

//        implicit val m:Manifest[Region] = new PimpedRefinedManifest[Region] {
        val pimpedManifest = q"""
            implicit val m:Manifest[$className] = new PimpedRefinedManifest[$className] {
              $createMethod
              $fieldsMethod
              $runtimeClassMethod
            }
        """
        // println(showCode(pimpedManifest))

        val caseClassAndManifest = q"""
          $tree
          $pimpedManifest
        """
        // println(showCode(caseClassAndManifest))
        c.Expr(caseClassAndManifest)
      }
      case _  => c.Expr(q""" throw new Exception("NRecord annottation can be only used on case classes!")""")
    }
  }
}