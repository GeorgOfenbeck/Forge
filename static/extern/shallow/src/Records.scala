package LOWERCASE_DSL_NAME.shallow

import scala.reflect.Manifest
import scala.virtualization.lms.common._
import scala.collection.mutable.{ArrayBuffer, HashMap}

object Record {

  //case class extends Record appears to be broken (scalac bug)
  class RecordImpl extends scala.virtualization.lms.common.Record {
    val fields: HashMap[String,Any] = new HashMap()
    val fieldNames: ArrayBuffer[String] = new ArrayBuffer() //maintains declared field order

    //records have structural equality
    override def equals(other: Any): Boolean = other match {
      case that: RecordImpl => this.fields == that.fields
      case _ => false
    }

    override def hashCode: Int = fields.##
  }

  class RecordOps(record: Record) {
    def selectDynamic[T:Manifest](field: String): T = record_select[T](record, field)
  }
  implicit def recordToRecordOps(record: Record) = new RecordOps(record)

  def __new[T:Manifest](args: (String, Boolean, T => _)*): T = record_new(args)

  def field[T:Manifest](struct: Any,index: String): T = record_select(struct.asInstanceOf[Record], index)

  def record_new[T:Manifest](fields: Seq[(String, Boolean, T => _)]): T = {
    val recordImpl = (new RecordImpl).asInstanceOf[RecordImpl]
    val recordRep = recordImpl.asInstanceOf[T]
    for ((name, isVar, rhs) <- fields) {
      val value = rhs(recordRep)
      //println(value, value.getClass.getSimpleName)
      recordImpl.fields.asInstanceOf[HashMap[String,Any]] += Pair(name,value.asInstanceOf[Any])
      recordImpl.fieldNames.asInstanceOf[ArrayBuffer[String]] += name
    }
    recordRep
  }

  def record_select[T:Manifest](record: Record,field: String): T = {
    record.asInstanceOf[RecordImpl].fields.get(field) match {
      case Some(f) =>
        //println("found field " + f)
        f.asInstanceOf[T]
      case None =>
        sys.error("field " + field + " does not exist")
    }
  }
}




