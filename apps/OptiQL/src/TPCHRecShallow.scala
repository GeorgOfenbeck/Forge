import ppl.tests.scalatest._

import optiql.shallow._
import optiql.shallow.classes._
import optiql.shallow.classes.Table._
import optiql.shallow.classes.Rewrite._
import records.Rec

object TPCHQ1RecShallow extends TPCHQ1TraitRecShallow
// object TPCHQ6Interpreter extends OptiQLApplicationInterpreter with TPCHQ6Trait
// object TPCHQ14Interpreter extends OptiQLApplicationInterpreter with TPCHQ14Trait

trait TPCHBaseTraitRecShallow extends TypesRecShallow {

  def printUsage = {
    println("Usage: TPCH"+queryName+" <input directory>")
    sys.exit(-1)
  }

  // //timing decided at staging time so we can fuse across I/O when possible
  // val timeIO: Boolean = System.getProperty("tpch.time.io", "true") != "false"
  // override def tic(in: Any*) = {
  //   if (timeIO) super.tic() //start timing immediately
  //   else super.tic(in:_*) //start timing after input loaded
  // }

  val queryName: String
  
  var tpchDataPath: String = _
  val sep = "\\|"
  def loadLineItems() = Table.fromFile[LineItem](tpchDataPath+"/lineitem.tbl", sep)
  // def loadCustomers() = Table.fromFile[Customer](tpchDataPath+"/customer.tbl", sep)
  // def loadNations() = Table.fromFile[Nation](tpchDataPath+"/nation.tbl", sep)
  // def loadOrders() = Table.fromFile[Order](tpchDataPath+"/orders.tbl", sep)
  // def loadParts() = Table.fromFile[Part](tpchDataPath+"/part.tbl", sep)
  // def loadPartSuppliers() = Table.fromFile[PartSupplier](tpchDataPath+"/partsupp.tbl", sep)
  // def loadRegions() = Table.fromFile[Region](tpchDataPath+"/region.tbl", sep)
  // def loadSuppliers() = Table.fromFile[Supplier](tpchDataPath+"/supplier.tbl", sep)
  
  def query(): Unit
  
  def main(args: Array[String]): Unit = {
    println("TPC-H " + queryName)
    if (args.length < 1) printUsage
    
    tpchDataPath = args(0)
    query()
  }

}


trait TPCHQ1TraitRecShallow extends TPCHBaseTraitRecShallow {

  val queryName = "Q1"
  def query() = {

    val lineItems = loadLineItems()
    // tic(lineItems.size)

    val q = lineItems Where(_.l_shipdate <= Date("1998-12-01")) GroupBy(l => new Tup2(l.l_returnflag,l.l_linestatus)) Select(g => Rec(
        returnFlag = g._1._1,
        lineStatus = g._1._2,
        sumQty = g._2.Sum(_.l_quantity),
        sumBasePrice = g._2.Sum(_.l_extendedprice),
        sumDiscountedPrice = g._2.Sum(l => l.l_extendedprice * (1.0 - l.l_discount)),
        sumCharge = g._2.Sum(l => l.l_extendedprice * (1.0 - l.l_discount) * (1.0 + l.l_tax)), //FIXME: infix_+ fails to resolve automatically
        avgQty = g._2.Average(_.l_quantity),
        avgPrice = g._2.Average(_.l_extendedprice),
        avgDiscount = g._2.Average(_.l_discount),
        countOrder = g._2.Count
      )) OrderBy(asc(_.returnFlag), asc(_.lineStatus))
    
    // // toc(q)
    TablePrinterRecShallow.printAsTable(q, 100)
    TablePrinterRecShallow.writeAsJSON(q, "out.json")
  }
}


// trait TPCHQ6Trait extends TPCHBaseTrait {
//   val queryName = "Q6"

//   def query() = {
//     val lineItems = loadLineItems()
//     tic(lineItems.size)

//     //FIXME: infix_&& fails to resolve automatically
//     val q = lineItems Where (l => infix_&&(l.l_shipdate >= Date("1994-01-01"), infix_&&(l.l_shipdate < Date("1995-01-01"), infix_&&(l.l_discount >= 0.05, infix_&&(l.l_discount <= 0.07, l.l_quantity < 24))))) 
//     val revenue = q.Sum(l => l.l_extendedprice * l.l_discount)

//     toc(revenue)
//     println(revenue)
//   }
// }

// trait TPCHQ14Trait extends TPCHBaseTrait {
//   val queryName = "Q14"

//   def query() = {
//     val parts = loadParts(); val lineItems = loadLineItems()
//     tic(parts.size, lineItems.size)

//     val shippedItems = lineItems.Where(li => li.l_shipdate >= Date("1995-09-01") && li.l_shipdate < Date("1995-10-01"))    
//     val q = parts.Join(shippedItems)(_.p_part_1, _.l_part_1)(
//       (p,l) => new Record { //this post-Join Select is very boilerplate but we need to get the type right
//         val l_extendedprice = l.l_extendedprice
//         val l_discount = l.l_discount
//         val p_type = p.p_type
//       })

//     val promoRevenue = q.Sum(l => if (l.p_type startsWith "PROMO") l.l_extendedprice * (1.0 - l.l_discount) else 0.0)
//     val totalRevenue = q.Sum(l => l.l_extendedprice * (1.0 - l.l_discount))
//     val promoPercentage = 100 * promoRevenue / totalRevenue
//     toc(promoPercentage)
//     println(promoPercentage)
//   }
// }
