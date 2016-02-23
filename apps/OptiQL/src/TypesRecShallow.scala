import optiql.shallow._
import optiql.shallow.classes._
import optiql.shallow.classes.Rewrite._

trait TypesRecShallow {

  @NRecord
  case class LineItem(
    val l_orderkey: Int,
    val l_partkey: Int,
    val l_suppkey: Int,
    val l_linenumber: Int,
    val l_quantity: Double,
    val l_extendedprice: Double,
    val l_discount: Double,
    val l_tax: Double,
    val l_returnflag: Char,
    val l_linestatus: Char,
    val l_shipdate: Date,
    val l_commitdate: Date,
    val l_receiptdate: Date,
    val l_shipinstruct: String,
    val l_shipmode: String,
    val l_comment: String
  )

  // implicit val m:Manifest[LineItem] = new PimpedRefinedManifest[LineItem] {
  //   def create(fields: Seq[(String, Any)]): LineItem = {
  //         LineItem(
  //           fields(0)._2.asInstanceOf[Int],
  //           fields(1)._2.asInstanceOf[Int],
  //           fields(2)._2.asInstanceOf[Int],
  //           fields(3)._2.asInstanceOf[Int],

  //           fields(4)._2.asInstanceOf[Double],
  //           fields(5)._2.asInstanceOf[Double],
  //           fields(6)._2.asInstanceOf[Double],
  //           fields(7)._2.asInstanceOf[Double],

  //           fields(8)._2.asInstanceOf[Char],
  //           fields(9)._2.asInstanceOf[Char],

  //           fields(10)._2.asInstanceOf[Date],
  //           fields(11)._2.asInstanceOf[Date],
  //           fields(12)._2.asInstanceOf[Date],

  //           fields(13)._2.asInstanceOf[String],
  //           fields(14)._2.asInstanceOf[String],
  //           fields(15)._2.asInstanceOf[String]
  //         )
  //   }
  //   def fields: List[(String, Manifest[_])] = List(
  //       ("l_orderkey",manifest[Int]),
  //       ("l_partkey",manifest[Int]),
  //       ("l_suppkey",manifest[Int]),
  //       ("l_linenumber",manifest[Int]),

  //       ("l_quantity",manifest[Double]),
  //       ("l_extendedprice",manifest[Double]),
  //       ("l_discount",manifest[Double]),
  //       ("l_tax",manifest[Double]),

  //       ("l_returnflag",manifest[Char]),
  //       ("l_commitdate",manifest[Char]),

  //       ("l_shipdate",manifest[Date]),
  //       ("l_suppkey",manifest[Date]),
  //       ("l_receiptdate",manifest[Date]),

  //       ("l_shipinstruct",manifest[String]),
  //       ("l_shipmode",manifest[String]),
  //       ("l_comment",manifest[String])
  //     )
  //   def runtimeClass = classOf[LineItem]
  // }

  // type LineItem = {
  //   val l_orderkey: Int
  //   val l_partkey: Int
  //   val l_suppkey: Int
  //   val l_linenumber: Int
  //   val l_quantity: Double
  //   val l_extendedprice: Double
  //   val l_discount: Double
  //   val l_tax: Double
  //   val l_returnflag: Char
  //   val l_linestatus: Char
  //   val l_shipdate: Date
  //   val l_commitdate: Date
  //   val l_receiptdate: Date
  //   val l_shipinstruct: String
  //   val l_shipmode: String  
  //   val l_comment: String
  // }

  // def LineItem(orderKey: Int, partKey: Int, supplierKey: Int, lineNumber: Int, quantity: Double,
  //              extendedPrice: Double, discount: Double, tax: Double, returnFlag: Char,
  //              lineStatus: Char, shipDate: Date, commitDate: Date, receiptDate: Date,
  //              shipInstructions: String, shipMode: String, comment: String): LineItem = new {
  //   val l_orderkey = orderKey
  //   val l_partkey = partKey
  //   val l_suppkey = supplierKey
  //   val l_linenumber = lineNumber
  //   val l_quantity = quantity
  //   val l_extendedprice = extendedPrice
  //   val l_discount = discount
  //   val l_tax = tax
  //   val l_returnflag = returnFlag
  //   val l_linestatus = lineStatus
  //   val l_shipdate = shipDate
  //   val l_commitdate = commitDate
  //   val l_receiptdate = receiptDate
  //   val l_shipinstruct = shipInstructions
  //   val l_shipmode = shipMode
  //   val l_comment = comment
  // }

  // type Customer = {
  //   val c_custkey: Int
  //   val c_name: String
  //   val c_address: String
  //   val c_nationkey: Int
  //   val c_phone: String
  //   val c_acctbal: Double
  //   val c_mktsegment: String
  //   val c_comment: String
  // }

  // def Customer(key: Int, name: String, address: String, nationKey: Int, phone: String,
  //              acctBal: Double, marketSegment: String, comment: String): Customer = new {
  //   val c_custkey = key
  //   val c_name = name
  //   val c_address = address
  //   val c_nationkey = nationKey
  //   val c_phone = phone
  //   val c_acctbal = acctBal
  //   val c_mktsegment = marketSegment
  //   val c_comment = comment
  // }

  // type Nation = {
  //   val n_nationkey: Int
  //   val n_name: String
  //   val n_regionkey: Int
  //   val n_comment: String
  // }

  // def Nation(key: Int, name: String, regionKey: Int, comment: String): Nation = new {
  //   val n_nationkey = key
  //   val n_name = name
  //   val n_regionkey = regionKey
  //   val n_comment = comment
  // }

  // type Order = {
  //   val o_orderkey: Int
  //   val o_custkey: Int
  //   val o_orderstatus: Char
  //   val o_totalprice: Double
  //   val o_orderdate: Date
  //   val o_orderpriority: String
  //   val o_clerk: String
  //   val o_shippriority: Int
  //   val o_comment: String
  // }

  // def Order(key: Int, customerKey: Int, orderStatus: Char, totalPrice: Double,
  //           orderDate: Date, orderPriority: String, clerk: String, shipPriority: Int,
  //           comment: String): Order = new {
  //   val o_orderkey = key
  //   val o_custkey = customerKey
  //   val o_orderstatus = orderStatus
  //   val o_totalprice = totalPrice
  //   val o_orderdate = orderDate
  //   val o_orderpriority = orderPriority
  //   val o_clerk = clerk
  //   val o_shippriority = shipPriority
  //   val o_comment = comment
  // }

  // type Part = {
  //   val p_partkey: Int
  //   val p_name: String
  //   val p_mfgr: String
  //   val p_brand: String
  //   val p_type: String
  //   val p_size: Int
  //   val p_container: String
  //   val p_retailprice: Double
  //   val p_comment: String
  // }

  // def Part(key: Int, name: String, manufacturer: String, brand: String, partType: String,
  //          size: Int, container: String, retailPrice: Double, comment: String): Part = new {
  //   val p_partkey = key
  //   val p_name = name
  //   val p_mfgr = manufacturer
  //   val p_brand = brand
  //   val p_type = partType
  //   val p_size = size
  //   val p_container = container
  //   val p_retailprice = retailPrice
  //   val p_comment = comment
  // }

  // type PartSupplier = {
  //   val ps_partkey: Int
  //   val ps_suppkey: Int
  //   val ps_availqty: Int
  //   val ps_supplycost: Double
  //   val ps_comment: String
  // }

  // def PartSupplier(partKey: Int, supplierKey: Int, availableQty: Int,
  //                  supplyCost: Double, comment: String): PartSupplier = new {
  //   val ps_partkey = partKey
  //   val ps_suppkey = supplierKey
  //   val ps_availqty = availableQty
  //   val ps_supplycost = supplyCost
  //   val ps_comment = comment
  // }

  // type Region = {
  //   val r_regionkey: Int
  //   val r_name: String
  //   val r_comment: String
  // }

  // def Region(key: Int, name: String, comment: String): Region = new {
  //   val r_regionkey = key
  //   val r_name = name
  //   val r_comment = comment
  // }

  // type Supplier = {
  //   val s_suppkey: Int
  //   val s_name: String
  //   val s_address: String
  //   val s_nationkey: Int
  //   val s_phone: String
  //   val s_acctbal: Double
  //   val s_comment: String
  // }

  // def Supplier(key: Int, name: String, address: String, nationKey: Int,
  //              phone: String, acctBal: Double, comment: String): Supplier = new {
  //   val s_suppkey = key
  //   val s_name = name
  //   val s_address = address
  //   val s_nationkey = nationKey
  //   val s_phone = phone
  //   val s_acctbal = acctBal
  //   val s_comment = comment
  // }

}
