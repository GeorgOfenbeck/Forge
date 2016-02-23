import optiql.shallow._
import optiql.shallow.classes._
import ppl.tests.scalatest._
import scala.virtualization.lms.common.Record

import ForgeArray._
import ForgeArrayBuffer._
import ForgeHashMap._
import Numeric._
import Rewrite._
import records.Rec
import optiql.shallow.classes.Table._


trait TestRecShallow {

  @NRecord
  case class Item(
    val id: Int,
    val quantity: Int,
    val price: Double,
    val status: Char
  )

  // def Item(_id: Int, _quantity: Int, _price: Double, _status: Char) = Rec (
  //   id = _id,
  //   quantity = _quantity,
  //   price = _price,
  //   status = _status
  // )

  lazy val items = Table(Item(0, 10, 2.49, 'N'), Item(1, 0, 49.95, 'B'), Item(2, 1000, 0.99, 'N'), Item(3, 18, 5.99, 'S'))
  val itemsSize = 4

  lazy val items2 = Table(Item(0, 10, 2.49, 'N'), Item(1, 0, 49.95, 'B'), Item(2, 1000, 0.99, 'N'), Item(3, 18, 5.99, 'S'), Item(4, 22, 2.99, 'N'))

  // lazy val emptyTable = Table[Item]()

  //allow some floating point error
  def approx(lhs: Double, rhs: Double) = {
    def abs(value: Double) = if (value < 0) 0-value else value
    abs(lhs - rhs) < .001
  }

}


object QueryableSelectRunnerRecShallow extends ForgeTestRunnerShallow with TestRecShallow {

  def main() = {
    val scalarResult = items Select (item => item.id)

    collect(scalarResult.size == itemsSize)
    for (i <- 0 until itemsSize) {
      collect(scalarResult(i) == i)
    }

    val recordResult = items Select(item => Rec(
      id = item.id,
      maxRevenue = item.quantity * item.price
    ))

    collect(recordResult.size == itemsSize)
    for (i <- 0 until itemsSize) {
      collect(recordResult(i).id == i)
    }

    collect(approx(recordResult(0).maxRevenue, 24.9))
    collect(recordResult(1).maxRevenue == 0)
    collect(approx(recordResult(2).maxRevenue, 990))
    collect(approx(recordResult(3).maxRevenue, 107.82))
    mkReportRepless
  }
}

object QueryableWhereRunnerRecShallow extends ForgeTestRunnerShallow with TestRecShallow {

  def main() = {
    val result = items Where(_.status == 'N') Select(item => Rec(
      id = item.id,
      maxRevenue = item.quantity * item.price
    ))

    collect(result.size == 2)

    collect(result.First.id == 0)
    collect(result.Last.id == 2)

    collect(approx(result.First.maxRevenue, 24.9))
    collect(approx(result.Last.maxRevenue, 990))

    val res2 = items Where(_.status == 'T') Select(item => item.id)
    collect(res2.size == 0)
    mkReportRepless
  }
}

object QueryableReduceRunnerRecShallow extends ForgeTestRunnerShallow with TestRecShallow {
  def main() = {
    val sumQuantity = items Sum(_.quantity)
    collect(sumQuantity == 1028)

    val minQuantity = items Min(_.price)
    collect(minQuantity == 0.99)

    val maxId = items Max(_.id)
    collect(maxId == 3)

    val avgPrice = items Average(_.price)
    collect(approx(avgPrice, 14.855))
    mkReportRepless
  }
}

object QueryableGroupByReduceRunnerRecShallow extends ForgeTestRunnerShallow with TestRecShallow {
  def main() = {
    // val res1 = items GroupBy(_.status) Select(g => Rec (
    //   "status" -> g._1,
    //   "sumQuantity" -> g._2.Sum(_.quantity),
    //   "minPrice" -> g._2.Min(_.price),
    //   "count" -> g._2.Count
    // ))
    val res1 = items GroupBy(_.status) Select(g => Rec(
      status = g._1,
      sumQuantity = g._2.Sum(_.quantity),
      minPrice = g._2.Min(_.price),
      count = g._2.Count
    ))
    collect(res1.size == 3)
    collect(res1(0).status == 'N' && res1(0).sumQuantity == 1010 && res1(0).minPrice == 0.99 && res1(0).count == 2)
    collect(res1(1).status == 'B' && res1(1).sumQuantity == 0 && res1(1).minPrice == 49.95 && res1(1).count == 1)
    collect(res1(2).status == 'S' && res1(2).sumQuantity == 18 && res1(2).minPrice == 5.99 && res1(2).count == 1)

    val res2 = items Where(_.quantity > 0) GroupBy(_.status) Select(g => Rec(
      status = g._1,
      sumQuantity = g._2.Sum(_.quantity),
      maxQuantity = g._2.Max(_.quantity),
      avgPrice = g._2.Average(_.price),
      count = g._2.Count
    ))

    collect(res2.size == 2)
    collect(res2.First.status == 'N' && res2.First.sumQuantity == 1010 && res2.First.maxQuantity == 1000 && approx(res2.First.avgPrice, 1.74) && res2.First.count == 2)
    collect(res2.Last.status == 'S' && res2.Last.sumQuantity == 18 && res2.Last.maxQuantity == 18 && approx(res2.Last.avgPrice, 5.99) && res2.Last.count == 1)

    val res3 = items Where(_.status == 'T') GroupBy(_.status) Select(g => g._1)
    collect(res3.size == 0)

    //val res4 = emptyTable GroupBy (_.status) Select(g => g.key) //FIXME: empty tables are marked mutable by the effects system
    //collect(res4.size == 0)
    mkReportRepless
  }
}

object QueryableGroupByRunnerRecShallow extends ForgeTestRunnerShallow with TestRecShallow {
  def main() = {
    val res = items GroupBy(_.status) SelectMany(g => g._2.Select(_.quantity))
    collect(res.size == items.size)
    collect(res(0) == 10) //N
    collect(res(1) == 1000) //N
    collect(res(2) == 0) //B
    collect(res(3) == 18) //S
    mkReportRepless
  }
}

object QueryableSortRunnerRecShallow extends ForgeTestRunnerShallow with TestRecShallow {
  def main() = {
    val sort1 = items OrderBy(asc(_.id))
    for (i <- 0 until itemsSize) {
      collect(sort1(i).id == i)
    }

    val sort2 = items OrderBy(asc(_.quantity), desc(_.price))
    collect(sort2(0).quantity == 0)
    collect(sort2(1).quantity == 10)
    collect(sort2(2).quantity == 18)
    collect(sort2(3).quantity == 1000)

    val sort3 = items OrderBy(desc(_.status), asc(_.quantity))
    collect(sort3(0).status == 'S')
    collect(sort3(1).status == 'N' && sort3(1).quantity == 10)
    collect(sort3(2).status == 'N' && sort3(2).quantity == 1000)
    collect(sort3(3).status == 'B')
    mkReportRepless
  }
}

object QueryableJoinRunnerRecShallow extends ForgeTestRunnerShallow with TestRecShallow {
  def main() = {
    val res = items.Join(items2)(_.id, _.id)((a,b) => Rec(
      id = a.id,
      quantity = b.quantity
    ))

    collect(res.size == items.size)
    collect(res(0).id == 0 && res(0).quantity == 10)
    collect(res(1).id == 1 && res(1).quantity == 0)
    collect(res(2).id == 2 && res(2).quantity == 1000)
    collect(res(3).id == 3 && res(3).quantity == 18)
    mkReportRepless
  }
}


class QuerySuiteRecShallow extends ForgeSuiteShallow {
  def testSelect() { runTest(QueryableSelectRunnerRecShallow) }
  def testWhere() { runTest(QueryableWhereRunnerRecShallow) }
  def testReduce() { runTest(QueryableReduceRunnerRecShallow) }
  def testGroupBy() { runTest(QueryableGroupByRunnerRecShallow) }
  def testGroupByReduce() { runTest(QueryableGroupByReduceRunnerRecShallow) }
  def testSort() { runTest(QueryableSortRunnerRecShallow) }
  def testJoin() { runTest(QueryableJoinRunnerRecShallow) }
}

// class QuerySuiteInterpreter extends ForgeSuiteInterpreter {
//   def testSelect() { runTest(QueryableSelectRunnerI) }
//   def testWhere() { runTest(QueryableWhereRunnerI) }
//   def testReduce() { runTest(QueryableReduceRunnerI) }
//   def testGroupBy() { runTest(QueryableGroupByRunnerI) }
//   def testGroupByReduce() { runTest(QueryableGroupByReduceRunnerI) }
//   def testSort() { runTest(QueryableSortRunnerI) }
//   def testJoin() { runTest(QueryableJoinRunnerI) }
// }

// class QuerySuiteCompiler extends ForgeSuiteCompiler {
//   def testSelect() { runTest(QueryableSelectRunnerC) }
//   def testWhere() { runTest(QueryableWhereRunnerC) }
//   def testReduce() { runTest(QueryableReduceRunnerC) }
//   def testGroupBy() { runTest(QueryableGroupByRunnerC) }
//   def testGroupByReduce() { runTest(QueryableGroupByReduceRunnerC) }
//   def testSort() { runTest(QueryableSortRunnerC) }
//   def testJoin() { runTest(QueryableJoinRunnerC) }
// }
