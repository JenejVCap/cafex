package cafex

import org.scalatest.FunSuite

class CafexSpec extends FunSuite {

  test("empty list returns 0") {
    assert(Cafex.totalBillWithServiceCharge(List()) == 0)
  }

  test("non existent food returns 0") {
    assert(Cafex.totalBillWithServiceCharge(List("dummy")) == 0)
  }

  test("filters bad values correctly") {
    val orders = List("Cola", "Coffee", "Cheese Sandwich", "dummy", "Coffee2")
    assert(Cafex.totalBillWithServiceCharge(orders) == 3.85)
  }

  test("only drinks, no charge") {
    assert(Cafex.totalBillWithServiceCharge(List("Cola", "Coffee")) == 1.5)
  }

  test("cold food adds 10 pct") {
    assert(Cafex.totalBillWithServiceCharge(List("Cheese Sandwich")) == 2.2)
  }

  test("hot food adds 20 pct") {
    assert(Cafex.totalBillWithServiceCharge(List("Steak Sandwich")) == 5.4)
  }

  test("no more than 20 units to be added as service charge") {
    val longList = List.fill(100)("Steak Sandwich")
    assert(Cafex.totalBillWithServiceCharge(longList) == 470)
  }
}
