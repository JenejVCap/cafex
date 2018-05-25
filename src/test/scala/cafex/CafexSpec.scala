package cafex

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class CafexSpec extends FunSuite with BeforeAndAfter {

  var cafex: Cafex = _
  before {
    cafex = new Cafex
  }

  test("empty list returns 0") {
    assert(cafex.total(List()) == 0)
  }

  test("non existent food returns 0") {
    assert(cafex.total(List("dummy")) == 0)
  }

  test("filters bad values correctly") {
    val orders = List("Cola", "Coffee", "Cheese Sandwich", "dummy", "Coffee2")
    assert(cafex.total(orders) == 3.85)
  }

  test("only drinks, no charge") {
    assert(cafex.total(List("Cola", "Coffee")) == 1.5)
  }

  test("cold food adds 10 pct") {
    assert(cafex.total(List("Cheese Sandwich")) == 2.2)
  }

  test("hot food adds 20 pct") {
    assert(cafex.total(List("Steak Sandwich")) == 5.4)
  }

  test("no more than 20 units to be added as service charge") {
    val longList = List.fill(100)("Steak Sandwich")
    assert(cafex.total(longList) == 470)
  }
}
