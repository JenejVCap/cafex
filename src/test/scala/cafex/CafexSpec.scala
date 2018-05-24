package cafex

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class CafexSpec extends FunSuite with BeforeAndAfter {

  var cafex: Cafex = _
  before {
    cafex = new Cafex
  }

  test("non existent food returns 0") {
    assert(cafex.total(List("dummy")) == 0)
  }

  test("filters bad values correctly") {
    val orders = List("Cola", "Coffee", "Cheese Sandwich", "asd", "Coffee2")
    assert(cafex.total(orders) == 3.5)
  }

  test("empty list returns 0") {
    assert(cafex.total(List()) == 0)
  }
}
