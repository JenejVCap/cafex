package cafex

import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode

class Cafex {

  val menu: Map[String, Product] = Map(
    "Cola" -> Product(Constants.drink, Constants.cold, 0.5),
    "Coffee" -> Product(Constants.drink, Constants.hot, 1),
    "Cheese Sandwich" -> Product(Constants.food, Constants.cold, 2),
    "Steak Sandwich" -> Product(Constants.food, Constants.hot, 4.5)
  )

  /**
   * Total money to pay for the order + service charge
   *
   * @param items A list of drinks and foods
   */
  def total(items: List[String]) = {
    val ordered: List[Product] = items.map { menu.get(_) }.filter {
      x => x.isDefined
    }.map { _.get }

    def serviceCharge(products: List[Product], charge: Charges): Int = products match {
      case Nil => Charges.getCharge(charge)
      case x => if (x.head.itemType == Constants.food && x.head.temperatureType == Constants.hot) {
        serviceCharge(x.tail, Charges(charge.onlyDrinks, charge.hasFood, true))
      } else if (x.head.itemType == Constants.food) {
        serviceCharge(x.tail, Charges(charge.onlyDrinks, true, charge.hasHotFood))
      } else {
        serviceCharge(x.tail, Charges(true, charge.hasFood, charge.hasHotFood))
      }
    }

    val chargePct: Int = serviceCharge(ordered, Charges(false, false, false))
    val totalWithoutCharge = items.map { menu.get(_) }.filter {
      x => x.isDefined && (x.getOrElse(Product("", "", 0)).cost > 0)
    }.map { _.get.cost }.sum

    def totalCharge(orderCost: BigDecimal, charge: Int): BigDecimal = {
      val tcharge = (orderCost * (charge * 0.01)).setScale(2, RoundingMode.HALF_EVEN)
      if (tcharge > Constants.maxCharge) Constants.maxCharge else tcharge
    }

    totalWithoutCharge + totalCharge(totalWithoutCharge, chargePct)
  }
}

/**
 * Description of a food/drink in the menu
 */
case class Product(
  itemType: String,
  temperatureType: String,
  cost: BigDecimal
)

object Constants {

  val cold = "cold"
  val hot = "hot"
  val food = "food"
  val drink = "drink"

  val onlyDrinksCharge = 0
  val foodCharge = 10
  val hotFoodCharge = 20

  val maxCharge = 20

}

/**
 * Possible charges.
 *
 * The values are mutually exclusive
 */
case class Charges(
  onlyDrinks: Boolean,
  hasFood: Boolean,
  hasHotFood: Boolean
)

object Charges {

  def getCharge(ss: Charges): Int = {
    if (ss.hasHotFood) Constants.hotFoodCharge
    else if (!ss.hasHotFood && ss.hasFood) Constants.foodCharge
    else Constants.onlyDrinksCharge
  }
}
