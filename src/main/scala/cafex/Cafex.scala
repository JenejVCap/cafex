package cafex

object Cafex {
  def main(args: Array[String]): Unit = {
    println("Just run the tests with sbt test!")
  }

  case class Item(name: String, cost: Double, state: State, itemType: ItemType)

  sealed trait State

  case object Hot extends State

  case object Cold extends State

  sealed trait ItemType

  case object Food extends ItemType

  case object Drink extends ItemType

  val menu: List[Item] = List(
    Item("Cola", 0.50, Cold, Drink),
    Item("Coffee", 1.00, Hot, Drink),
    Item("Cheese Sandwich", 2.00, Cold, Food),
    Item("Steak Sandwich", 4.50, Hot, Food)
  )



  def totalBillWithServiceCharge(items: List[String]): Double = {
    def totalBill(items: List[String], total: Double = 0.0): Double = items match {
      case Nil => total
      case h::t =>
        val cost = menu.collectFirst {
          case item if item.name == h => item.cost
        }.getOrElse(0.0)
        totalBill(t, total + cost)
    }
    val totalNoSC: Double = totalBill(items)
    val itemsAvailable: List[Item] = items.flatMap { it => menu.collectFirst {
      case item if item.name == it => item
    }}
    val hasHotFood: Option[Boolean] = itemsAvailable collectFirst {
      case item if item.state == Hot && item.itemType == Food => true
    }
    val hasFood: Option[Boolean] = itemsAvailable collectFirst {
      case item if item.itemType == Food => true
    }
    val serviceCharge: Double = (hasHotFood, hasFood) match {
      case (Some(_), Some(_)) => 0.20
      case (None, Some(_)) => 0.10
      case _ => 0.0
    }
    val totalWithServiceCharge = if (totalNoSC * serviceCharge > 20) totalNoSC + 20
      else totalNoSC + (totalNoSC * serviceCharge)
    "%.2f".format(totalWithServiceCharge).toDouble
  }
}
