package cafex

import scala.math.BigDecimal

class Cafex {

	/**
	 * Total money to pay for the order
	 *
	 * @param items A list of drinks and foods
	 */
	def total(items: List[String]): BigDecimal = items.map { menu.get(_) }.filter {
		x => x.isDefined && (x.get > 0)
	}.map { _.get }.sum

	val menu: Map[String, BigDecimal] = Map(
		"Cola" -> 0.5,
		"Coffee" -> 1,
		"Cheese Sandwich" -> 2,
		"Steak Sandwich" -> 4.5
	)
}

