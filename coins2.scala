object Application extends App {
  type Coin = (Int,String)

  trait DenominatedUnits {
    def addCoin(name: String, value: Int) : DenominatedUnits
    def distribute(pence: Int) : Map[String, Int]
  }

  class CurrencySystem(known_coins: List[Coin]) extends DenominatedUnits {
    override def toString() : String = known_coins.mkString("\n")

    def largestDenomination(e1: Coin, e2: Coin) : Boolean =
      e1._1 > e2._1

    def addCoin(name: String, value: Int) : CurrencySystem =
        new CurrencySystem((value, name) :: known_coins)

    def distribute(value: Int) : Map[String, Int] = {
      // first lets create a bag of coin denominations
      val sorted_coins = known_coins.sortWith(largestDenomination)
      val moneyBag: Map[String, Int] = sorted_coins.foldLeft(Map[String, Int]())(
        (totals, coin) => (totals + ((coin._2 -> 0)))) + ("pence" -> value)

      def take_coin(progress: Map[String, Int]) : Map[String, Int] = {
        // how many left do we have to take away from
        val smallest = progress("pence")
        // find the biggest coin that fits into us
        val biggestCoin = sorted_coins.find((coin: Coin) =>
          smallest >= coin._1
        )
        biggestCoin match {
          case Some((_, "pence")) => progress
          case Some((coinSize: Int, coinName: String)) =>
            take_coin(progress + ("pence" -> (progress("pence") - coinSize),
                      coinName -> (progress(coinName) + 1)))
          case None => progress
        }
      }

      take_coin(moneyBag)
    }
  }

  class FreshSystem() extends DenominatedUnits {
    override def toString() : String = "(no coins)"

    def addCoin(name: String, value: Int) : DenominatedUnits =
        new CurrencySystem(List[Coin]((value, name)))
    def distribute(pence: Int) : Map[String, Int] = Map[String, Int]()
  }


  val ukMoney = new CurrencySystem(List[Coin](
    (5, "5p"),
    (10, "10p"),
    (20, "20p"),
    (50, "50p"),
    (100, "£"),
    (200, "£2"),
    (500, "£5"),
    (1, "pence")
  ))

  println(ukMoney)
  println(ukMoney.distribute(473).mkString("\n"))
  println("yup")
}
