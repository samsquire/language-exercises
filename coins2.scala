type Coin = (Int,String)

trait CurrencySystem {
  def addCoin(name: String, value: Int) : CurrencySystem
  def distribute(pence: Int) : Map[String, Int]
}

class Coins(known_coins: List[Coin]) extends CurrencySystem {
  override def toString() : String = known_coins.mkString("\n")

  def largestDenomination(e1: Coin, e2: Coin) : Boolean =
    e1._2 > e2._2

  def addCoin(name: String, value: Int) : CurrencySystem =
      new Coins(((value, name) :: known_coins).sortWith(largestDenomination))

  def distribute(value: Int) : Map[String, Int] = {
    // first lets create a bag of coin denominations
    val moneyBag: Map[String, Int] = known_coins.foldLeft(Map[String, Int]())(
      (totals, coin) => (totals + ((coin._2 -> 0)))) + ("pence" -> value)

    println("moneyBag")
    println(moneyBag.mkString(" "))
    def take_coin(progress: Map[String, Int]) : Map[String, Int] = {
      // how many left do we have to take away from
      val smallest = progress("pence")
      println(smallest)
      // find the biggest coin that fits into us
      val biggestCoin = known_coins.find((coin: Coin) =>
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

class FreshSystem() extends CurrencySystem {
  override def toString() : String = "(no coins)"

  def addCoin(name: String, value: Int) : CurrencySystem =
      new Coins(List[Coin]((value, name)))
  def distribute(pence: Int) : Map[String, Int] = Map[String, Int]()
}


val purse = new FreshSystem().addCoin("pence", 1).addCoin("5p", 5).addCoin("£", 100).addCoin("£5", 500).addCoin("50p", 50).addCoin("20p", 20)
println(purse)
println(purse.distribute(473).mkString("\n"))
println("hi")

