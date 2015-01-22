def distribute_coins(pence: Int) : Map[String, Int] = {

  def take_coin(coins: Map[String, Int]): Map[String, Int] = {
    println(coins)
    val pence = coins("pence")
    val (denomination: Int, coin_name: String) = pence match {
      case x if (pence >= 200) => (200, "£2")
      case x if (pence >= 100) => (100, "£")
      case x if (pence >= 50) => (50, "50p")
      case x if (pence >= 20) => (20, "20p")
      case x if (pence >= 10) => (10, "10p")
      case x if (pence >= 5) => (5, "5p")
      case x => (pence, "pence")
    }
    if (coin_name == "pence")
      coins
    else
      take_coin(coins + ("pence" -> (pence - denomination), coin_name -> (coins(coin_name) + 1)))
  }
  take_coin(Map[String, Int]("£" -> 0,
    "5p" -> 0,
    "10p" -> 0,
    "20p" -> 0,
    "50p" -> 0,
    "£2" -> 0,
    "2p" -> 0,
    "pence" -> pence))
}


val coinage = distribute_coins(473)
println(coinage.mkString("\n"))

