package org.dsl

object UseEdsl {
  import Edsl._
  import Edsl.Extensions._

  def translate(data: RemoteData) =
    asString(data.underlyingPrice - data.strikePrice) + "/" + data.symbol

  def main(args: Array[String]): Unit = {
    val remoteData =
      RemoteData(
        Value("symbol", "AAPL"),
        Value("strikePrice", 123),
        Value("underlyingPrice", 456)
      )(Map("symbol" -> "AAPL", "strikePrice" -> 123, "underlyingPrice" -> 456))
    val result = translate(remoteData)
    println(pprint.apply(result))
  }
}
