package org.dsl

object UseQdsl {
  import Qdsl._

  def data: RemoteData = ???

  def regularQuery(): Unit = {
    val output =
      query(
        asString(data.underlyingPrice - data.strikePrice) + "/" + data.symbol
      )

    println("====== Regular Query Result ======\n" + pprint(output))
  }

  def queryWithMap(): Unit = {
    val output =
      query(
        Entity(asString(data.underlyingPrice - data.strikePrice))
          .map((value: String) => value + "/" + data.symbol)
      )

    println("====== Query With Map Result ======\n" + pprint(output))
  }

  def main(args: Array[String]): Unit = {
    regularQuery()
    queryWithMap()
  }
}
