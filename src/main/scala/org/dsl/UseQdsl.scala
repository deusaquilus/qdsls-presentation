package org.dsl

object UseQdsl {
  import Qdsl._

  def data: RemoteData = ???
  def main(args: Array[String]): Unit = {
    val output =
      query(
        asString(data.underlyingPrice - data.strikePrice) + "/" + data.symbol
      )

    println("====== Runtime Result ======\n" + pprint(output))
  }
}
