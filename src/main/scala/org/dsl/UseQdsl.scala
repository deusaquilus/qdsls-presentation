package org.dsl

object UseQdsl {
  import Qdsl._

  def main(args: Array[String]) = {
    def data: RemoteData = ???
    query(
      Entity(asString(data.underlyingPrice - data.strikePrice))
        .map(value => value + "/" + data.symbol)
    )

    query(
      asString(data.underlyingPrice - data.strikePrice) + "/" + data.symbol
    )
  }
}
