// package org.dsl

// case class RemoteData(
//   def symbol: String
//   def strikePrice: Int
//   def underlyingPrice: Int
// )

// RemoteData(AAPL, 140, 161)
// RemoteData(MSFT, 220, 337)
// RemoteData(NFLX, 600, 654)
// RemoteData(GOOG, 2800, 2936)
// ...
// // 20 Billion More...

// def execute(data: RemoteData) =
//   toString(data.underlyingPrice - data.strikePrice) + "/" + data.symbol


// Query(toString(data.underlyingPrice - data.strikePrice) + "/" + data.symbol)

