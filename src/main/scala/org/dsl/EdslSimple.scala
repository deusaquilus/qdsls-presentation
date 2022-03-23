package org.dsl

import zio.json._
import zio.json.JsonCodec.apply

object EdslSimple:

  object Serialize:
    import Ast._
    given JsonEncoder[Subtract]        = DeriveJsonEncoder.gen[Subtract]
    given JsonEncoder[Concat]          = DeriveJsonEncoder.gen[Concat]
    given JsonEncoder[ToString]        = DeriveJsonEncoder.gen[ToString]
    given JsonEncoder[Constant]        = DeriveJsonEncoder.gen[Constant]
    given JsonEncoder[Key]             = DeriveJsonEncoder.gen[Key]
    given astEncoder: JsonEncoder[Ast] = DeriveJsonEncoder.gen[Ast]
    def apply(ast: Ast)                = astEncoder.encodeJson(ast, Some(2))

  sealed trait Ast
  object Ast:
    case class Subtract(left: Ast, right: Ast) extends Ast
    case class Concat(left: Ast, right: Ast)   extends Ast
    case class ToString(value: Ast)            extends Ast
    case class Constant(value: String)         extends Ast
    case class Key(name: String)               extends Ast

  import Ast._

  case class Query(ast: Ast)

  val query =
    Query(
      Concat(
        Concat(
          ToString(Subtract(Key("underlyingPrice"), Key("strikePrice"))),
          Constant("/")
        ),
        Key("symbol")
      )
    )

  extension (a: Any)
    def toString = a.toString
    def toInt =
      if (a.isInstanceOf[Int]) a.asInstanceOf[Int]
      else a.toString.toInt

  case class Evaluator(row: Map[String, Any]):
    private def eval(ast: Ast): Any =
      ast match
        case Subtract(a: Ast, b: Ast) => eval(a).toInt - eval(b).toInt
        case Concat(a: Ast, b: Ast)   => eval(a).toString + eval(b).toString
        case ToString(v: Ast)         => eval(v).toString
        case Constant(v: String)      => v
        case Key(value: String)       => row(value)
    def apply(q: Query) =
      eval(q.ast)

  def runEvaluate(): Unit = {
    val symbols = List(
      Map("symbol" -> "AAPL", "strikePrice" -> 140, "underlyingPrice"  -> 161),
      Map("symbol" -> "MSFT", "strikePrice" -> 220, "underlyingPrice"  -> 337),
      Map("symbol" -> "NFLX", "strikePrice" -> 600, "underlyingPrice"  -> 654),
      Map("symbol" -> "GOOG", "strikePrice" -> 2800, "underlyingPrice" -> 2936)
    )
    symbols.foreach(sym => println(Evaluator(sym)(query)))
  }

  def runSerialize(): Unit = {
    println(Serialize(query.ast))
  }

  def main(args: Array[String]): Unit =
    runEvaluate()
    runSerialize()

end EdslSimple
