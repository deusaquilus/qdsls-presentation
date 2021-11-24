package org.dsl

object EdslSimple:

  trait Ast
  object Ast:
    case class Subtract(a: Ast, b: Ast) extends Ast
    case class Concat(a: Ast, b: Ast)   extends Ast
    case class ToString(v: Ast)         extends Ast
    case class Constant(v: String)      extends Ast
    case class Key(name: String)        extends Ast

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

  def evalQuery(qry: Query, row: Map[String, Any]): Any =
    def eval(ast: Ast): Any =
      ast match
        case Subtract(a: Ast, b: Ast) => eval(a).toInt - eval(b).toInt
        case Concat(a: Ast, b: Ast)   => eval(a).toString + eval(b).toString
        case ToString(v: Ast)         => eval(v).toString
        case Constant(v: String)      => v
        case Key(value: String)       => row(value)
    eval(qry.ast)

  def main(args: Array[String]): Unit =
    println(evalQuery(query, Map("underlyingPrice" -> 123, "strikePrice" -> 100, "symbol" -> "AAPL")))

end EdslSimple
