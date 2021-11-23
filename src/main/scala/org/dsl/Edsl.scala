package org.dsl

object Edsl:
  trait Ast[T]
  object Ast:
    case class Subtract(a: Ast[Int], b: Ast[Int])     extends Ast[Int]
    case class Concat(a: Ast[String], b: Ast[String]) extends Ast[String]
    case class ToString(v: Ast[Int])                  extends Ast[String]
    case class Constant(v: String)                    extends Ast[String]
    case class Key[T](name: String)                   extends Ast[T]

  case class Value[T](key: String, value: T)
  case class RemoteData(
      symbol: Value[String],
      strikePrice: Value[Int],
      underlyingPrice: Value[Int]
  )(map: Map[String, Any])

  object Extensions:
    import Ast._
    extension (v: Value[Int])
      def -(other: Value[Int]) = Subtract(Key(v.key), Key(other.key))

    extension (v: Value[String])
      def +(other: Value[String]) = Concat(Key(v.key), Key(other.key))

    extension (v: Ast[String])
      def +(other: Value[String]) = Concat(v, Key(other.key))
      def +(other: String)        = Concat(v, Constant(other))
  end Extensions
  import Extensions._

  def asString(ast: Ast[Int]) = Ast.ToString(ast)

end Edsl
