package org.dsl

import scala.quoted._
import org.dsl.Format

object Extractors {
  object Lambda1:
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[(String, quotes.reflect.Term)] =
      import quotes.reflect.{Ident => TIdent, Constant => TConstant, Block => TBlock, ValDef => TValDef, _}
      term match
        case Lambda(List(TValDef(ident, Inferred(), None)), methodBody) => Some((ident, methodBody))
        case TBlock(List(), expr)                                       => unapply(expr)
        case _                                                          => None
  end Lambda1

  def underlyingArgument(expr: Expr[_])(using Quotes) =
    import quotes.reflect._
    expr.asTerm.underlyingArgument.asExpr

  object UntypeApply:
    private def recurse(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term =
      import quotes.reflect._
      term match
        case TypeApply(content, args) => recurse(content)
        case other                    => other

    def unapply(using Quotes)(term: quotes.reflect.Term) = Some(recurse(term))
  end UntypeApply

  def is[T: Type](using Quotes)(tpe: quotes.reflect.TypeRepr) =
    import quotes.reflect._
    tpe <:< TypeRepr.of[T]
}

object Qdsl {
  import Extractors._

  case class RemoteData(
      symbol: String,
      strikePrice: Int,
      underlyingPrice: Int
  )

  trait Entity[T]:
    def map[R](f: T => R): Entity[R]
  object Entity:
    // we don't want an actual implementation
    // for practical reasons can make this throw an exception saying "must be quoted"
    def apply[T](value: T): Entity[T] = ???

  // This is only used inside of the quoted method so it doesn't need an implementation either
  def asString(i: Int): String = ???

  trait Ast
  object Ast:
    case class Subtract(a: Ast, b: Ast)                  extends Ast
    case class Concat(a: Ast, b: Ast)                    extends Ast
    case class ToString(v: Ast)                          extends Ast
    case class Constant(v: String)                       extends Ast
    case class Key(name: String)                         extends Ast
    case class Map(left: Ast, v: SetVariable, body: Ast) extends Ast
    case class SetVariable(name: String)                 extends Ast
    case class GetVariable(name: String)                 extends Ast

  object AsTerm:
    def unapply(expr: Expr[_])(using Quotes): Option[quotes.reflect.Term] =
      import quotes.reflect._
      Some(expr.asTerm)

  inline def query(inline value: Any): Ast = ${ queryImpl('value) }
  def queryImpl(value: Expr[Any])(using Quotes): Expr[Ast] =
    import quotes.reflect.{toString => _, _}
    def recurse(curr: Expr[_]): Ast =
      curr match {
        case AsTerm(Literal(StringConstant(value))) =>
          Ast.Constant(value)
        case AsTerm(Apply(UntypeApply(Select(left, "map")), List(Lambda1(valueVar, lambdaBody)))) =>
          Ast.Map(recurse(left.asExpr), Ast.SetVariable(valueVar), recurse(lambdaBody.asExpr))
        case '{ type t; Entity.apply[`t`](($value: `t`)) } => recurse(value)
        case '{ ($left: String) + ($right: String) }       => Ast.Concat(recurse(left), recurse(right))
        case '{ asString($i) }                             => Ast.ToString(recurse(i))
        case '{ ($left: Int) - ($right: Int) } =>
          Ast.Subtract(recurse(left), recurse(right))
        case AsTerm(Select(id @ Ident(data), fieldName)) if (is[RemoteData](id.tpe)) =>
          Ast.Key(fieldName)
        case AsTerm(Ident(data)) =>
          Ast.GetVariable(data)
        case _ =>
          report.throwError(s"Cannot match the expression: ${Printer.TreeStructure.show(curr.asTerm)}")
      }
    val result: Ast = recurse(underlyingArgument(value))
    println(pprint.apply(result))
    '{ ??? }
}
