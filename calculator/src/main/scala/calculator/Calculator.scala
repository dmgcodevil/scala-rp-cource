package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
                     namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map { case (name, exp) => (name, calculate(exp, namedExpressions))}
  }

  def calculate(expr: Signal[Expr], references: Map[String, Signal[Expr]]): Signal[Double] = {
    Signal(eval(expr(), references))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case literal: Literal => literal.v
      case ref: Ref => eval(getReferenceExpr(ref.name, references), references)
      case plus: Plus => eval(plus.a, references) + eval(plus.b, references)
      case minus: Minus => eval(minus.a, references) - eval(minus.b, references)
      case times: Times => eval(times.a, references) * eval(times.b, references)
      case divide: Divide => eval(divide.a, references) / eval(divide.b, references)
      case _ => Double.NaN
    }
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
