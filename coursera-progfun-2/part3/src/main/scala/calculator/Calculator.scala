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
    namedExpressions.map{
      case(varName, signalExpr) =>
        (varName, Signal{eval(signalExpr(), namedExpressions)})
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) => {
        if(containsCyclicRef(Ref(name), Set(), references))
          Double.NaN
        else
          eval(getReferenceExpr(name, references), references)
      }
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }

  protected def containsCyclicRef(expr: Expr, referencedSoFar: Set[String], references: Map[String, Signal[Expr]]): Boolean = {
    expr match {
      case Ref(name) => {
        if(referencedSoFar.contains(name))
          true
        else
          containsCyclicRef(getReferenceExpr(name, references), referencedSoFar + name, references)
      }
      case Literal(v) => false
      case Plus(a, b) => containsCyclicRef(a, referencedSoFar, references) || containsCyclicRef(b, referencedSoFar, references)
      case Minus(a, b) => containsCyclicRef(a, referencedSoFar, references) || containsCyclicRef(b, referencedSoFar, references)
      case Times(a, b) => containsCyclicRef(a, referencedSoFar, references) || containsCyclicRef(b, referencedSoFar, references)
      case Divide(a, b) => containsCyclicRef(a, referencedSoFar, references) || containsCyclicRef(b, referencedSoFar, references)
    }
  }

}
