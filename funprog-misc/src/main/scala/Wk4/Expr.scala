package Wk4

/**
  * Created by jmontroy on 7/2/16.
  */
trait Expr

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr

object exprs {
  def show(e: Expr) = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
  }
}
