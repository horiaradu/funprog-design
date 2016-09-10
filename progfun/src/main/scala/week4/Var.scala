package week4

/**
  * Created by horiaradu on 10/09/16.
  */
class Var[T](expr: => T) extends Signal[T](expr) {
  override def update(expr: => T): Unit = super.update(expr)
}

object Var {
  def apply[T](expr: => T): Var[T] = new Var(expr)
}
