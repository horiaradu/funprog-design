package week4

import scala.util.DynamicVariable

/**
  * Created by horiaradu on 10/09/16.
  */
class Signal[T](expr: => T) {
  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  update(expr)

  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  protected def computeValue(): Unit = {
    val newValue = Signal.caller.withValue(this)(myExpr())
    if (myValue != newValue) {
      myValue = newValue

      val obs = observers
      observers = Set()

      obs.foreach(_.computeValue())
    }
  }

  def apply(): T = {
    observers += Signal.caller.value
    assert(!Signal.caller.value.observers.contains(this), "cyclic signal definition")
    myValue
  }

}

object Signal {
  //  private val caller = new StackableVariable[Signal[_]](NoSignal)
  private val caller = new DynamicVariable[Signal[_]](NoSignal)

  def apply[T](expr: => T): Signal[T] = new Signal(expr)
}

object NoSignal extends Signal[Nothing](???) {
  override def computeValue(): Unit = ()
}
