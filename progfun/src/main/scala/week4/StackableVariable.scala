package week4

/**
  * Created by horiaradu on 10/09/16.
  */
class StackableVariable[T](init: T) {
  private var values: List[T] = List(init)

  def value: T = values.head

  def withValue[R](newValue: T)(op: => R): R = {
    values = newValue :: values
    try op finally values = values.tail
  }

}

/*
 * val caller = new StackableVariable(initialSig)
 * caller.withValue(otherSig) { ... }
 * ...
 * caller.value
 */