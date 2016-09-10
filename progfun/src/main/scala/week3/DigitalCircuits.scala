package week3

/**
  * Created by horiaradu on 03/09/16.
  */
object DigitalCircuits {
  def main(args: Array[String]): Unit = {
    object sim extends Circuits with Parameters

    import sim._

    val in1, in2, sum, carry = new Wire

    halfAdder(in1, in2, sum, carry)
    probe("sum", sum)
    probe("carry", carry)

    in1.setSignal(true)
    in2.setSignal(true)

    run()
  }
}
