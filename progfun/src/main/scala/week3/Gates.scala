package week3

/**
  * Created by horiaradu on 03/09/16.
  */
abstract class Gates extends Simulation {

  class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()

    def getSignal: Boolean = sigVal

    def setSignal(sig: Boolean): Unit =
      if (sig != sigVal) {
        sigVal = sig
        actions.foreach(_ ())
      }

    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }

  }

  def InverterDelay: Int

  def AndDelay: Int

  def OrDelay: Int

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output.setSignal(!inputSig)
      }
    }
    input.addAction(invertAction)
  }

  def and(first: Wire, second: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val firstSig = first.getSignal
      val secondSig = second.getSignal
      afterDelay(AndDelay) {
        output.setSignal(firstSig && secondSig)
      }
    }
    first.addAction(andAction)
    second.addAction(andAction)
  }

  def or(first: Wire, second: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val firstSig = first.getSignal
      val secondSig = second.getSignal
      afterDelay(AndDelay) {
        output.setSignal(firstSig || secondSig)
      }
    }
    first.addAction(orAction)
    second.addAction(orAction)
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value=${wire.getSignal}")
    }

    wire.addAction(probeAction)
  }

}
