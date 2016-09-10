object session {

  trait Result {
    def value: Int
  }

  def compute(f: => () => Int) = new Result {
    private var result: Option[Int] = None

    override def value: Int =
      result match {
        case Some(x) => x
        case None => {
          result = Some(f())
          value
        }
      }
  }

  val pure = () => 3
  val computed = compute(pure)

  computed.value
  computed.value

  val impure = () => {
    println("foo")
    3
  }

  val impureComputed = compute(impure)

  impureComputed.value
  impureComputed.value
}