object session {
  (1 to 10).toStream.filter(x => x % 2 == 0)(1)

  def from(n: Int): Stream[Int] = n #:: from(n + 1)

  val nats = from(0)
  val m4s = nats.map(_ * 4)
  m4s.take(100).toList

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))

  val primes = sieve(from(2))
  primes.take(10).toList
}