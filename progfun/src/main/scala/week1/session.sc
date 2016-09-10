object session {

  trait Generator[+T] {
    // alias for this
    self =>

    def generate: T

    def map[S](f: T => S) = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]) = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  val integers = new Generator[Int] {
    val rand = new java.util.Random

    def generate = rand.nextInt
  }

  val booleans = for (x <- integers) yield x > 0

  def pairs[T, U](t: Generator[T], u: Generator[U]) =
    t.flatMap(x => u.map(y => (x, y)))

  for {
    x <- integers
  } yield System.out.println(x + 1)

  def single[T](x: T) = new Generator[T] {
    def generate = x
  }

  def choose(low: Int, high: Int): Generator[Int] =
    for (x <- integers) yield low + x % (high - low)

  def oneOf[T](xs: T*) =
    for (index <- choose(0, xs.length)) yield xs(index)

  oneOf(1, 2, 3, 4)

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list

  def emptyLists = single(Nil)

  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail


  /** ******** Tree ************/
  trait Tree

  case class Inner(left: Tree, right: Tree) extends Tree

  case class Leaf(x: Int) extends Tree

  def trees: Generator[Tree] =
    for {
      isLeaf <- booleans
      tree <- if (isLeaf) leafs else inners
    } yield tree

  def leafs: Generator[Leaf] =
    for (x <- integers) yield Leaf(x)

  def inners: Generator[Inner] =
    for {
      left <- trees
      right <- trees
    } yield Inner(left, right)

  trees.generate

  /** ********* Testing ***********/

  def test[T](generator: Generator[T], times: Int = 100)(test: T => Boolean): Unit = {
    for (i <- 0 until times) {
      val value = generator.generate
      assert(test(value), s"test failed for: $value")
    }
    println(s"Test passed $times times")
  }

  //  test(integers, 10)(x => x == 3)

  test(pairs(lists, lists), 10) { case (xs, ys) => (xs ++ ys).length >= ys.length }

  test(pairs(lists, lists), 10) { case (xs, ys) => (xs ++ ys).length == xs.length + ys.length }
}
