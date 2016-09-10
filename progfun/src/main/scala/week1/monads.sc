import scala.util.control.NonFatal

object monads {

  abstract class Option[+T] {
    def flatMap[U](f: T => Option[U]): Option[U] =
      this match {
        case Some(x) => f(x)
        case None => None
      }

    /** **** without map, using Option in for doesn't work *******/
    def map[U](f: T => U): Option[U] =
      this match {
        case Some(x) => Some(f(x))
        case None => None
      }
  }

  case class Some[+T](x: T) extends Option[T]

  case object None extends Option[Nothing]

  val f = (x: Int) => Some(x + 1)
  val g = (x: Int) => Some(x * x)

  // left unit law
  Some(2).flatMap(f) == f(2)
  None.flatMap(f) == None

  // right unit law
  Some(2).flatMap(x => Some(x)) == Some(2)
  None.flatMap(x => Some(x)) == None

  //associative law
  Some(2).flatMap(f).flatMap(g) == Some(2).flatMap(x => f(x).flatMap(g))
  None.flatMap(f).flatMap(g) == None.flatMap(x => f(x).flatMap(g))

  for {
    x <- Some(2)
    y <- Some(3)
  } yield x + y

  for {
    x <- Some(2)
    y <- None
  } yield x + y.asInstanceOf[Int]


  /** **********************/

  abstract class Try[+T] {
    def flatMap[U](f: T => Try[U]): Try[U] =
      this match {
        case Success(x) =>
          try f(x) catch {
            case NonFatal(ex) => Failure(ex)
          }
        case fail: Failure => fail
      }

    def map[U](f: T => U): Try[U] =
      this match {
        case Success(x) => Try(f(x))
        case fail: Failure => fail
      }
  }

  case class Success[T](x: T) extends Try[T]

  case class Failure(ex: Throwable) extends Try[Nothing]

  object Try {
    def apply[T](expr: => T): Try[T] =
      try Success(expr)
      catch {
        case NonFatal(ex) => Failure(ex)
      }
  }

  Try(1 + 1)
  Try(1 / 0)

  for {
    x <- Try(1 + 2 + 3)
    y <- Try(2 + 5)
  } yield x + y

  for {
    x <- Try(1 * 2)
    y <- Try(1 / 0)
  } yield x + y

  val f1 = (x: Int) => Success(x + 1)
  val g1 = (x: Int) => Success(x * x)

  // left unit law
  Success(2).flatMap(f1) == f1(2)

  // right unit law
  Success(2).flatMap(x => Success(x)) == Success(2)

  //associative law
  Success(2).flatMap(f1).flatMap(g1) == Success(2).flatMap(x => f1(x).flatMap(g1))
}