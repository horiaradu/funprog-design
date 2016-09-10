import scala.collection.mutable

object waterPouringOptimized {

  class Pouring(capacity: Vector[Int]) {
    type State = Vector[Int]
    val initialState = capacity.map(_ => 0)

    trait Move {
      def change(state: State): State
    }

    case class Empty(glass: Int) extends Move {
      def change(state: State) = state.updated(glass, 0)
    }

    case class Fill(glass: Int) extends Move {
      def change(state: State) = state.updated(glass, capacity(glass))
    }

    case class Pour(from: Int, to: Int) extends Move {
      def change(state: State) = {
        val amount = state(from).min(capacity(to) - state(to))
        state
          .updated(from, state(from) - amount)
          .updated(to, state(to) + amount)
      }
    }

    val glasses = capacity.indices
    val moves =
      (for (glass <- glasses) yield Empty(glass)) ++
        (for (glass <- glasses) yield Fill(glass)) ++
        (for {
          from <- glasses
          to <- glasses
          if from != to
        } yield Pour(from, to))


    class Path(history: List[Move], val endState: State) {
      def extend(move: Move) = new Path(move :: history, move.change(endState))

      override def toString = s"${history.reverse.mkString(" ")} --> $endState"
    }

    val initialPath = new Path(Nil, initialState)

    def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
      if (paths.isEmpty) Stream.empty
      else {
        val more = for {
          path <- paths
          next <- moves.map(path.extend)
          if !explored.contains(next.endState)
        } yield next

        paths #:: from(more, explored ++ more.map(_.endState))
      }

    val pathSets = from(Set(initialPath), Set(initialState))

    def solution(target: Int): Stream[Path] = {
      def isSolution(path: Path) = path.endState.contains(target)

      for {
        pathSet <- pathSets
        path <- pathSet
        if isSolution(path)
      } yield path
    }
  }


  val problem = new Pouring(Vector(4, 7))
  problem.moves

  problem.initialPath

  problem.pathSets.take(4).toList

  problem.solution(5)
}