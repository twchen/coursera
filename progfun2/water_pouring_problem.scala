class Pouring(capacity: Vector[Int]) {
  type State = Vector[Int]
  val initialState = capacity map (_ => 0)

  // moves
  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    override def change(state: State): State = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    override def change(state: State): State = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: State): State = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  // indecies
  val glasses = 0 until capacity.length
  // all possible moves
  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for {
        from <- glasses
        to <- glasses
        if from != to
      } yield Pour(from, to))

  class Path(history: List[Move], val endState: State) {
    // the head of history is the last move
    //def endState: State = (history foldRight initialState)(_ change _)

    // track the end state of a list of moves
    private def trackState(history: List[Move]): State = history match {
        case Nil => initialState
        case move :: rest => move change trackState(rest)
    }

    // extend the current path with one more move
    def extend(move: Move): Path = new Path(move :: history, move change endState)

    override def toString = (history.reverse mkString " ") + "-->" + endState // need to reverse history first because the head is the last move
  }

  val initialPath: Path = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.Empty
    else {
        val more = for {
            path <- paths
            next <- moves map path.extend
            if !(explored contains next.endState)
        } yield next
        paths #:: from(more, (explored ++ (more map (_.endState))))
    }

  val pathSets = from(Set(initialPath), Set(initialState))

  // find the shortest path to a state that one glass contains target volumn
  def solution(volumn: Int): Stream[Path] =
    for {
        pathSet <- pathSets
        path <- pathSet
        if path.endState contains volumn
    } yield path
}