package misc.examples

object NPiece {

  // Domain

  sealed trait Piece {
    def x: Int
    def y: Int
  }
  object Piece {
    type Apply = ((Int, Int) => Piece)
  }

  sealed trait PieceType extends Piece.Apply

  case class King(y: Int, x: Int) extends Piece
  case class Queen(y: Int, x: Int) extends Piece
  case class Rook(y: Int, x: Int) extends Piece
  case class Bishop(y: Int, x: Int) extends Piece
  case class Knight(y: Int, x: Int) extends Piece

  case object King extends PieceType { override def toString = "King" }
  case object Queen extends PieceType { override def toString = "Queen" }
  case object Rook extends PieceType { override def toString = "Rook" }
  case object Bishop extends PieceType { override def toString = "Bishop" }
  case object Knight extends PieceType { override def toString = "Knight" }

  /** Game board */
  type Board = Set[Piece]
  /** List of available pieces */
  type Stash = List[PieceType]

  // Type classes

  sealed trait Moves[P <: Piece] {
    /** can given `piece` capture the `other` */
    def canCapture(piece: P, other: Piece): Boolean
  }

  object Moves {
    import scala.math._

    def apply[T <: Piece : Moves]: Moves[T] =
      implicitly[Moves[T]]

    implicit object kingMoves extends Moves[King] {
      def canCapture(k: King, p: Piece) =
        abs(k.x - p.x) < 2 && abs(k.y - p.y) < 2
    }

    implicit object queenMoves extends Moves[Queen] {
      def canCapture(q: Queen, p: Piece) =
        q.x == p.x || q.y == p.y || abs(q.x - p.x) == abs(q.y - p.y)
    }

    implicit object rookMoves extends Moves[Rook] {
      def canCapture(r: Rook, p: Piece) =
        r.x == p.x || r.y == p.y
    }

    implicit object bishopMoves extends Moves[Bishop] {
      def canCapture(b: Bishop, p: Piece) =
        abs(b.x - p.x) == abs(b.y - p.y)
    }

    implicit object knightMoves extends Moves[Knight] {
      def canCapture(k: Knight, p: Piece) = {
        (p.x == k.x + 1 && p.y == k.y - 2) ||
        (p.x == k.x + 2 && p.y == k.y - 1) ||
        (p.x == k.x + 2 && p.y == k.y + 1) ||
        (p.x == k.x + 1 && p.y == k.y + 2) ||
        (p.x == k.x - 1 && p.y == k.y + 2) ||
        (p.x == k.x - 2 && p.y == k.y + 1) ||
        (p.x == k.x - 2 && p.y == k.y - 1) ||
        (p.x == k.x - 1 && p.y == k.y - 2)
      }
    }
  }

  // Solution

  /** placements of pieces in `stash` on `m x n` dimensional board */
  def pieces(m: Int, n: Int, stash: Stash): Set[Board] = {
    val cache = collection.mutable.WeakHashMap.empty[(Board, Stash), Boolean]
    val res = collection.mutable.Set.empty[Board]

    def placePieces(board: Board, stash: Stash, r: Int): Unit = {
      cache.put((board, stash), true)
      if (stash.nonEmpty) for {
        row <- r to m
        col <- 1 to n
        bs @ (b, s) <- permutations(row, col, board, stash)
      } yield {
        if (s.isEmpty) {
          res += b
        } else if (!cache.contains(bs)) {
          placePieces(b, s, row)
        }
      }
    }

    placePieces(Set(), stash, 1)
    res.toSet
  }

  /** debug version */
  def piecesDebug(m: Int, n: Int, stash: Stash): Set[Board] = {
    val cache = collection.mutable.WeakHashMap.empty[(Board, Stash), Boolean]
    val hits = new java.util.concurrent.atomic.AtomicLong()
    val recs = new java.util.concurrent.atomic.AtomicLong()
    val boards = new java.util.concurrent.atomic.AtomicLong()
    val res = collection.mutable.Set.empty[Board]

    def printStats(): Unit =
      println(s"recs=$recs cache=${cache.size} hits=$hits boards=$boards res=${res.size}")

    def placePieces(board: Board, stash: Stash, r: Int): Unit = {
      cache.put((board, stash), true)
      recs.incrementAndGet
      if (stash.nonEmpty) for {
        row <- r to m
        col <- 1 to n
        bs @ (b, s) <- permutations(row, col, board, stash)
      } yield {
        if (recs.get % 100000 == 0) printStats()
        if (s.isEmpty) {
          boards.incrementAndGet()
          res += b
        } else if (cache.contains(bs)) {
          hits.incrementAndGet();
        } else {
          placePieces(b, s, row)
        }
      }
    }

    placePieces(Set(), stash, 1)
    println(s"FINAL")
    printStats()
    res.toSet
  }

  /** permutations of the game `board` for given square `(row, col)` and `stash` */
  def permutations(row: Int, col: Int, board: Board, stash: Stash): Set[(Board, Stash)] =
    stash.filter(p => canPlace(p(row, col), board)).map { p =>
      (board + p(row, col), stash diff List(p))
    }(collection.breakOut)

  /** is it safe to place `piece` on `board` */
  def canPlace(piece: Piece, board: Board): Boolean =
    board.forall(p => !isSameSquare(piece, p) && !canCapture(piece, p) && !canCapture(p, piece))

  /** do pieces share the same square on board */
  def isSameSquare(p1: Piece, p2: Piece): Boolean =
    p1.x == p2.x && p1.y == p2.y

  /** erasure workaround */
  def canCapture(p1: Piece, p2: Piece): Boolean =
    p1 match {
      case p: King   => Moves[King].canCapture(p, p2)
      case p: Queen  => Moves[Queen].canCapture(p, p2)
      case p: Rook   => Moves[Rook].canCapture(p, p2)
      case p: Bishop => Moves[Bishop].canCapture(p, p2)
      case p: Knight => Moves[Knight].canCapture(p, p2)
    }
}

object NPieceTest extends App {
  import NPiece._

  val t1 = assert { pieces(1, 1, List(Queen)) == Set(Set(Queen(1, 1))) }
  val t2 = assert { pieces(1, 1, List(Queen, Queen)) == Set() }
  val t3 = assert { pieces(2, 2, List(Queen)) == Set(Set(Queen(1, 1)), Set(Queen(1, 2)), Set(Queen(2, 1)), Set(Queen(2, 2))) }
  val t4 = assert { pieces(2, 2, List(Queen, Queen)) == Set() }
  val t5 = assert {
    pieces(4, 4, List(Queen, Queen, Queen, Queen)) == Set(
      Set(Queen(1,2), Queen(2,4), Queen(3,1), Queen(4,3)),
      Set(Queen(1,3), Queen(2,1), Queen(3,4), Queen(4,2)))
  }

  val t6 = assert { pieces(3, 3, List(King, King, Rook)).size == 4 }
  val t7 = assert { pieces(4, 4, List(Rook, Rook, Knight, Knight, Knight, Knight)).size == 8 }

  lazy val t8 = assert { pieces(9, 6, List(King, King, Queen, Bishop, Rook, Knight)).size == 20136752 }

  println("OK")
}
