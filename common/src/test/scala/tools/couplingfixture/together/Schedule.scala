package tools.couplingfixture.together

// CouplingSpec's fixture: Schedule's collaborators defined in its own file. Zinc records none of Schedule's uses of
// them; its class files do -- through a field, a lambda and an anonymous class alike.

final class Schedule(showtimes: Showtimes) {
  def first: Int = showtimes.count
  def sizes(rows: Seq[Int]): Seq[Listing] = rows.map(n => new Listing(n))
  def later: Runnable = new Runnable { def run(): Unit = { Venue.default; () } }
}

final class Showtimes { def count: Int = 1 }

final case class Listing(size: Int)

object Venue { def default: String = "venue" }

/** Uses no other class of the file. */
final class Alone { def answer: Int = 42 }

/** A sealed family: each child uses its parent; the parent's companion mirror (`ordinal`) testing every child is the
 *  compiler's, no use of them. */
sealed trait Event
final case class Started(at: Int) extends Event
case object Stopped extends Event
