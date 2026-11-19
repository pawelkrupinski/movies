package tools.couplingfixture.kinds

// CouplingSpec's fixture: one type of each kind Coupling.Collaborators tells apart, read from their class files. The
// DATA types first -- a dependency on one is no coupling --, then the COLLABORATORS, each named for why it is one
// (Rule and EmptyRule, a pure strategy, are data too).

// ── data ──

final case class Point(x: Int, y: Int) { def norm: Int = x * x + y * y }

case object Origin

enum Colour {
  case Red, Green
  case Mixed(red: Int, green: Int)
}

/** A sealed data hierarchy: every implementation a case class or a case object. */
sealed trait Figure
final case class Circle(radius: Int) extends Figure
case object Blank extends Figure

/** Constants and pure functions: a string helper. */
object Slug {
  val Separator: String = "-"
  private val Spaces = "\\s+".r
  def of(title: String): String = Spaces.replaceAllIn(title.trim.toLowerCase(java.util.Locale.ROOT), Separator)
}

/** An opaque type and a type alias live in an object of pure functions. */
object Units {
  opaque type Metres = Double
  type Span = (Int, Int)
  def metres(value: Double): Metres = value
  def span(from: Int, to: Int): Span = (from, to)
}

final case class Score(value: Double) extends AnyVal

final class Celsius(val degrees: Double) extends AnyVal { def warm: Boolean = degrees > 20 }

final class NotFound(what: String) extends RuntimeException(what)

/** Holds only data. */
final case class Booking(at: Point, colour: Colour)

/** A lazy val of its own fields is no state. */
final case class Title(raw: String) { lazy val clean: String = raw.trim }

/** A method taking a collaborator: its caller holds the store, and counts it. */
final case class Stamp(at: Long) { def stored(store: Store): Option[String] = store.get(at.toString) }

// ── collaborators ──

/** State: a var. */
object Counter {
  private var count = 0
  def next(): Int = { count += 1; count }
}

/** State: a mutable collection held. */
object Memo {
  private val seen = scala.collection.mutable.Map.empty[String, Int]
  def size: Int = seen.size
}

/** An effect: it draws randomness from the world. */
object Env { def home: String = java.util.UUID.randomUUID().toString }

/** Behaviour: a case class carrying a closure (ProjectionDraft's `rowsOf`, reading the store). */
final case class Draft(count: Int, rowsOf: Int => Seq[String]) { def rows: Seq[String] = rowsOf(count) }

/** State: a clock held (LatestTitleYear). */
final class YearCap(clock: java.time.Clock) { def value: Int = java.time.LocalDate.now(clock).getYear + 1 }

/** State: a hand-rolled memo, written after construction (TitleNormalizer's caches). */
final class Normalizer(rules: Seq[String]) {
  private var last: Option[(String, String)] = None
  def apply(title: String): String = last.filter(_._1 == title).fold {
    val clean = rules.foldLeft(title)((t, r) => t.replace(r, "")); last = Some(title -> clean); clean
  }(_._2)
}

/** A record-like final class of vals, with pure methods: data, as a case class is. */
final class Node(val title: String, val listings: Seq[Int]) { val weight: Int = listings.size; def label: String = s"$title x$weight" }

/** An interface, implemented by a class holding state. */
trait Store { def get(key: String): Option[String] }
final class MapStore extends Store {
  private val entries = scala.collection.mutable.Map.empty[String, String]
  def get(key: String): Option[String] = entries.get(key)
}

/** An interface with no implementation. */
trait Port { def send(message: String): Unit }

/** A pure strategy: an object of pure functions implementing a trait -- data, as its trait is. */
trait Rule { def applies(title: String): Boolean }
object EmptyRule extends Rule { def applies(title: String): Boolean = title.isEmpty }

/** Holds a collaborator. */
final case class Wiring(store: Store, size: Int)

/** Reads an object that reads the world: an effect passed on. */
object Tickets { def home(): String = s"~${Env.home}" }

/** Reads an object that only keeps private state (Counter's var): nothing passed on -- a caller of Counter counts it. */
object Numbering { def label(): String = s"#${Counter.next()}" }

/** A hierarchy one of whose implementations is a collaborator. */
sealed trait Source
final case class Fixed(value: String) extends Source
final class Live(store: Store) extends Source { def value: Option[String] = store.get("live") }
