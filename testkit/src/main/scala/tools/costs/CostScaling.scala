package tools.costs

import org.scalatest.Assertions.fail

import java.lang.reflect.{InvocationHandler, InvocationTargetException, Method, Proxy}
import java.util.concurrent.atomic.AtomicLong

/**
 * "Run the operation at N and at 4N, and hold its WORK to a budget" — the shape of every
 * cost guard in this codebase, in one place.
 *
 * Work is COUNTED, never timed: rows a read hands back (what Mongo would decode) and writes
 * (what Mongo would take), via [[Work.counting]], or any other deterministic probe a spec
 * has (a `sanitize` counter). A wall-clock bound on a shared CI runner fails for reasons
 * that have nothing to do with the code.
 *
 * The bugs this shape has caught, each an O(N²) that looked fine at the sizes specs used:
 * a staging chain kicked once per JOINING venue, each kick re-reading the growing group
 * (727a57a6e — the US sample leg's scrape tick went from 27s to 1,419s); every venue's
 * detail finish re-reading the whole group (f56949875 — 52,000 row reads for one film at 160
 * venues); a scrape walking the whole corpus per venue.
 */
object CostScaling {

  /**
   * The cost at `n` and at `factor * n` must both stay within `perUnit` per unit, AND grow
   * no faster than the size did. The first bound catches a cost that is already too high at
   * the small size; the second catches a quadratic whose constant hides it at both.
   */
  def assertLinear(what: String, n: Int, perUnit: Double, factor: Int = 4)(cost: Int => Long): Unit = {
    val (small, large) = (cost(n), cost(factor * n))
    val budget = perUnit * factor * n
    if (large > budget || (small > 0 && large.toDouble / small > factor * 1.125))
      fail(s"$what: cost $small at $n, $large at ${factor * n} — linear allows at most ${budget.toLong} " +
        s"at ${factor * n} and growth of ${factor}x; this grew ${if (small > 0) f"${large.toDouble / small}%.1f" else "∞"}x")
  }

  /** The cost must not grow with `n` at all: the work of one operation against a corpus of
   *  `factor * n` is no more than against `n`. */
  def assertIndependent(what: String, n: Int, factor: Int = 4)(cost: Int => Long): Unit = {
    val (small, large) = (cost(n), cost(factor * n))
    if (large > small)
      fail(s"$what: cost $small against $n, $large against ${factor * n} — it must not depend on the size at all")
  }
}

/** Rows read and writes made through the collaborators a spec wrapped with [[Work.counting]]. */
final class Work {
  private val read     = new AtomicLong
  private val written  = new AtomicLong
  private val byMethod = new java.util.concurrent.ConcurrentHashMap[String, AtomicLong]()
  def reads: Long  = read.get
  def writes: Long = written.get
  def total: Long  = reads + writes
  /** Rows (read or written) per method name — what a failing budget should be read against. */
  def breakdown: Map[String, Long] = {
    import scala.jdk.CollectionConverters.*
    byMethod.asScala.view.mapValues(_.get).toMap
  }
  def reset(): Unit = { read.set(0); written.set(0); byMethod.clear() }
  private def tally(method: String, n: Long): Unit = { byMethod.computeIfAbsent(method, _ => new AtomicLong).addAndGet(n); () }
  private[costs] def addRead(method: String, n: Long): Unit  = { read.addAndGet(n); tally(method, n) }
  private[costs] def addWrite(method: String, n: Long): Unit = { written.addAndGet(n); tally(method, n) }
}

object Work {

  /** `StagingRepository`'s reads that the Mongo repository answers off its anchor index. */
  val StagingIndexReads: Set[String] = Set("holdsAnchor", "cinemasUnder")

  /** Method-name stems that mean a WRITE; every other call is a read. */
  private val WriteStems =
    Seq("upsert", "replace", "delete", "update", "put", "insert", "amend", "save", "remove", "write", "patch", "change", "move", "merge")

  /**
   * `delegate`, behind a proxy of the trait `contract` that adds to `work` the rows each read
   * returns (a collection's size, an `Option`'s 0 or 1, a checked read's collection) and, per
   * write, the rows it names (a batch's size, else one). Calls the delegate makes on ITSELF
   * are not seen — this counts what its callers ask for, which is what the delegate's real
   * counterpart would be asked.
   *
   * `indexOnly` names reads the real store answers off an in-memory index, decoding nothing
   * (`StagingRepository.cinemasUnder` / `holdsAnchor`): each costs one, not one per element.
   */
  def counting[T](contract: Class[T], delegate: T, work: Work, indexOnly: Set[String] = Set.empty): T =
    Proxy.newProxyInstance(contract.getClassLoader, Array(contract), new InvocationHandler {
      def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]): AnyRef = {
        val result =
          try method.invoke(delegate, (if (args == null) Array.empty[AnyRef] else args)*)
          catch { case e: InvocationTargetException => throw e.getCause }
        if (WriteStems.exists(method.getName.startsWith))
          work.addWrite(method.getName, Option(args).flatMap(_.collectFirst { case rows: Iterable[?] => rows.size.toLong }).getOrElse(1L))
        else work.addRead(method.getName, if (indexOnly(method.getName)) 1L else rows(result))
        result
      }
    }).asInstanceOf[T]

  private def rows(result: Any): Long = result match {
    case (first: Iterable[?], _: Boolean) => first.size.toLong
    case collection: Iterable[?]          => collection.size.toLong
    case option: Option[?]                => option.size.toLong
    case array: Array[?]                  => array.length.toLong
    case _                                => 0L
  }
}
