package services.identity

import scala.collection.mutable

import models.ListingKey

/** How the identity model's update cuts its work into regions and resolves them ([[IncrementalResolver]]): units of
 *  listings packed into bounded batches, and the batches resolved in order, on the calling thread or side by side. */
private[identity] object Regions {

  /** `units` — each a set of listings resolved together, never split — merged where they overlap
   *  and packed, smallest first, into batches of at most `limit` listings (a unit larger alone). */
  def pack(handed: Seq[Set[ListingKey]], limit: Int): Seq[Set[ListingKey]] = {
    // Indexed: the merge reads units by index, and an update hands them over as a list.
    val units = handed.toIndexedSeq
    // Overlapping units merged by a union-find over their indices, each listing naming the first unit holding it.
    val parent = Array.tabulate(units.size)(identity)
    def root(i: Int): Int = { var r = i; while (parent(r) != r) r = parent(r); var j = i
                              while (parent(j) != r) { val up = parent(j); parent(j) = r; j = up }; r }
    val firstUnit = mutable.HashMap.empty[ListingKey, Int]
    units.zipWithIndex.foreach { case (unit, i) =>
      unit.foreach(key => firstUnit.get(key).fold(firstUnit(key) = i)(other => parent(root(i)) = root(other)))
    }
    val merged = units.indices.groupMapReduce(root)(units(_))(_ ++ _).values.toSeq
    // Smallest first, ties by smallest listing: the packing is a function of the units, not of their order.
    val byFirst = ListingKey.ordering
    merged.map(unit => (unit.size, unit.min(using byFirst), unit)).sortWith { case ((sa, fa, _), (sb, fb, _)) =>
      sa < sb || (sa == sb && byFirst.lt(fa, fb)) }.map(_._3)
      .foldLeft(Vector.empty[Set[ListingKey]]) { (batches, unit) =>
        batches.lastOption match {
          case Some(open) if open.size + unit.size <= limit => batches.init :+ (open ++ unit)
          case _                                            => batches :+ unit
        }
      }
  }

  /** `f` of each of `items`, in their order: on the calling thread, or on at most `threads` threads of a pool made for the call (DaemonExecutors' bounded one)
   *  (each item's work its own; the results gathered in order, the first failure thrown). The caller's switched-off
   *  rules ([[RuleAblation]], held per thread) hold on every thread. */
  private[identity] def inOrder[A, B](items: Seq[A], threads: Int)(f: A => B): Seq[B] =
    if (threads <= 1 || items.sizeIs < 2) items.map(f)
    else {
      val off  = RuleAblation.switchedOff
      val pool = tools.DaemonExecutors.boundedEC("identity-regions", math.min(threads, items.size))
      try items.map(item => pool.submit(() => RuleAblation.disabling(off)(f(item)))).map { future =>
        try future.get() catch { case e: java.util.concurrent.ExecutionException if e.getCause != null => throw e.getCause }
      }
      finally { val _ = pool.shutdownNow() }
    }
}
