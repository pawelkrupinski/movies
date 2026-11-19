package controllers

import services.cadence.CadenceReport

import java.time.Instant
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt

/** What the dev-only rating cadence page shows of one country's [[DebugStack]] ([[CadenceController]]). */
object CadencePage {

  /** The per-(rating source, film) cadence report, its films named from the corpus-listing snapshot `/debug` renders,
   *  and the mirror badge of the OLDER of the two snapshots' stamps at `now`: the page is as behind as its stalest half.
   *  The two reads run side by side. */
  def read(stack: DebugStack, now: Instant)(using ExecutionContext): (Seq[CadenceReport.Group], Option[MirrorFreshness.Age]) = {
    val recordsFuture = Future(stack.ratingCadence())
    val listingFuture = Future(stack.listing())
    val (records, listing) = Await.result(recordsFuture.zip(listingFuture), 70.seconds)
    (CadenceReport.build(records.value, listing.value.titleByTmdb.get),
      Seq(records, listing).minBy(_.mirrorNewest.getOrElse(Instant.MAX)).mirrorAge(now))
  }
}
