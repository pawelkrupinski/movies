package services

import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, Projections, Sorts}
import play.api.Logging
import services.movies.{MovieRepository, ScreeningsRepository}

import java.time.{Duration => JDuration, Instant}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * How current the local read-mirror's copy of ONE country's database is — the
 * newest `updatedAt` it holds, which the `/debug` navbar renders beside the
 * country it is showing.
 *
 * Why it exists: every `/debug` page reads the mirror
 * (`MONGODB_MOVIES_MIRROR_URI`) unconditionally, and a mirror that has stopped
 * syncing serves a SNAPSHOT — a page that renders perfectly, timestamps itself
 * `now`, and is quietly hours or days out of date. Three separate incidents
 * were diagnosed as data or pipeline bugs before anyone thought to check the
 * sync (2026-08-02 two days, 2026-08-04 two days, 2026-08-30 one day — the last
 * of those read as a rating-cadence bug: the US corpus had frozen during that
 * country's first hour, so its cadence page showed nothing but the 2h base
 * interval while prod had long since backed films off to 4h and 8h). The sync
 * now takes itself down when a supervisor dies (`scripts/local-mirror/mirror.sh`),
 * but that only covers the failure we have already seen; a page that states its
 * own age can never be believed to be live again, whatever stops the sync next.
 *
 * Mirror-side only, deliberately: the alternative — comparing against prod's
 * newest `updatedAt` — is the re-seed gate's job (`staleness.js`) and costs a
 * tunnel round-trip per page load. What the page needs is the cheap question,
 * "how old is what I am looking at".
 */
trait MirrorFreshness {
  /** Newest `updatedAt` in this database's mirrored corpus, or None when nothing
   *  is mirrored (prod, `MONGODB_MOVIES_MIRROR_URI` unset) or the mirror is
   *  unreachable/empty. */
  def newestUpdate(): Option[Instant]
}

object MirrorFreshness {
  /** Nothing is mirrored — prod, and every caller that reads its data straight
   *  from the source. The `/debug` navbar then shows no age, because there is no
   *  copy that could be behind. */
  val notMirrored: MirrorFreshness = () => None

  /** Past this the sync is not keeping up and the page is showing a snapshot.
   *  The same 30 minutes `staleness-rule.js` re-seeds on, so the banner turns red
   *  exactly when a healthy sync would already have re-seeded itself. */
  val StaleAfter: FiniteDuration = 30.minutes

  /** How far behind the mirror is, and whether that is far enough to disbelieve
   *  the page. */
  final case class Age(behind: FiniteDuration, stale: Boolean) {
    def label: String = MirrorFreshness.label(behind)
  }

  /** `12s`, `4m`, `26h`, `3d`. NOT `CadenceReport.intervalLabel`: that one names
   *  a refresh INTERVAL, whose smallest unit is a minute and whose whole point is
   *  to read `2d` rather than `48h`. An age has to be legible at both ends — a
   *  healthy mirror is seconds behind, a wedged one days. */
  def label(behind: FiniteDuration): String =
    if (behind < 1.minute)    s"${behind.toSeconds}s"
    else if (behind < 1.hour) s"${behind.toMinutes}m"
    else if (behind < 2.days) s"${behind.toHours}h"
    else                      s"${behind.toDays}d"

  /** The age to render, or None when there is nothing mirrored to age. A mirror
   *  reading AHEAD of the clock (a laptop whose time has drifted, a document
   *  stamped by a host running fast) is reported as 0 rather than as a negative
   *  age — it is not evidence of anything. */
  def describe(newest: Option[Instant], now: Instant): Option[Age] =
    newest.map { at =>
      val behind = FiniteDuration(math.max(0L, JDuration.between(at, now).toMillis), MILLISECONDS)
      Age(behind, behind >= StaleAfter)
    }
}

/**
 * Reads the newest `updatedAt` across the two mirrored collections that carry
 * one — `movies` and `screenings`, the same pair `staleness.js` measures lag
 * from (the rest stamp differently-named fields, and are written alongside a
 * corpus write anyway).
 *
 * Unindexed on `updatedAt` and it stays that way: this is a bounded
 * sort-and-take-one over a local, corpus-sized collection (12–26ms measured
 * against the biggest mirror), on a dev-only page. An unreachable mirror yields
 * None rather than throwing — the navbar then shows no age, which is what it
 * shows in prod too.
 */
class MongoMirrorFreshness(db: Option[MongoDatabase],
                           timeout: FiniteDuration = MongoConnection.LocalMirrorTimeout)
  extends MirrorFreshness with Logging {

  private val Field = "updatedAt"

  private val collections: Seq[MongoCollection[Document]] =
    db.toSeq.flatMap(database =>
      Seq(MovieRepository.Collection, ScreeningsRepository.Collection).map(database.getCollection))

  override def newestUpdate(): Option[Instant] = collections.flatMap(newestIn).maxOption

  private def newestIn(collection: MongoCollection[Document]): Option[Instant] =
    Try(Await.result(
      collection.find(Filters.exists(Field))
        .projection(Projections.include(Field))
        .sort(Sorts.descending(Field))
        .limit(1)
        .toFuture(), timeout))
      .recover { case exception => logger.debug(s"Mirror freshness read failed: ${exception.getMessage}"); Seq.empty }
      .toOption.toSeq.flatten
      .headOption
      .flatMap(document => Option(document.getDate(Field)))
      .map(date => Instant.ofEpochMilli(date.getTime))
}
