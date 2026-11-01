package services.readmodel

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/** [[WebReadModel]]'s change stamps: the model-wide `lastModified`, and each city's conditional-GET
 *  validator under it, each moving only when what it covers changes. `formerSlugs` names the slugs
 *  a city used to answer at, whose rows it still serves. */
private[readmodel] final class ReadModelStamps(clock: java.time.Clock, formerSlugs: String => Seq[String]) {
  // ATOMIC, NOT @volatile. Advancing a stamp is a read-modify-write, and the two
  // change streams deliver on different threads (the backstop scheduler and
  // /rehydrate touch the model too). @volatile would publish the write but not
  // make the update atomic, so an interleaving could LOSE one and move the stamp
  // backwards — after which a later advance can re-issue a value a client
  // already holds, which is a 304 for changed bytes. `advance` is a pure
  // function of its argument, so it is safe to re-apply on a CAS retry.
  private val _lastModified = new java.util.concurrent.atomic.AtomicReference[java.time.Instant](clock.instant())
  /** Model-wide change stamp — moves when ANYTHING in the corpus changes. The
   *  sitemap's `<lastmod>`, the `filmSlugs` memo and `/debug/readmodel` all want
   *  exactly this. A conditional GET does not: see [[lastModifiedFor]]. */
  def lastModified: java.time.Instant = _lastModified.get()

  /** The floor under every city's stamp — what versions the corpus-wide indexes. */
  def floor: java.time.Instant = _globalFloor.get()

  // ── Per-city cache validators ───────────────────────────────────────────────
  //
  // A conditional GET for one city asks a narrower question than `lastModified`
  // answers: did the bytes THAT CITY renders change? Answering it with the
  // model-wide stamp meant a Warsaw showtime invalidated London's ETag, so every
  // city's payload looked like it changed every couple of minutes and no 304 --
  // browser, mobile app or Cloudflare -- survived long enough to be worth much.
  //
  // Stamps are only ever allowed to run FAST, never slow: an over-eager bump
  // costs one revalidation, a missed one serves stale showtimes behind a 304.
  private val cityStamps = new ConcurrentHashMap[String, java.time.Instant]()

  // The floor under every city's stamp: changes no per-city bump can scope.
  //
  // ⚠️ THE SLUG CORPUS IS WHY THIS EXISTS. `FilmSlugs` assigns `/{city}/movie/{slug}`
  // addresses over the WHOLE corpus -- a film appearing anywhere can take the bare
  // slug off a film playing in a different city and silently change that city's
  // rendered links. So any change to the `(id, title, releaseYear)` projection
  // `FilmSlugs` is a pure function of moves EVERY city, and nothing else does.
  private val _globalFloor = new java.util.concurrent.atomic.AtomicReference[java.time.Instant](_lastModified.get())

  /** The conditional-GET validator for one city: the latest of the model-wide
   *  floor, the city's own stamp, and the stamps of any slug it formerly used
   *  (`screeningsForCity` still serves rows filed under those, so they are part
   *  of what the city renders).
   *
   *  The city stamps are read BEFORE the floor: a reload advances the floor past them first and
   *  only then drops them, so a stamp found gone means the floor already covers it. Read floor
   *  first, a request racing the reload took the floor from before it and the stamp from after,
   *  and answered a validator older than one already handed out. */
  def lastModifiedFor(citySlug: String): java.time.Instant = {
    val stamps = (citySlug +: formerSlugs(citySlug)).map(cityStamps.get)
    stamps.foldLeft(_globalFloor.get())(laterOf)
  }

  private def laterOf(current: java.time.Instant, candidate: java.time.Instant): java.time.Instant =
    if (candidate != null && candidate.isAfter(current)) candidate else current

  private def advance(previous: java.time.Instant): java.time.Instant = tools.MonotonicStamp.after(previous, clock)

  def touch(): Unit = { _lastModified.updateAndGet(previous => advance(previous)); () }

  /** Bump one city's validator (and the model-wide stamp with it): past its own stamp AND
   *  the floor, so `lastModifiedFor` moves even when the clock reads no later than the floor
   *  a reload just advanced (a stamp under the floor is no move at all). */
  def touchCity(citySlug: String): Unit = {
    touch()
    cityStamps.compute(citySlug, (_, previous) => advance(laterOf(_globalFloor.get(), previous)))
    ()
  }

  /** Bump the floor, and with it every city. */
  def touchEveryCity(): Unit = {
    touch()
    _globalFloor.updateAndGet(previous => advance(previous))
    ()
  }

  /** After a full reload: every city is re-derived, so no per-city stamp survives as evidence of
   *  anything; the floor alone answers for all of them — advanced past the latest of them FIRST, so
   *  no city's validator moves backwards when its stamp goes (a cached copy at the old stamp would
   *  otherwise look newer than the reload's data). Only stamps the floor now covers go: one a stream
   *  stamped meanwhile is past it, and stays. */
  def coverEveryCity(): Unit = {
    val latestCity = cityStamps.values.asScala.foldLeft(_globalFloor.get())(laterOf)
    val floor      = _globalFloor.updateAndGet(previous => advance(laterOf(previous, latestCity)))
    touch()
    cityStamps.values.removeIf(stamp => !stamp.isAfter(floor))
    ()
  }
}
