package controllers

import models.Cinema
import services.UptimeMonitor

/** Which section of the /uptime page each monitored service lands in — the triage
 *  verdicts (failing, gone upstream, no screenings, thin) and the healthy homes
 *  (cinemas by city, the enrichment services, "Other") — for ONE country's roster. */
final class UptimeTriage(country: models.Country) {

  // The city groups this deployment can render, scoped to the ONE country it
  // serves. `Cinema.byCity` spans every country at once, and rows are matched to
  // a city BY DISPLAY NAME — so an unscoped grouping put any cinema whose name is
  // shared across rosters under a foreign city too. "Studio" names a cinema in
  // both Opole and Germany, which is how the German page grew an "Opole" header
  // (and rendered that one German cinema twice).
  private val byCity: Seq[(String, Seq[Cinema])] =
    country.cities.map(c => c.labels.nominative -> c.cinemas)

  // Derived from the cinema model so every wired scraper groups under the
  // "Cinemas" header automatically — no edit needed when a cinema is added.
  // Scoped with `byCity` above: a service that ISN'T one of this country's
  // cinemas must fall through to "Other" and stay visible, rather than being
  // excluded here and dropped from the page entirely.
  private val cinemaNames = byCity.flatMap(_._2).map(_.displayName)

  // External enrichment sources plus network-level (chain-wide) detail health.
  // Cinema City and Cineworld each fetch a film's detail once per network and
  // record it here as one "<Chain> Enrichment" entry instead of one
  // "<venue>|enrichment" sub-row per venue (see EnrichDetailsHandler /
  // DetailEnricher.enrichmentServiceOverride). A name missing from this list
  // isn't dropped — `otherVerdicts` still picks it up — but it falls into
  // "Other" instead of classifying/rendering alongside the rest of the Global
  // section, which is how Cineworld Enrichment ended up filed under "Other" on
  // the UK page instead of here. Order here is the render order of the Global
  // section, so the chain-wide rows lead the external sources.
  private val enrichmentNames = Seq(
    "Cinema City Enrichment", "Cineworld Enrichment", "TMDB", "IMDb", "Filmweb", "Metacritic", "Rotten Tomatoes"
  )

  // Above this many rows a section whose size scales with the roster — the
  // healthy by-city section, and the thin one — is dropped and only its count is
  // reported. One uptime service per venue means the section scales
  // with the roster, and each rendered row carries all 96 of its 15-min slots:
  // the US (5,031 venues) is ~484k bar objects plus the same again as HTML and
  // once more as the page's SSE bootstrap JSON. That OOM-killed the US web pod on
  // 2026-08-31 — and because `pekko.jvm-exit-on-fatal-error` exits the JVM rather
  // than limping, an admin opening /uptime took the PUBLIC site down with it.
  // Nothing is lost by hiding them: the triage sections above already carry every
  // row that is failing or returning nothing, which is what the page is read for.
  // 500 keeps Poland (~60) and the UK (343) rendering as before; Germany (1,515)
  // and the US collapse. Thin rows (51 PL / 16 US on 2026-09-26) sit far below it,
  // but a feed change that left most of a country thin would not.
  private val MaxRenderedRows = 500
  private sealed trait Health
  private case object Failing extends Health
  // Red, but for a reason no one can act on: the venue's page is a 404. Split out
  // of Failing so the section that means "something broke today" stays readable.
  // Named `Missing`, not `Gone`, for the same reason `Healthy` is not `Ok`: Play's
  // `Results` already has both `Gone` and `NotFound` and a controller inherits
  // them. The section it feeds is still called "gone upstream".
  private case object Missing extends Health
  private case object Zero    extends Health
  // Green, but every recent scrape held no screening in the next 72h — a feed that
  // stopped carrying the programme and kept a stray far-off slot (Kino Polonez,
  // 2026-09-26). Least severe of the flagged verdicts.
  private case object Thin    extends Health
  private case object Healthy extends Health

  /** Split the active services into the page's sections. Two triage sections lead:
   *  rows that have been FAILING (every one of their last ≤3 active 15-min buckets
   *  is fully red) and rows returning NO SCREENINGS (last ≤3 active buckets all
   *  white) are pulled to the top — cinemas and enrichment services together, in a
   *  flat list tagged with each row's city — so a glance shows what's broken now.
   *  Everything healthy keeps its normal home: cinemas grouped by city, the
   *  enrichment-service rows, then "Other". A cinema's "<cinema>|enrichment"
   *  sub-row travels with it (and a failing sub-row promotes its cinema, keeping
   *  cinema + enrichment together). Package-private and parameterised on both
   *  lookups so the grouping is unit-testable without rendering or a Materializer.
   *
   *  The two lookups are SEPARATE on purpose. `statusesOf` decides every row's
   *  section and is asked for all of them; `row` builds the 96-slot bar series and
   *  is asked ONLY for rows that survive into a rendered section. Collapsing them
   *  back into one is what made the page cost one bar series per registered
   *  service — see MaxRenderedRows. */
  private[controllers] def groupRows(active: Set[String], statusesOf: String => Seq[String],
                                     errorsOf: String => Seq[String],
                                     row: String => ServiceRow): UptimeSections =
    new Grouping(active, statusesOf, errorsOf, row).sections

  /** One `groupRows` pass over a set of active services and its lookups. */
  private final class Grouping(active: Set[String], statusesOf: String => Seq[String],
                               errorsOf: String => Seq[String], row: String => ServiceRow) {

    private def cinemaRow(displayName: String): ServiceRow = {
      val enrichService = UptimeMonitor.enrichmentService(displayName)
      row(displayName).copy(enrichment = Option.when(active.contains(enrichService))(row(enrichService)))
    }

    // Classification runs over NAMES and statuses only — never over built rows.
    // A cinema unit is the WORST of its scrape row and its enrichment sub-row, so
    // a failing enrichment surfaces the cinema too (keeping the pair together).
    private def cinemaHealthOf(displayName: String): Health = {
      val enrichService = UptimeMonitor.enrichmentService(displayName)
      val enrichHealth =
        if (active.contains(enrichService)) verdict(enrichService) else Healthy
      worse(verdict(displayName), enrichHealth)
    }

    // Errors are read ONLY for a row that already classified as Failing — the
    // question "is this a 404" is meaningless otherwise, and asking it for all
    // 5,031 US services would undo the point of classifying over names alone.
    private def verdict(service: String): Health = classify(statusesOf(service)) match {
      case Failing if goneUpstream(errorsOf(service)) => Missing
      case other                                      => other
    }

    // Names in city order, each with its verdict. Reused by both the triage split
    // and the by-city section so a cinema's placement is decided exactly once.
    private val cinemaVerdicts: Seq[(String, String, Health)] = byCity.flatMap { case (city, venues) =>
      venues.map(_.displayName).filter(active.contains).map(dn => (city, dn, cinemaHealthOf(dn)))
    }
    private val serviceVerdicts = enrichmentNames.filter(active.contains).map(n => n -> verdict(n))
    // Exclude the "<cinema>|enrichment" services — they render as cinema sub-rows.
    private val otherVerdicts = (active -- cinemaNames.toSet -- enrichmentNames.toSet)
      .filterNot(UptimeMonitor.isEnrichmentService).toSeq.sorted.map(n => n -> verdict(n))

    // Flat triage sections, in the order cinemas → enrichment services → other,
    // each carrying the city it belongs to (None for non-cinema rows).
    private def flaggedAs(verdict: Health): Seq[FlaggedRow] =
      cinemaVerdicts.collect { case (city, dn, h) if h == verdict => FlaggedRow(cinemaRow(dn), Some(city)) } ++
        serviceVerdicts.collect { case (n, h) if h == verdict => FlaggedRow(row(n), None) } ++
        otherVerdicts.collect { case (n, h) if h == verdict => FlaggedRow(row(n), None) }

    // The remainder (healthy / mixed) keeps its normal home — up to a point. Past
    // MaxRenderedRows the by-city section is DROPPED rather than rendered:
    // see the constant for why a US-sized roster cannot be put on the page.
    private val healthyCinemas = cinemaVerdicts.collect { case (city, dn, Healthy) => city -> dn }
    private val collapsed      = healthyCinemas.sizeIs > MaxRenderedRows

    private def cinemasByCity: Seq[(String, Seq[ServiceRow])] =
      if (collapsed) Seq.empty
      else byCity.flatMap { case (city, _) =>
        val rows = healthyCinemas.collect { case (c, dn) if c == city => cinemaRow(dn) }
        Option.when(rows.nonEmpty)(city -> rows)
      }

    // Counted over verdicts, before any row is built, for the same reason.
    private val thinCount     = (cinemaVerdicts.map(_._3) ++ serviceVerdicts.map(_._2) ++ otherVerdicts.map(_._2)).count(_ == Thin)
    private val thinCollapsed = thinCount > MaxRenderedRows

    def sections: UptimeSections = {
      val failing = flaggedAs(Failing)
      val gone    = flaggedAs(Missing)
      val zero    = flaggedAs(Zero)
      val thin    = if (thinCollapsed) Seq.empty else flaggedAs(Thin)
      UptimeSections(
        failing = failing,
        gone = gone,
        zero = zero,
        thin = thin,
        hiddenThin = if (thinCollapsed) thinCount else 0,
        cinemasByCity = cinemasByCity,
        services = serviceVerdicts.collect { case (n, Healthy) => row(n) },
        other = otherVerdicts.collect { case (n, Healthy) => row(n) },
        hiddenHealthyCinemas = if (collapsed) healthyCinemas.size else 0
      )
    }
  }

  /** Classify a bar series by its last `RecentScrapes` buckets that recorded any
   *  activity (ignoring untouched "empty" slots): all red ⇒ Failing, all white
   *  ⇒ Zero, all green-but-thin ⇒ Thin, anything else (any success, or no activity at all) ⇒ Healthy. A lone
   *  red blip among greens stays Healthy; a brand-new service that has only ever
   *  failed is Failing from its first bucket. Yellow (partial failure) is NOT
   *  failing. */
  private def classify(statuses: Seq[String]): Health = {
    val recent = statuses.iterator.filter(_ != "empty").toSeq.takeRight(UptimeTriage.RecentScrapes)
    if (recent.isEmpty) Healthy
    else if (recent.forall(_ == "red")) Failing
    else if (recent.forall(_ == "zero")) Zero
    else if (recent.forall(_ == "thin")) Thin
    else Healthy
  }

  /** Durable evidence in the window clearly dominates — nothing else broke, the
   *  venue simply isn't there. A LONE non-404 (a 500, a timeout, a WAF hiccup) is
   *  tolerated when durable errors outnumber it more than 2-to-1; anything short
   *  of that makes it an ordinary failure again, a venue whose page EXISTS and is
   *  misbehaving, which is worth acting on.
   *
   *  The tolerance exists because "every recorded error" used to mean literally
   *  every one: Kino Center's filmstarts.de page 404s on 8 of its last 9 recorded
   *  attempts, with one stray HTTP 503 (confirmed still 404 live) — and because
   *  the window is a sliding 3-bucket recency check, not a durable "since" marker,
   *  a single future 503 could always land inside it before the last one aged
   *  out, keeping a genuinely dead page in Failing forever. Two or more non-404s
   *  still describe a venue flickering between gone and merely broken, so those
   *  stay failing.
   *
   *  Recency, not duration, is the test here — the buckets only reach back a day.
   *  The worker asks the longer question against the scrape archive
   *  ([[services.scrapes.GoneUpstream]], "404ing for over a day") before it backs
   *  a venue's scrape cadence off; both sides share the one predicate that
   *  matters, `isNotFound`. */
  private def goneUpstream(errors: Seq[String]): Boolean = {
    val (durable, other) = errors.partition(services.scrapes.GoneUpstream.saysPageIsGone)
    other.sizeIs <= 1 && durable.size > other.size * 2
  }

  /** Worst-first, and `Missing` ranks BELOW Failing: a cinema whose page is gone
   *  but whose enrichment is also failing has something live to fix, so it belongs
   *  in Failing. */
  private def worse(a: Health, b: Health): Health =
    if (a == Failing || b == Failing) Failing
    else if (a == Missing || b == Missing) Missing
    else if (a == Zero || b == Zero) Zero
    else if (a == Thin || b == Thin) Thin
    else Healthy
}

object UptimeTriage {
  // Triage classification: a row is judged by its most recent activity. A cinema
  // scrapes roughly every ~5 min (legacy) / ≤15 min (queue), so the last ≤3
  // buckets that recorded anything ≈ "the last few scrapes".
  val RecentScrapes = 3
}

/** A row promoted into the leading "Failing" / "No screenings" triage sections,
 *  carrying the city it was pulled out of (None for non-cinema rows). */
case class FlaggedRow(row: ServiceRow, city: Option[String])

/** The /uptime page's sections, as `groupRows` decides them. A named record
 *  rather than a tuple because the sections are not interchangeable and the
 *  hidden-cinema count only makes sense beside the section it explains. */
case class UptimeSections(
  failing: Seq[FlaggedRow],
  // Red, but only ever with a 404: the venue's page no longer exists. Its own
  // section so it stops crowding `failing`, which is the list that means
  // "something broke and someone should look".
  gone: Seq[FlaggedRow],
  zero: Seq[FlaggedRow],
  // Green on every recent scrape, each with nothing in the next 72h (see Thin).
  thin: Seq[FlaggedRow],
  cinemasByCity: Seq[(String, Seq[ServiceRow])],
  services: Seq[ServiceRow],
  other: Seq[ServiceRow],
  // Healthy cinemas deliberately NOT rendered because the roster is too large.
  // 0 whenever the by-city section is shown in full.
  hiddenHealthyCinemas: Int = 0,
  // Thin rows not rendered for the same reason; 0 whenever the section is shown.
  hiddenThin: Int = 0
)
