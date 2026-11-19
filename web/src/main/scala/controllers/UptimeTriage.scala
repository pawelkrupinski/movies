package controllers

import controllers.UptimeVerdict.{Failing, Healthy, Missing, Thin, Zero, Health}
import models.{Cinema, EnrichmentUptimeService}

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
      val enrichService = EnrichmentUptimeService.forCinema(displayName)
      row(displayName).copy(enrichment = Option.when(active.contains(enrichService))(row(enrichService)))
    }

    // Classification runs over NAMES and statuses only — never over built rows.
    // A cinema unit is the WORST of its scrape row and its enrichment sub-row, so
    // a failing enrichment surfaces the cinema too (keeping the pair together).
    private def cinemaHealthOf(displayName: String): Health = {
      val enrichService = EnrichmentUptimeService.forCinema(displayName)
      val enrichHealth =
        if (active.contains(enrichService)) verdict(enrichService) else Healthy
      UptimeVerdict.worse(verdict(displayName), enrichHealth)
    }

    private def verdict(service: String): Health = UptimeVerdict.of(statusesOf(service), errorsOf(service))

    // Names in city order, each with its verdict. Reused by both the triage split
    // and the by-city section so a cinema's placement is decided exactly once.
    private val cinemaVerdicts: Seq[(String, String, Health)] = byCity.flatMap { case (city, venues) =>
      venues.map(_.displayName).filter(active.contains).map(dn => (city, dn, cinemaHealthOf(dn)))
    }
    private val serviceVerdicts = enrichmentNames.filter(active.contains).map(n => n -> verdict(n))
    // Exclude the "<cinema>|enrichment" services — they render as cinema sub-rows.
    private val otherVerdicts = (active -- cinemaNames.toSet -- enrichmentNames.toSet)
      .filterNot(EnrichmentUptimeService.matches).toSeq.sorted.map(n => n -> verdict(n))

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
