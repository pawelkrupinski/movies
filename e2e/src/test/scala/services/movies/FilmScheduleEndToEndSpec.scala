package services.movies

import controllers.{FilmSchedule, MovieControllerService}
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.DomainEvent
import tools.FixtureTestWiring

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.Locale
import java.time.{LocalDate, LocalDateTime}
import scala.collection.mutable

/**
 * End-to-end regression: run the full production pipeline against the
 * recorded fixture and assert that `MovieControllerService.toSchedules`
 * (which drives the `/` view) emits the complete `FilmSchedule` for
 * the anchor film `"Zawodowcy"` — every field a user would see on
 * the home page card, plus every enrichment field on the underlying
 * `MovieRecord`.
 *
 * Wires every service `AppLoader` wires (cache, TMDB stage, four
 * `*Ratings` listeners, `ImdbIdResolver`, `UnscreenedCleanup`). Reads
 * cinema HTML / JSON and TMDB / IMDb / MC / RT / Filmweb responses
 * from `test/resources/fixtures/08-06-2026/`. No Play server, no real
 * network, no stubs — the only test-only swaps are `InMemoryMovieRepository`
 * and `FakeHttpFetch`.
 *
 * The flow:
 *   1. `runOneScrapeTick` — sequential mirror of `cinemaScrapeRunner.run`,
 *      so the test doesn't race the production scheduler.
 *   2. `drainServices()` — drains the `cascadeDrainOrder` worker pools
 *      (MovieService → ImdbIdResolver → the four `*Ratings`) so every
 *      bus event has fired and every downstream HTTP call has resolved.
 *   3. `unscreenedCleanup.removeUnscreened()` — mirrors the daily
 *      cleanup pass that fires ~20 s into production boot.
 *   4. `movieControllerService.toSchedules(now)` — pinned to the
 *      fixture's capture date so the recorded showtimes are still
 *      "future".
 *
 * The disappearance regression this guards against: a row that lands in
 * the cache but goes invisible to users (empty `cinemaShowings`, dub
 * variant folded into regular and overwriting its slot, etc.).
 * Asserting the full `FilmSchedule` makes that mode loud — any field
 * regressing to None or wrong value fails specifically.
 */
class FilmScheduleEndToEndSpec extends AnyFlatSpec with Matchers {
  // Anchor film: Guy Ritchie's "Zawodowcy" (orig. "In the Grey") — a
  // mainstream current release showing across six Poznań cinemas in the
  // fixture window, fully enriched (TMDB + IMDb + MC + RT + Filmweb). It
  // replaced "Diabeł ubiera się u Prady 2" once that left cinemas. Values
  // pulled from the recorded fixture (`/3/search/movie.<hash>` for the
  // Polish title, then `/3/movie/1122573/external_ids` for the IMDb id).
  // If TMDB reassigns the film to a different id later, re-record with
  // `RecordAllDataToFixture` and update these.
  private val AnchorTitle  = "Zawodowcy"
  private val AnchorTmdbId = 1122573
  private val AnchorImdbId = "tt27681354"

  // Dub scenario: a Helios/Cinema City secondary-language variant of a
  // DIFFERENT current film kept as its own `FilmSchedule` row sharing the
  // base film's tmdbId. The anchor "Zawodowcy" has no dub variant in this
  // corpus, so the "dub stays a distinct row, doesn't fold into the
  // regular" invariant is exercised against "Straszny film" (orig. "Scary
  // Movie"), which Cinema City scrapes both as the regular Polish row and
  // as "Straszny film ukraiński dubbing" (Ukrainian-dub). Same tmdbId,
  // distinct cleanTitle → two schedules.
  private val DubBaseTitle = "Straszny film"
  private val DubTitle     = "Straszny film ukraiński dubbing"
  private val DubTmdbId    = 1273221

  private val now  = LocalDateTime.of(2026, 6, 8, 0, 0)
  private val seen = mutable.ListBuffer.empty[DomainEvent]

  // Boot the full pipeline ONCE and share it across both tests — the scrape
  // tick is this spec's dominant cost, so booting per test was redundant. The
  // boot is the production shape (tick + cascade drain + cleanup); the second
  // test runs one further tick on this same wiring, which is the "across
  // multiple scrape ticks" invariant it needs.
  private lazy val wiring: FixtureTestWiring = {
    val w = new FixtureTestWiring("08-06-2026")
    w.eventBus.subscribe { case e => seen.append(e) }
    w.bootStartup()
    w
  }
  // The web app's read transform, built from the read model the worker
  // projected — the seam the two apps share in production (web serves the
  // denormalised documents), exercised here in one JVM.
  private lazy val schedules: Seq[FilmSchedule] =
    new MovieControllerService(wiring.webReadModel).toSchedules(Poznan, now)

  // The rendered `FilmSchedule` carries the resolved metadata (what the user
  // sees) but not the source-data provenance — `cinemaData`, `cinemaTitles`,
  // tmdbId/imdbId live on the worker's `MovieRecord`. Join back to it by film
  // id so this spec can still assert the full pipeline (worker merge + web
  // render) end to end.
  private lazy val recordByFilmId: Map[String, MovieRecord] =
    wiring.movieCache.snapshot().map(r => services.readmodel.ReadModelProjection.filmId(r) -> r.record).toMap
  private def recordFor(s: FilmSchedule): Option[MovieRecord] = recordByFilmId.get(s.resolved._id)

  "the full enrichment pipeline" should
    "keep the anchor film visible after a startup scrape + event drain + cleanup tick" in {

    // 6. ASSERT: the anchor is renderable on the home page. `toSchedules`
    //    drives the `/` view in production — it filters the cache snapshot
    //    by "has at least one future showtime", groups per-cinema, and
    //    yields one `FilmSchedule` per visible card. Asserting against
    //    `toSchedules` (rather than the raw cache snapshot) catches the
    //    real disappearance mode: a row that's in the cache but invisible
    //    to users.
    //
    //    `now` is pinned (class field) to the fixture's capture date so the
    //    recorded showtimes are "in the future" relative to the test clock;
    //    `schedules` is the shared, once-booted result.
    val anchorSchedules = schedules.filter(s =>
      recordFor(s).exists(e => e.tmdbId.contains(AnchorTmdbId) || e.imdbId.contains(AnchorImdbId))
    )

    def cinemasIn(s: FilmSchedule): Set[Cinema] =
      s.showings.flatMap(_._2).map(_.cinema).toSet
    def showtimesAt(s: FilmSchedule, cinema: Cinema): Int =
      s.showings.flatMap(_._2).filter(_.cinema == cinema).flatMap(_.showtimes).size

    val diag = anchorSchedules.map { s =>
      val perCinema = cinemasIn(s).toSeq.sortBy(_.displayName)
        .map(c => s"${c.displayName}->${showtimesAt(s, c)}st").mkString(",")
      s"title='${s.movie.title}' cinemas=$perCinema"
    }.mkString(" | ")

    withClue(s"events seen=${seen.size}, schedules=${schedules.size}, anchor schedules: $diag\n") {
      anchorSchedules should not be empty

      // ── The anchor's Polish schedule ────────────────────────────────────
      //
      // The card every user sees: the canonical Polish row built from six
      // Poznań cinema scrapes that all normalise to the same cleanTitle.
      // Asserts every visible field, not just structural shape, so a
      // regression in *any* enrichment client (TMDB / IMDb / MC / RT /
      // Filmweb) or any cinema parser is caught here.
      val regular = anchorSchedules.find(_.movie.title == AnchorTitle).getOrElse(
        fail(s"no regular '$AnchorTitle' schedule found: $diag")
      )

      regular.movie.title          shouldBe AnchorTitle
      regular.movie.runtimeMinutes shouldBe Some(97)
      regular.movie.releaseYear    shouldBe Some(2026)

      // posterUrl: no Multikino slot scrapes the anchor in this Poznań-centric
      // corpus, so the Charlie Monroe (Kino Malta) poster wins the merge.
      // Specific URL nailed down — a parser regression dropping the poster
      // field would shift this.
      regular.posterUrl shouldBe Some(
        "https://www.kinomalta.pl/wp-content/uploads/2026/05/zawodowcy-zawodowcy-plakat-po-zmniejszeniu-200x288.jpg"
      )

      // Merged `MovieRecord.synopsis` picks the longest non-empty across all
      // cinemas that scrape this film. The exact text moves with the corpus,
      // so assert it's a substantial Polish synopsis and let the whole-corpus
      // snapshot below pin the exact value.
      regular.synopsis.map(_.length).getOrElse(0) should be > 400

      // Cast accessor picks the longest non-empty list across sources. Assert
      // the headline leads are always present rather than pinning an exact set
      // whose size moves with the TMDB match.
      regular.cast.toSet should contain allElementsOf Set(
        "Henry Cavill", "Jake Gyllenhaal", "Eiza González", "Rosamund Pike"
      )
      regular.director shouldBe Seq("Guy Ritchie")

      // Every cinema scraping the anchor exposes a deep-link to its own film
      // page. Assert the six Poznań deep-links are all present (the regression
      // this guards); the whole-corpus snapshot below pins the full set.
      regular.cinemaFilmUrls.toSet should contain allElementsOf Set(
        Helios                -> "https://helios.pl/poznan/kino-helios/filmy/zawodowcy-4461",
        KinoPalacowe          -> "http://kinopalacowe.pl/filmy/14534-zawodowcy/",
        CharlieMonroe         -> "https://kinomalta.pl/movies/zawodowcy",
        CinemaCityKinepolis   -> "https://www.cinema-city.pl/filmy/zawodowcy/8118s2r",
        Rialto                -> "https://www.kinorialto.poznan.pl/wydarzenie/?id=157065",
        CinemaCityPoznanPlaza -> "https://www.cinema-city.pl/filmy/zawodowcy/8118s2r"
      )

      // Showings, per cinema. Numbers are whatever each cinema happened to
      // schedule in the fixture window. The point is "the right cinemas are
      // present with their full schedules".
      val byCinema: Map[Cinema, Int] = regular.showings.flatMap(_._2)
        .groupBy(_.cinema).view.mapValues(_.flatMap(_.showtimes).size).toMap
      byCinema shouldBe Map(
        Helios                -> 6,
        KinoPalacowe          -> 4,
        CharlieMonroe         -> 2,
        CinemaCityKinepolis   -> 12,
        Rialto                -> 4,
        CinemaCityPoznanPlaza -> 8
      )

      // Date range covers the fixture window — the four consecutive days the
      // anchor screens, starting from the pinned `now`.
      regular.showings.map(_._1).toSet shouldBe Set(
        LocalDate.of(2026, 6, 8), LocalDate.of(2026, 6, 9),
        LocalDate.of(2026, 6, 10), LocalDate.of(2026, 6, 11)
      )

      // Enrichment — every external service contributed. Read from the worker's
      // MovieRecord (the read model drops source data + ids).
      val enrichment = recordFor(regular).getOrElse(fail("regular schedule missing enrichment"))
      enrichment.tmdbId            shouldBe Some(AnchorTmdbId)
      enrichment.imdbId            shouldBe Some(AnchorImdbId)
      enrichment.originalTitle     shouldBe Some("In the Grey")
      enrichment.imdbRating        shouldBe Some(6.3)
      enrichment.metascore         shouldBe Some(53)
      enrichment.rottenTomatoes    shouldBe Some(47)
      enrichment.filmwebRating     shouldBe Some(6.07988)
      enrichment.metacriticUrl     shouldBe Some("https://www.metacritic.com/movie/in-the-grey")
      enrichment.rottenTomatoesUrl shouldBe Some("https://www.rottentomatoes.com/m/in_the_grey")
      enrichment.filmwebUrl        shouldBe Some("https://www.filmweb.pl/film/Zawodowcy-2026-10051619")

      // Cinemas keep their RAW reported spelling as provenance — some report
      // all-caps "ZAWODOWCY", some "Zawodowcy". `displayTitle` ranks those raw
      // spellings (the all-caps one ranks low) and then `recase`s the winner, so
      // the home-page card reads "Zawodowcy" and never shouts.
      enrichment.cinemaTitles should contain allElementsOf Set(
        "Zawodowcy",
        "ZAWODOWCY"
      )
      enrichment.displayTitle(AnchorTitle) shouldBe AnchorTitle
      regular.movie.title                  shouldBe enrichment.displayTitle(AnchorTitle)

      // Provenance: every cinema that reported this row contributes a slot in
      // `cinemaData`. Year-divergence is intentional — Charlie Monroe drops
      // the year, the CC slots carry 2025, the rest 2026. The redirect in
      // `recordCinemaScrape` collapses all six Poznań slots onto one CacheKey.
      // Assert containment (other cities' cinemas also fold onto this CacheKey)
      // — the whole-corpus snapshot pins the full cross-city provenance.
      enrichment.cinemaData.keySet should contain allElementsOf Set(
        CharlieMonroe, CinemaCityKinepolis, KinoPalacowe, Rialto, Helios, CinemaCityPoznanPlaza
      )
      enrichment.cinemaData(CharlieMonroe).title              shouldBe Some("Zawodowcy")
      enrichment.cinemaData(CharlieMonroe).releaseYear        shouldBe None
      enrichment.cinemaData(CinemaCityKinepolis).title        shouldBe Some("Zawodowcy")
      enrichment.cinemaData(CinemaCityKinepolis).releaseYear  shouldBe Some(2025)
      enrichment.cinemaData(KinoPalacowe).title               shouldBe Some("Zawodowcy")
      enrichment.cinemaData(KinoPalacowe).releaseYear         shouldBe Some(2026)
      // Rialto reports the title ALL-CAPS and now keeps that raw spelling in its
      // provenance slot (casing moved out of the client to `displayTitle.recase`).
      // The all-caps slot ranks low in the picker, so `displayTitle` below is the
      // proper-cased "Zawodowcy".
      enrichment.cinemaData(Rialto).title                     shouldBe Some("ZAWODOWCY")
      enrichment.cinemaData(Rialto).releaseYear               shouldBe Some(2026)
      enrichment.cinemaData(Helios).title                     shouldBe Some("Zawodowcy")
      // Helios fetches the year via its REST `/cinema/.../screening` endpoint
      // — the fixture URL bakes the current date (`LocalDate.now`), so the
      // fixture mismatches on any day after the recording date and the year
      // drops to None. Accepts both shapes so the spec doesn't rot every
      // midnight; the snapshot test downstream covers the day-of-recording
      // shape if anyone re-records the corpus.
      enrichment.cinemaData(Helios).releaseYear               should (be (Some(2026)) or be (None))
      enrichment.cinemaData(CinemaCityPoznanPlaza).title      shouldBe Some("Zawodowcy")
      enrichment.cinemaData(CinemaCityPoznanPlaza).releaseYear shouldBe Some(2025)

      // The anchor has a single visible schedule — no dub variant for this
      // film in the corpus. The dub-stays-distinct invariant is exercised
      // against "Straszny film" below.
      anchorSchedules should have size 1

      // ── Secondary-script dub stays a distinct row ───────────────────────
      //
      // The anchor itself has no dub variant, so the "a Helios/CC
      // secondary-language dub keeps its own FilmSchedule rather than folding
      // into the regular row" invariant — the original Prada-Cyrillic guard —
      // is exercised here against "Straszny film" (orig. "Scary Movie"), which
      // Cinema City scrapes both as the regular Polish row and as "Straszny
      // film ukraiński dubbing" (Ukrainian dub). Both carry the same tmdbId;
      // the cleanTitle-strict fold gate must keep them as two rows. If the dub
      // disappears the gate is over-aggressive; if it folds onto the regular
      // row, CC Plaza's regular slot would inflate by the dub's showtimes.
      val dubSiblings = schedules.filter(s => recordFor(s).exists(_.tmdbId.contains(DubTmdbId)))
      withClue(s"dub siblings: ${dubSiblings.map(_.movie.title).mkString(" | ")}\n") {
        dubSiblings should have size 2

        val dubRegular = dubSiblings.find(_.movie.title == DubBaseTitle).getOrElse(
          fail(s"no regular '$DubBaseTitle' schedule found")
        )
        val dub = dubSiblings.find(_.movie.title == DubTitle).getOrElse(
          fail(s"no dub '$DubTitle' schedule found alongside the regular")
        )

        // Both rows inherit the same TMDB-resolved enrichment — only the
        // cinema-side data differs.
        recordFor(dubRegular).flatMap(_.tmdbId) shouldBe Some(DubTmdbId)
        dub.movie.title          shouldBe DubTitle
        dub.movie.runtimeMinutes shouldBe Some(94)
        dub.movie.releaseYear    shouldBe Some(2026)

        // Only Cinema City Poznań Plaza scrapes the dub in Poznań — its own
        // deep-link, its own slot. Anything else here is cross-cleanTitle bleed.
        dub.cinemaFilmUrls shouldBe Seq(
          CinemaCityPoznanPlaza -> "https://www.cinema-city.pl/filmy/straszny-film-ukrainski-dubbing/8117s2r1"
        )
        val dubByCinema: Map[Cinema, Int] = dub.showings.flatMap(_._2)
          .groupBy(_.cinema).view.mapValues(_.flatMap(_.showtimes).size).toMap
        dubByCinema shouldBe Map(CinemaCityPoznanPlaza -> 4)

        val dubEnrichment = recordFor(dub).getOrElse(fail("dub schedule missing enrichment"))
        dubEnrichment.tmdbId shouldBe Some(DubTmdbId)
        // Many Cinema City branches across cities scrape the dub and fold onto
        // one CacheKey, so assert containment of the Poznań slot plus the
        // cleanliness invariant: every provenance slot carries the dub title,
        // never the base "Straszny film" — that's the cross-cleanTitle bleed
        // this guards against.
        dubEnrichment.cinemaData.keySet                             should contain (CinemaCityPoznanPlaza)
        dubEnrichment.cinemaData(CinemaCityPoznanPlaza).title       shouldBe Some(DubTitle)
        dubEnrichment.cinemaData(CinemaCityPoznanPlaza).releaseYear shouldBe Some(2026)
        dubEnrichment.cinemaData.values.flatMap(_.title).foreach(_ shouldBe DubTitle)
      }
    }

    // ── Whole-corpus snapshot ─────────────────────────────────────────────
    //
    // Every other `FilmSchedule` gets the same depth of assertion that
    // the anchor gets above — every visible field, every cinemaFilmUrl, every
    // enrichment value, every showtime with its room + format tokens.
    // Inline literals for 214 movies × ~10 fields × N days × M slots
    // would be unreadable, so the expected output is a checked-in
    // snapshot file. The test renders the full corpus deterministically
    // (titles sorted, cinemas sorted by display name, showtimes sorted
    // by clock) and compares the result against the snapshot.
    //
    // When the snapshot doesn't exist (first run after a fresh fixture
    // recording), the test writes it and fails loudly so the diff goes
    // through code review before being trusted. To regenerate after a
    // legitimate change: delete the file and re-run.
    val snapshotPath = Paths.get("test/resources/fixtures/08-06-2026/expected-schedules.txt")
    val actual = renderSchedules(schedules)
    if (!Files.exists(snapshotPath)) {
      Files.write(snapshotPath, actual.getBytes(StandardCharsets.UTF_8))
      fail(s"Snapshot didn't exist — wrote ${snapshotPath}. Review the contents, commit, and re-run.")
    }
    val expected = new String(Files.readAllBytes(snapshotPath), StandardCharsets.UTF_8)
    withClue(
      s"Whole-corpus snapshot mismatch. To regenerate after an intentional change:\n" +
        s"  rm $snapshotPath && sbt 'e2e/testOnly services.movies.FilmScheduleEndToEndSpec'\n"
    ) {
      actual shouldBe expected
    }
  }

  /** Render every FilmSchedule into a deterministic multi-line block. One
   *  block per film, separated by blank lines; films sorted alphabetically
   *  by display title. Each block lists every field a viewer of the `/`
   *  card would see (title, runtime, year, poster, synopsis size, cast,
   *  director, per-cinema deep-links) plus every enrichment value (tmdbId,
   *  imdbId, ratings, MC/RT/FW URLs, per-cinema slot provenance) plus the
   *  full per-(date, cinema) showtime list with room + format tokens. */
  private def renderSchedules(schedules: Seq[FilmSchedule]): String =
    schedules.sortBy(s => (s.movie.title.toLowerCase(Locale.ROOT), s.movie.releaseYear)).map(renderOne).mkString("\n\n")

  private def renderOne(s: FilmSchedule): String = {
    val e = recordFor(s)
    val cinemaUrls = s.cinemaFilmUrls.sortBy(_._1.displayName)
      .map { case (c, u) => s"${c.displayName} = $u" }
    val scrapes = e.map(_.cinemaData.toSeq
      .sortBy { case (c, sd) => (c.displayName, sd.title.getOrElse(""), sd.releaseYear.getOrElse(Int.MinValue)) }
      .map { case (c, sd) =>
        s"${c.displayName} / ${sd.title.getOrElse("—")} / ${sd.releaseYear.map(_.toString).getOrElse("—")}"
      })
      .getOrElse(Nil)
    // `cinemaTitles` is the set of raw spellings each cinema reported.
    // `displayTitle` is the picker's choice across that set — the
    // canonical form that `MovieController.toSchedules` writes into
    // `Movie.title` (and therefore the `=== TITLE ===` header above).
    // Showing both makes the picker's behaviour explicit per film.
    val cinemaTitles = e.map(_.cinemaTitles.toSeq.sorted).getOrElse(Nil)
    val showings = s.showings.sortBy(_._1).flatMap { case (date, byCinema) =>
      byCinema.sortBy(_.cinema.displayName).map { sht =>
        val slots = sht.showtimes.sortBy(_.dateTime).map { st =>
          val room   = st.room.fold("")(r => s" $r")
          val format = if (st.format.isEmpty) "" else s" ${st.format.mkString("/")}"
          s"${st.dateTime.toLocalTime}$room$format"
        }.mkString(" · ")
        f"  $date  ${sht.cinema.displayName}%-28s  $slots"
      }
    }
    val lines = Seq(
      s"=== ${s.movie.title} ===",
      s"displayTitle:      ${s.movie.title}",
      s"cinemaTitles:      ${if (cinemaTitles.isEmpty) "—" else cinemaTitles.mkString(" | ")}",
      s"runtimeMinutes:    ${s.movie.runtimeMinutes.map(_.toString).getOrElse("—")}",
      s"releaseYear:       ${s.movie.releaseYear.map(_.toString).getOrElse("—")}",
      s"countries:         ${if (s.movie.countries.isEmpty) "—" else s.movie.countries.mkString(", ")}",
      s"posterUrl:         ${s.posterUrl.getOrElse("—")}",
      s"synopsis.length:   ${s.synopsis.map(_.length.toString).getOrElse("—")}",
      s"cast:              ${if (s.cast.nonEmpty) s.cast.mkString(", ") else "—"}",
      s"director:          ${if (s.director.nonEmpty) s.director.mkString(", ") else "—"}",
      s"tmdbId:            ${e.flatMap(_.tmdbId).map(_.toString).getOrElse("—")}",
      s"imdbId:            ${e.flatMap(_.imdbId).getOrElse("—")}",
      s"originalTitle:     ${e.flatMap(_.originalTitle).getOrElse("—")}",
      s"imdbRating:        ${e.flatMap(_.imdbRating).map(_.toString).getOrElse("—")}",
      s"metascore:         ${e.flatMap(_.metascore).map(_.toString).getOrElse("—")}",
      s"rottenTomatoes:    ${e.flatMap(_.rottenTomatoes).map(_.toString).getOrElse("—")}",
      s"filmwebRating:     ${e.flatMap(_.filmwebRating).map(_.toString).getOrElse("—")}",
      s"metacriticUrl:     ${e.flatMap(_.metacriticUrl).getOrElse("—")}",
      s"rottenTomatoesUrl: ${e.flatMap(_.rottenTomatoesUrl).getOrElse("—")}",
      s"filmwebUrl:        ${e.flatMap(_.filmwebUrl).getOrElse("—")}"
    ) ++
      (if (cinemaUrls.nonEmpty) Seq("cinemaFilmUrls:") ++ cinemaUrls.map("  " + _) else Seq("cinemaFilmUrls:    —")) ++
      (if (scrapes.nonEmpty) Seq("cinemaScrapes:") ++ scrapes.map("  " + _) else Seq("cinemaScrapes:     —")) ++
      Seq("showings:") ++ showings
    lines.mkString("\n")
  }

  // Reproduction attempt: a dub variant ("Straszny film ukraiński dubbing")
  // and its base Polish row ("Straszny film") share one tmdbId. Pre-fix data
  // showed the dub row's per-cinema slots polluted with the base title's
  // entries (and vice versa). With the cleanTitle-strict fold gate in place,
  // no current code path should bleed across the two rows; this case proves
  // it across two scrape ticks back-to-back, which is when accumulation would
  // normally show.
  //
  // (The original guard used Prada's Helios Cyrillic dub "ДИЯВОЛ НОСИТЬ
  // ПРАДА 2"; that film left cinemas, so the same invariant is now exercised
  // against the Straszny-film Ukrainian-dub pair — the closest current
  // secondary-language sibling in the corpus.)
  //
  // The two-tick shape catches:
  //   - re-scrape adding cinema slots to the wrong row,
  //   - identity-gate fold firing on second-pass tmdbId hits,
  //   - bus events from the second tick triggering a TMDB re-resolve
  //     that lands on the wrong sibling.
  //
  // If this assertion ever fails, the bug is in current code; otherwise
  // the user's production data is purely legacy folded state.
  it should "keep the dub variant's cinema slots clean across multiple scrape ticks" in {
    // The shared `wiring` boot already ran the canonical first tick (+ drain +
    // cleanup). One more tick here re-runs every scraper against the same
    // fixtures — every (cinema, title, year) tuple is unchanged, so
    // `recordCinemaScrape`'s `isNew` check should suppress every bus event. If
    // a regression makes the redirect cross cleanTitles, or the identity-gate
    // fold misbehave on already-resolved rows, the dub row's cinema slots pick
    // up the base title's entries here.
    wiring.runOneScrapeTick()

    val dubRow = wiring.movieCache.snapshot()
      .find(_.title == DubTitle)
      .getOrElse(fail(s"dub row '$DubTitle' missing from cache after two scrape ticks"))

    withClue(s"cinema slot drift after second scrape tick: ${dubRow.record.cinemaData}\n") {
      // Many Cinema City branches scrape the dub title; they all fold onto one
      // CacheKey. Every provenance slot must still carry the dub title (year
      // 2026) and never the base "Straszny film" — any base entry here is the
      // cross-cleanTitle bleed this guards against.
      dubRow.record.cinemaData.keySet                             should contain (CinemaCityPoznanPlaza)
      dubRow.record.cinemaData(CinemaCityPoznanPlaza).title       shouldBe Some(DubTitle)
      dubRow.record.cinemaData(CinemaCityPoznanPlaza).releaseYear shouldBe Some(2026)
      dubRow.record.cinemaData.values.flatMap(_.title).foreach(_ shouldBe DubTitle)
    }

    // Conversely, the regular Polish row mustn't pick up the dub scrape
    // either — same invariant from the other side.
    val regularRow = wiring.movieCache.snapshot()
      .find(_.title == DubBaseTitle)
      .getOrElse(fail(s"regular '$DubBaseTitle' row missing from cache after two scrape ticks"))
    withClue(s"base row picked up dub title: ${regularRow.record.cinemaData}\n") {
      regularRow.record.cinemaData.values
        .flatMap(_.title)
        .foreach { t => t should not be DubTitle }
    }
  }

}
