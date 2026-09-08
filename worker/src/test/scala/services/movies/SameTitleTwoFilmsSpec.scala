package services.movies

import models.{CharlieMonroe, Cinema, CinemaMovie, Helios, KinoApollo, KinoMuza, KinoPalacowe, Movie, Multikino, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.staging.{InMemoryStagingFolder, InMemoryStagingRepository}

import java.time.LocalDateTime

/**
 * Two DIFFERENT films share one Polish title, and the cinemas screening one of
 * them publish no year to say which. Prod, 2026-08-14, `/poznan`:
 *
 *   - `tylkojednanoc|2026` — tmdbId 1433367, "One Night Only", 102 min.
 *   - `tylkojednanoc|1961` — tmdbId 41050, Antonioni's "La notte", 121 min.
 *
 * The page rendered the SAME film twice under one slug, each card carrying the
 * same Multikino / Cinema City booking links, because the venues' listings were
 * split across the two rows. What decided the split was `concludedKeyFor`: with
 * no year on the listing it ranked the concluded same-title rows by
 * `canonicalRank` and took the smallest — and `canonicalRank` breaks ties on the
 * LOWER YEAR, so the 1961 film won every time.
 *
 * That tie-break is a canonicalisation rule for rows of ONE film (prefer the
 * year-bearing spelling, then the lower year). Applied across two films it means
 * "the older film always wins", which is exactly backwards: what cinemas screen
 * without printing a year is overwhelmingly the new release, not the classic.
 *
 * The archived PL corpus says how common this is: of 33 listings of
 * "Tylko jedna noc" across 31 cinemas, 32 carry NO release year and none carries
 * an `originalTitle`. So the year is not available to disambiguate, and
 * `MixedFilmDetector` — which needs a published original title — cannot speak
 * either. The listing's RUNTIME is what those cinemas do publish, and it is the
 * same evidence `MixedFilmDetector.corroborated` already trusts to tell two films
 * apart — read here as NEARNESS, not as agreement within a tolerance. TMDB says
 * 102 minutes; Cinema City publishes 102 but Multikino publishes 105, so a ±2
 * agreement test answers "neither film" for Multikino and it falls straight back
 * to the wrong guess. 105 is 3 minutes from the romcom and 16 from "La notte" —
 * not a close call.
 */
class SameTitleTwoFilmsSpec extends AnyFlatSpec with Matchers {

  private val Title    = "Tylko jedna noc"
  private val NewFilm  = 1433367   // "One Night Only" (2026), TMDB runtime 102
  private val OldFilm  = 41050     // "La notte"       (1961), TMDB runtime 121
  private val When     = LocalDateTime.of(2026, 8, 14, 20, 5)

  private def cache() = new CaffeineMovieCache(new InMemoryMovieRepository, normalizer = titleNormalizer)

  /** A concluded row for one of the two films, carrying TMDB's own runtime/year. */
  private def resolved(tmdbId: Int, year: Int, runtime: Int, cinemas: Seq[Cinema]): MovieRecord =
    MovieRecord(
      tmdbId = Some(tmdbId),
      data   = (cinemas.map(c => (c: Source) -> SourceData(
                  title = Some(Title), releaseYear = Some(year), runtimeMinutes = Some(runtime),
                  showtimes = Seq(Showtime(When, bookingUrl = None)))) :+
                ((models.Tmdb: Source) -> SourceData(
                  title = Some(Title), releaseYear = Some(year), runtimeMinutes = Some(runtime)))).toMap)

  /** What every one of those 31 cinemas actually publishes: a title, a runtime,
   *  and NO year. */
  private def yearlessScrape(cinema: Cinema, runtime: Int): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title = Title, releaseYear = None, runtimeMinutes = Some(runtime)),
      cinema    = cinema,
      posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
      showtimes = Seq(Showtime(When, bookingUrl = None)))

  private def cinemasOn(cache: MovieCache, year: Int): Set[Cinema] =
    cache.get(cache.keyOf(Title, Some(year))).map(_.cinemaData.keySet).getOrElse(Set.empty)

  "a yearless listing of a title two different films share" should
    "go to the film whose runtime it matches, not to whichever is older" in {
    val c = cache()
    c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Seq(Helios)))
    c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))

    // Multikino lists the 2026 romcom at 105 minutes with no year — the prod payload,
    // and 3 minutes off TMDB's 102, so nothing "agrees" here; nearness decides.
    c.recordCinemaScrape(Multikino, Seq(yearlessScrape(Multikino, 105)))

    cinemasOn(c, 2026) should contain(Multikino)
    cinemasOn(c, 1961) should not contain Multikino
  }

  // The exact prod path. Multikino's LISTING ships `runtimeMinutes = 0` — the real
  // figure arrives from its detail page and is recorded on the slot — so the tick
  // that has to choose a film carries no minutes of its own. Prod had Multikino's
  // slot (105 min) sitting on the 1961 row; every tick re-confirmed it there.
  it should "use the runtime already on this venue's slot when the listing ships none" in {
    val c = cache()
    c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Nil))
    c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))
    // Multikino's slot, stranded on the 1961 row, carrying the 105 minutes its detail
    // page reported.
    c.putIfPresent(c.keyOf(Title, Some(1961)), r => r.copy(data = r.data + ((Multikino: Source) ->
      SourceData(title = Some(Title), runtimeMinutes = Some(105),
        showtimes = Seq(Showtime(When, bookingUrl = None))))))

    // The listing tick itself knows nothing: no year, no minutes.
    c.recordCinemaScrape(Multikino, Seq(
      yearlessScrape(Multikino, 105).copy(movie = Movie(title = Title, releaseYear = None, runtimeMinutes = Some(0)))))

    cinemasOn(c, 2026) should contain(Multikino)
    cinemasOn(c, 1961) should not contain Multikino
  }

  // The tail the runtime rule cannot reach. 22 of this film's prod slots sat ALONE on the
  // 1961 row with no runtime published anywhere — not on the listing, not on the slot. If
  // such a venue simply stays where it happens to sit, nothing will ever move it and the
  // duplicate card outlives every scrape. The venue counts are the remaining evidence: a
  // title shared by a current release and an old picture is the release when a venue says
  // nothing else.
  it should "move a runtime-less venue off a film almost nobody is screening" in {
    val c = cache()
    c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Seq(Helios, models.CinemaCityKinepolis)))
    // Multikino sits alone on the 1961 row and publishes no minutes at all.
    c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))
    c.putIfPresent(c.keyOf(Title, Some(1961)), r => r.copy(data = r.data + ((Multikino: Source) ->
      SourceData(title = Some(Title), showtimes = Seq(Showtime(When, bookingUrl = None))))))

    c.recordCinemaScrape(Multikino, Seq(
      yearlessScrape(Multikino, 105).copy(movie = Movie(title = Title, releaseYear = None, runtimeMinutes = None))))

    cinemasOn(c, 2026) should contain(Multikino)
    cinemasOn(c, 1961) should not contain Multikino
  }

  it should "still reach the older film when the runtime is the OLDER one's" in {
    val c = cache()
    c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Seq(Helios)))
    c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))

    // A repertory house screening Antonioni prints 121 minutes and no year.
    c.recordCinemaScrape(Multikino, Seq(yearlessScrape(Multikino, 121)))

    cinemasOn(c, 1961) should contain(Multikino)
    cinemasOn(c, 2026) should not contain Multikino
  }

  // The user-visible symptom, end to end: one venue prints the year, the rest
  // don't. Both listings are the same film and must reach the same row — else the
  // read model projects a card per row and `/poznan` shows the film twice under
  // one slug, each card holding the same booking links.
  "listings of one film that disagree about whether to print a year" should
    "all land on that one film's row, not split across a same-titled older one" in {
    val c = cache()
    c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Nil))
    c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))

    // Kino Spójnia is the one venue in the corpus that publishes the year …
    c.recordCinemaScrape(Helios, Seq(
      yearlessScrape(Helios, 105).copy(movie = Movie(title = Title, releaseYear = Some(2026), runtimeMinutes = Some(105)))))
    // … Multikino, like the other 31, does not.
    c.recordCinemaScrape(Multikino, Seq(yearlessScrape(Multikino, 105)))

    cinemasOn(c, 2026) should contain allOf (Helios, Multikino)
    cinemasOn(c, 1961) shouldBe empty
  }

  // The prod shape this class of bug ACTUALLY survives on. Everything above is a
  // healthy tick, where the end-of-tick prune sweeps the venue's slot off the row
  // it left. But that prune stands down on a degraded tick (`listingIsComplete =
  // false` — a chunked scrape reduced from some of its date-chunks), because
  // deleting slots a broken fetch merely failed to list is what flickers a
  // still-playing film off the site. With the prune the ONLY remover, the write
  // was a COPY: the venue's showtimes went on the new row and stayed on the old
  // one too. That is `/poznan/movie/zaproszenie` on 2026-09-04 — Cinema City
  // Kinepolis 21:40 and Kino Malta 20:35 rendered under BOTH the 2026 and the
  // 1986 film. Moving the slot at the write is safe on a degraded tick precisely
  // because it removes a slot only where we just OBSERVED the venue listing it.
  it should "move the venue off the other film even when the tick is too short to prune" in {
    val c = cache()
    c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Seq(Helios)))
    c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))
    // Multikino's slot stranded on the 1961 row, with its showtimes.
    c.putIfPresent(c.keyOf(Title, Some(1961)), r => r.copy(data = r.data + ((Multikino: Source) ->
      SourceData(title = Some(Title), runtimeMinutes = Some(105),
        showtimes = Seq(Showtime(When, bookingUrl = None))))))

    c.recordCinemaScrape(Multikino, Seq(yearlessScrape(Multikino, 105)), listingIsComplete = false)

    cinemasOn(c, 2026) should contain(Multikino)
    withClue("the venue's slot must MOVE, not be copied onto a second film: ") {
      cinemasOn(c, 1961) should not contain Multikino
    }
  }

  // ── Lalka, prod 2026-09-04: the new film living on the old film's row ───────
  //
  // `lalka|1968` (Wojciech Has, TMDB 152 min) held 121 slots, all but ONE of them
  // the 2026 Maciej Kawalski film — Multikino's 32 slots carrying its synopsis,
  // Kino Pod Baranami / Iluzjon / Nowe Horyzonty publishing year 2026 and 162
  // minutes. `/poznan/movie/lalka-1968` served the new film under the old one's
  // year and ratings.
  //
  // What kept it there is that TMDB publishes NO runtime for the unreleased 2026
  // film. `strictNearest` only compares candidates that carry a runtime of their
  // own, so the 1968 row was the sole entrant and won by walkover — 10 minutes
  // away from the venue's published 162, which the 2026 film matches exactly.
  // IMDb HAS that runtime (162); reading only the Tmdb slot threw it away.
  private val Lalka    = "Lalka"
  private val HasFilm  = 81315    // Wojciech Has, 1968 — TMDB runtime 152
  private val NewLalka = 1321666  // Maciej Kawalski, 2026 — TMDB runtime ABSENT, IMDb 162

  /** A concluded row whose own runtime is known to some resolvers but not others —
   *  the shape TMDB leaves behind for a film it has not dated yet. */
  private def resolvedRuntimes(tmdbId: Int, year: Int, tmdbRuntime: Option[Int], imdbRuntime: Option[Int]): MovieRecord =
    MovieRecord(
      tmdbId = Some(tmdbId),
      data   = Map[Source, SourceData](
        models.Tmdb -> SourceData(title = Some(Lalka), releaseYear = Some(year), runtimeMinutes = tmdbRuntime),
        models.Imdb -> SourceData(title = Some(Lalka), releaseYear = Some(year), runtimeMinutes = imdbRuntime)))

  private def lalkaCinemas(c: MovieCache, year: Int): Set[Cinema] =
    c.get(c.keyOf(Lalka, Some(year))).map(_.cinemaData.keySet).getOrElse(Set.empty)

  "a candidate film TMDB gives no runtime" should "not lose the runtime comparison by walkover" in {
    val c = cache()
    c.put(c.keyOf(Lalka, Some(1968)), resolvedRuntimes(HasFilm, 1968, tmdbRuntime = Some(152), imdbRuntime = Some(153)))
    c.put(c.keyOf(Lalka, Some(2026)), resolvedRuntimes(NewLalka, 2026, tmdbRuntime = None, imdbRuntime = Some(162)))

    // Kino Pod Baranami lists "Lalka" at 162 minutes — the new film exactly, and
    // ten minutes off Has's. Its listing ships no year (the year arrives later,
    // on the deferred detail pass).
    c.recordCinemaScrape(Multikino, Seq(
      CinemaMovie(
        movie     = Movie(title = Lalka, releaseYear = None, runtimeMinutes = Some(162)),
        cinema    = Multikino,
        posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
        showtimes = Seq(Showtime(When, bookingUrl = None)))))

    lalkaCinemas(c, 2026) should contain(Multikino)
    withClue("the venue's 162 minutes match the 2026 film exactly: ") {
      lalkaCinemas(c, 1968) should not contain Multikino
    }
  }

  // The second, independent reason those venues stayed put: their year never got a
  // vote. Placement narrows candidates by the LISTING's year, and every one of
  // these venues is a deferred-detail client (`IluzjonClient`, `NoweHoryzontyClient`,
  // `KinoPodBaranamiClient`, `PionierClient` all read `releaseYear` off the detail
  // page), so the listing tick carries none. The 2026 we already hold on that
  // venue's own slot is ignored — even though the identical fallback exists one
  // line away for runtime ("Multikino sends 0 and its detail page fills the runtime
  // in a beat later"). So every tick re-derives the same yearless placement and the
  // split never heals.
  "a yearless listing from a venue whose own slot records the year" should
    "be placed as though the listing carried it" in {
    val c = cache()
    c.put(c.keyOf(Lalka, Some(1968)), resolvedRuntimes(HasFilm, 1968, tmdbRuntime = Some(152), imdbRuntime = Some(153)))
    c.put(c.keyOf(Lalka, Some(2026)), resolvedRuntimes(NewLalka, 2026, tmdbRuntime = None, imdbRuntime = None))
    // The venue sits on the 1968 row, but its own slot already records 2026 —
    // written by the detail pass after the placement was decided.
    c.putIfPresent(c.keyOf(Lalka, Some(1968)), r => r.copy(data = r.data + ((Multikino: Source) ->
      SourceData(title = Some(Lalka), releaseYear = Some(2026),
        showtimes = Seq(Showtime(When, bookingUrl = None))))))

    // Neither the listing nor either film offers a runtime to disambiguate with.
    c.recordCinemaScrape(Multikino, Seq(
      CinemaMovie(
        movie     = Movie(title = Lalka, releaseYear = None, runtimeMinutes = None),
        cinema    = Multikino,
        posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
        showtimes = Seq(Showtime(When, bookingUrl = None)))))

    lalkaCinemas(c, 2026) should contain(Multikino)
    withClue("the year on this venue's own slot must route it: ") {
      lalkaCinemas(c, 1968) should not contain Multikino
    }
  }

  // A chain's shared detail slot is written once per NETWORK (`CinemaCityChain`,
  // see `Cinema.chainDetailVenues`) and, by design, is never scraped and never
  // pruned — `recordCinemaScrape` only prunes the scraping cinema's own slot. So
  // when a same-title split moves the last Cinema City venue off a row, the chain
  // slot is stranded there and keeps describing a film the row no longer holds.
  // Prod: `zaproszenie|1986` — Wanda Jakubowska's war drama — rendered
  // "Reżyseria: Olivia Wilde" off exactly such an orphan.
  "a chain's shared detail slot" should "not outlive the last venue of its chain on a row" in {
    val c = cache()
    c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Nil))
    c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))
    // One Cinema City venue sitting on the old film's row (its own slot says
    // nothing about the year), plus the network detail for the NEW film written
    // onto that same row.
    c.putIfPresent(c.keyOf(Title, Some(1961)), r => r.copy(data = r.data
      + ((models.CinemaCityKinepolis: Source) -> SourceData(title = Some(Title),
          showtimes = Seq(Showtime(When, bookingUrl = None))))
      + ((models.CinemaCityChain: Source) -> SourceData(title = Some(Title),
          director = Seq("Wrong Director"), synopsis = Some("the other film")))))

    // Cinema City's venue lists the film at the new one's runtime, so it moves.
    c.recordCinemaScrape(models.CinemaCityKinepolis, Seq(yearlessScrape(models.CinemaCityKinepolis, 102)))

    cinemasOn(c, 2026) should contain(models.CinemaCityKinepolis)
    withClue("no Cinema City venue is left on the 1961 row, so its chain detail is an orphan: ") {
      c.get(c.keyOf(Title, Some(1961))).map(_.data.keySet).getOrElse(Set.empty) should
        not contain (models.CinemaCityChain: Source)
    }
  }

  it should "stay while any venue of its chain is still on the row" in {
    val c = cache()
    c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Nil))
    c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))
    c.putIfPresent(c.keyOf(Title, Some(1961)), r => r.copy(data = r.data
      + ((models.CinemaCityKinepolis: Source)   -> SourceData(title = Some(Title),
          showtimes = Seq(Showtime(When, bookingUrl = None))))
      + ((models.CinemaCityPoznanPlaza: Source) -> SourceData(title = Some(Title),
          showtimes = Seq(Showtime(When, bookingUrl = None))))
      + ((models.CinemaCityChain: Source)       -> SourceData(title = Some(Title),
          director = Seq("Right Director")))))

    // Only ONE of the two Cinema City venues moves.
    c.recordCinemaScrape(models.CinemaCityKinepolis, Seq(yearlessScrape(models.CinemaCityKinepolis, 102)))

    c.get(c.keyOf(Title, Some(1961))).map(_.data.keySet).getOrElse(Set.empty) should
      contain (models.CinemaCityChain: Source)
  }

  // Convergence: whatever order the venues arrive in, and however many settles
  // run, the film ends up on ONE row with ONE card. This is the property the
  // duplicate broke — the corpus never reached a fixpoint, it just kept both.
  "the corpus" should "settle to a single row for the film, whatever the arrival order" in {
    val venues = Seq(Multikino, Helios, models.CinemaCityKinepolis, models.CinemaCityPoznanPlaza)
    venues.permutations.take(6).foreach { order =>
      val c = cache()
      c.put(c.keyOf(Title, Some(2026)), resolved(NewFilm, 2026, 102, Nil))
      c.put(c.keyOf(Title, Some(1961)), resolved(OldFilm, 1961, 121, Nil))
      order.foreach(v => c.recordCinemaScrape(v, Seq(yearlessScrape(v, 105))))
      c.canonicalizeBySanitize()

      withClue(s"arrival order ${order.map(_.displayName).mkString(", ")}: ") {
        // Every venue on the film they are actually screening …
        cinemasOn(c, 2026) should contain allElementsOf venues
        // … and none of them stranded on the same-titled 1961 film, which is what
        // put a second card under the same slug on the live page.
        cinemasOn(c, 1961) shouldBe empty
      }
    }
  }

  // ── It, kinowo_us 2026-09-07: the year is IN the title, and the key drops it ──
  //
  // Twenty-one Cinepolis / EVO venues list "It (2017)" at 135 minutes (Andy
  // Muschietti); five — Bright Star and four Rooftop Cinemas — list "It (1990)" at
  // 168 (Tommy Lee Wallace's miniseries). Neither side publishes a `releaseYear`
  // and neither publishes an `originalTitle`: the Flicks listings carry a title and
  // nothing else, and the only place the year appears is INSIDE it.
  //
  // `sanitize` strips a bracketed year (`xtra-canonical-trailing-paren-year`, so
  // "Milczenie owiec (1991)" merges into "Milczenie owiec"), which is right for a
  // merge KEY and wrong as the whole of a placement decision: both spellings reduce
  // to "it", the listing reaches `concludedKeyFor` carrying no year at all, and a
  // yearless listing lands on any concluded same-title row — here the only one
  // there is, the 2017 film. The row then held two films, `MixedFilmSplitter`
  // re-diverted the five slots on every settle, the staging fold put them straight
  // back, and the United States convergence leg counted five splits per pass for
  // ever.
  //
  // `EmbeddedYear` already reads exactly this annotation, and its own header says
  // it is the row's lookup year when the scrape carries none. The landing simply
  // never asked it.
  // tmdbId 570670 (Muschietti, 2017) against 33217 (Wallace, 1990) — two films.
  private val It2017 = 570670

  /** The 2017 film's resolved row, as the Cinepolis venues describe it. */
  private def resolvedIt2017 = MovieRecord(tmdbId = Some(It2017), data = Map[Source, SourceData](
    models.Tmdb      -> SourceData(title = Some("It"), releaseYear = Some(2017), runtimeMinutes = Some(135)),
    (Helios: Source) -> SourceData(title = Some("It (2017)"), runtimeMinutes = Some(135),
      director = Seq("Andy Muschietti"), showtimes = Seq(Showtime(When, bookingUrl = None)))))

  /** A Flicks listing: a title, a runtime, a director — and no year of its own. */
  private def flicksListing(cinema: Cinema, title: String, runtime: Int, director: String): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title = title, releaseYear = None, runtimeMinutes = Some(runtime)),
      cinema    = cinema,
      posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Seq(director),
      showtimes = Seq(Showtime(When, bookingUrl = None)))

  private def itCinemas(c: MovieCache, year: Int): Set[Cinema] =
    c.get(c.keyOf("It", Some(year))).map(_.cinemaData.keySet).getOrElse(Set.empty)

  "a listing that prints its release year inside the title" should
    "land on the film of THAT year, not on the same-titled film already resolved" in {
    val c = cache()
    withClue("the premise — the canonical key drops the bracketed year, so both spellings collide: ") {
      titleNormalizer.sanitize("It (1990)") shouldBe titleNormalizer.sanitize("It (2017)")
    }
    c.put(c.keyOf("It", Some(2017)), resolvedIt2017)

    c.recordCinemaScrape(Multikino, Seq(flicksListing(Multikino, "It (1990)", 168, "Tommy Lee Wallace")))

    withClue(s"cinemas on the 2017 row: ${itCinemas(c, 2017).map(_.displayName)}: ") {
      itCinemas(c, 2017) should not contain Multikino
    }
    itCinemas(c, 1990) should contain(Multikino)
  }

  // The convergence axis itself. The split is a CLEANUP — the row is only mixed
  // because the landing put a second film on it — so with the landing right there
  // is nothing for a settle to split, and the count that ran away stays at zero.
  it should "leave the resolved row describing one film, so no settle has to split it" in {
    val c = cache()
    c.put(c.keyOf("It", Some(2017)), resolvedIt2017)

    c.recordCinemaScrape(Multikino, Seq(flicksListing(Multikino, "It (1990)", 168, "Tommy Lee Wallace")))

    val strays = c.get(c.keyOf("It", Some(2017)))
      .map(r => MixedFilmDetector.strays(r, c.normalizer)).getOrElse(Nil)
    withClue(s"the 2017 row still holds a second film's slots: ${strays.map(_._1)}: ") {
      strays shouldBe empty
    }
  }

  // The other direction, which must not move: a venue that prints the year of the
  // film the row already IS keeps landing on it, so this cannot spawn a duplicate
  // row for every venue that decorates its title.
  it should "still land the venues that print the resolved row's own year on it" in {
    val c = cache()
    c.put(c.keyOf("It", Some(2017)), resolvedIt2017)

    c.recordCinemaScrape(Multikino, Seq(flicksListing(Multikino, "It (2017)", 135, "Andy Muschietti")))

    itCinemas(c, 2017) should contain(Multikino)
    itCinemas(c, 1990) shouldBe empty
  }

  // ── Through STAGING, where neither film exists yet ──────────────────────────
  //
  // Everything above is a listing landing on a row that already exists. On a corpus
  // holding NEITHER film — a fresh convergence replay, and every US venue's first tick —
  // both listings are newcomers and go to `pending_movies`, which the landing files under
  // the year the VENUE published: none, for the venues that print it only inside a title.
  // The fold reads the titles instead (`FilmCanonicalizer.separateByAssertedYear`), so
  // nothing about how a row is KEYED had to change to keep the two films apart.

  "two films that share a title and print their year only inside it" should
    "incubate as staging rows and fold to a row each" in {
    val repository = new InMemoryMovieRepository()
    val staging    = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val c = new CaffeineMovieCache(repository, new InProcessEventBus,
      staging = Some(staging), normalizer = titleNormalizer)

    // Neither film is in `movies`, so both venues' listings divert to staging.
    c.recordCinemaScrape(Helios,    Seq(flicksListing(Helios,    "It (2017)", 135, "Andy Muschietti")))
    c.recordCinemaScrape(Multikino, Seq(flicksListing(Multikino, "It (1990)", 168, "Tommy Lee Wallace")))

    new InMemoryStagingFolder(staging, repository, titleNormalizer).foldGroup("It (1990)")

    val folded = repository.findAll()
    withClue(s"the fold made one film of both: ${folded.map(r => (r.title, r.year, r.record.cinemaSlots.map(_._1.displayName)))}: ") {
      folded.map(_.year).toSet shouldBe Set(Some(1990), Some(2017))
    }
    folded.flatMap(r => MixedFilmDetector.strays(r.record, titleNormalizer)) shouldBe empty
  }

  it should "not have the slots a settle splits off folded straight back onto the row they left" in {
    val repository = new InMemoryMovieRepository()
    val staging    = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val c          = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    // The prod row: resolved to Muschietti's film and screened by a majority of venues,
    // with one screening Wallace's merged onto it — the state a settle is there to
    // repair. Two venues on the 2017 side because that is what makes the 1990 venue the
    // STRAY: `MixedFilmDetector` reads the row's cinemas alone (never its resolution), so
    // one venue each way is a tie either side of which is an equally correct split.
    // kinowo_us ran 21 venues against 5.
    c.put(c.keyOf("It", Some(2017)), resolvedIt2017.copy(
      data = resolvedIt2017.data
        + ((models.CinemaCityKinepolis: Source) -> SourceData(
            title = Some("It (2017)"), runtimeMinutes = Some(135), director = Seq("Andy Muschietti"),
            showtimes = Seq(Showtime(When, bookingUrl = None))))
        + ((Multikino: Source) -> SourceData(
            title = Some("It (1990)"), runtimeMinutes = Some(168), director = Seq("Tommy Lee Wallace"),
            showtimes = Seq(Showtime(When, bookingUrl = None))))))
    val splitter = new MixedFilmSplitter(c, staging)

    splitter.splitMixedRows() shouldBe 1
    new InMemoryStagingFolder(staging, repository, titleNormalizer).foldGroup("It (1990)")
    c.rehydrate()

    withClue("the fold handed the split slot back to the film it was just taken off: ") {
      itCinemas(c, 2017) should not contain Multikino
    }
    itCinemas(c, 1990) should contain(Multikino)
    withClue("a second settle re-splitting the same slot is the loop itself: ") {
      splitter.splitMixedRows() shouldBe 0
    }
  }

  // ── THE REGRESSION GUARD ────────────────────────────────────────────────────
  //
  // The year is printed by SOME venues and not others, so it must never decide where a
  // newcomer is FILED. `1a1c62e29` made it decide, and a film listed as "Titanic (1997)"
  // at one cinema and "Titanic" at the next incubated as two staging rows under two keys,
  // resolving apart — 5,235 of 31,772 Polish screenings never reached the read model.
  "a film only SOME of whose venues print its year" should
    "stage under one key and fold to one row carrying every venue's showtimes" in {
    val repository = new InMemoryMovieRepository()
    val staging    = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val c = new CaffeineMovieCache(repository, new InProcessEventBus,
      staging = Some(staging), normalizer = titleNormalizer)

    val printing = Seq(Helios, Multikino, KinoMuza)
    val silent   = Seq(KinoApollo, KinoPalacowe, CharlieMonroe)
    printing.foreach(v => c.recordCinemaScrape(v, Seq(flicksListing(v, "Titanic (1997)", 194, "James Cameron"))))
    silent.foreach(v   => c.recordCinemaScrape(v, Seq(flicksListing(v, "Titanic",        194, "James Cameron"))))

    withClue(s"the venues incubated apart: ${staging.findAll().map(r => (r.cinema.displayName, r.title, r.year))}: ") {
      staging.findAll().map(r => (titleNormalizer.sanitize(r.title), r.year)).distinct should have size 1
    }

    new InMemoryStagingFolder(staging, repository, titleNormalizer).foldGroup("Titanic")

    val folded = repository.findAll()
    withClue(s"folded to ${folded.map(r => (r.title, r.year, r.record.cinemaSlots.size))}: ") {
      folded should have size 1
    }
    // A slot is keyed per SHOWN title (`CinemaShowing`), not by the bare cinema.
    folded.head.record.cinemaSlots.flatMap(s => Source.cinemaOf(s._1)).toSet shouldBe (printing ++ silent).toSet
    withClue("a venue's showtimes were dropped on the way through staging: ") {
      folded.head.record.cinemaSlots.count(_._2.showtimes.nonEmpty) shouldBe 6
    }
    staging.findAll() shouldBe empty
  }
}
