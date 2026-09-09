package services.movies

import clients.TmdbClient
import models.{Helios, KinoMuranow, Multikino, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.staging.InMemoryStagingRepository
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime

/**
 * Two unrelated films under one Polish title share a `movies` row, and no tmdbId
 * can serve both. `clusterByFilm` can't help: it splits ROWS by tmdbId, and a row
 * holding both films has one.
 *
 * The splitter undoes the merge rather than inventing a second way to make a
 * record — the stray cinema's slot goes back to `pending_movies` exactly as a
 * newcomer's scrape diverts it, and the ordinary staging path resolves and folds
 * it into its own row from there.
 */
class MixedFilmSplitterSpec extends AnyFlatSpec with Matchers {

  private def slot(title: String, director: Seq[String], original: Option[String], year: Option[Int], runtime: Option[Int]) =
    SourceData(title = Some(title), director = director, originalTitle = original,
               releaseYear = year, runtimeMinutes = runtime)

  private def fixture(record: MovieRecord, title: String, year: Option[Int]) = {
    val repository = new InMemoryMovieRepository()
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val staging    = new InMemoryStagingRepository(normalizer = titleNormalizer)
    cache.put(cache.keyOf(title, year), record)
    (cache, staging, new MixedFilmSplitter(cache, staging))
  }

  "the row holding both Joan of Arc films" should "send the stray cinema back to staging" in {
    // Production: Muranów screens Besson's 1999 film, Nowe Horyzonty Pálmason's 2025 one.
    val record = MovieRecord(data = Map[Source, SourceData](
      KinoMuranow -> slot("Joanna d'Arc", Seq("Luc Besson"), Some("Joan of Arc"), Some(1999), Some(160)),
      Helios      -> slot("Joanna d'Arc", Seq.empty, Some("Jóhanna af Örk"), Some(2025), None)))
    val (cache, staging, splitter) = fixture(record, "Joanna d'Arc", Some(2025))

    splitter.splitMixedRows() shouldBe 1

    // The row keeps ONE film's cinemas…
    val remaining = cache.get(cache.keyOf("Joanna d'Arc", Some(2025))).getOrElse(fail("row vanished"))
    remaining.cinemaSlots should have size 1
    // …and the other is back in staging with its OWN identity, so it resolves on
    // what ITS cinema published rather than on the row it was merged into.
    //
    // Which of the two is the stray is deliberately not asserted: both groups hold
    // one slot, so the choice is a tie broken deterministically and either side is
    // equally correct. What must hold is that they are SEPARATED and the staged one
    // keeps its own title and year.
    val staged = staging.findAll()
    staged should have size 1
    val strayed    = staged.head.record.cinemaSlots.head._2
    val keptOnRow  = remaining.cinemaSlots.head._2
    strayed.originalTitle should not be keptOnRow.originalTitle
    strayed.releaseYear   should not be keptOnRow.releaseYear
    Set(strayed.originalTitle, keptOnRow.originalTitle) shouldBe
      Set(Some("Joan of Arc"), Some("Jóhanna af Örk"))
  }

  "a row where one cinema of many screens a different film" should "move only that one" in {
    // Production "Obcy": the majority screen Ozon's L'étranger; one screens
    // Brandt Andersen's film. The majority must be left completely undisturbed.
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino   -> slot("Obcy", Seq("François Ozon"), Some("L'étranger"), Some(2025), Some(120)),
      Helios      -> slot("Obcy", Seq("François Ozon"), Some("L’Étranger"), Some(2025), Some(122)),
      KinoMuranow -> slot("Obcy", Seq("Brandt Andersen"), Some("I Was A Stranger"), Some(2024), Some(103))))
    val (cache, staging, splitter) = fixture(record, "Obcy", Some(2025))

    splitter.splitMixedRows() shouldBe 1

    val remaining = cache.get(cache.keyOf("Obcy", Some(2025))).getOrElse(fail("row vanished"))
    remaining.cinemaSlots.map(_._1).toSet shouldBe Set[Source](Multikino, Helios)
    staging.findAll().head.record.cinemaSlots.head._2.director shouldBe Seq("Brandt Andersen")
  }

  "the split count" should "reach the metrics sink, and read zero on the idempotent second pass" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      KinoMuranow -> slot("Joanna d'Arc", Seq("Luc Besson"), Some("Joan of Arc"), Some(1999), Some(160)),
      Helios      -> slot("Joanna d'Arc", Seq.empty, Some("Jóhanna af Örk"), Some(2025), None)))
    val (cache, staging, _) = fixture(record, "Joanna d'Arc", Some(2025))
    val recorded = scala.collection.mutable.ArrayBuffer.empty[Int]
    val splitter = new MixedFilmSplitter(cache, staging, splitMetrics = recorded += _)

    splitter.splitMixedRows() shouldBe 1
    splitter.splitMixedRows() shouldBe 0

    recorded.toSeq shouldBe Seq(1, 0)
  }

  "a row describing ONE film" should "be left alone" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot("Dreams", Seq("Michel Franco"), Some("Dreams: Sueños"), Some(2025), Some(98)),
      Helios    -> slot("Dreams", Seq.empty, None, None, None)))
    val (cache, staging, splitter) = fixture(record, "Dreams", Some(2025))

    splitter.splitMixedRows() shouldBe 0
    cache.get(cache.keyOf("Dreams", Some(2025))).map(_.cinemaSlots.size) shouldBe Some(2)
    staging.findAll() shouldBe empty
  }

  /** The split runs inside `MovieService.settle`, and the convergence suite's first
   *  claim is that a settle CONVERGES — "a further settle changes no key, moves no
   *  film's cinemas, folds no row and writes nothing". That claim is what a splitter
   *  most needs policing it, since a split the next pass undoes would churn forever.
   *
   *  Neither fixture corpus actually holds a mixed row, though — the only split that
   *  ever fired in one was a false positive, since fixed — so the convergence legs
   *  never exercise this. Hence this: a genuinely mixed row, put through the real
   *  `settle`, twice. */
  "settle" should "split a mixed row, and change nothing on a second pass" in {
    val repository = new InMemoryMovieRepository()
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val staging    = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val service    = new MovieService(cache, new InProcessEventBus(),
      new TmdbClient(http = new tools.GetOnlyHttpFetch {
        override def get(url: String): String = """{"results":[]}"""
      }, apiKey = Some("stub")),
      staging = staging)

    cache.put(cache.keyOf("Joanna d'Arc", Some(2025)), MovieRecord(data = Map[Source, SourceData](
      KinoMuranow -> slot("Joanna d'Arc", Seq("Luc Besson"), Some("Joan of Arc"), Some(1999), Some(160)),
      Helios      -> slot("Joanna d'Arc", Seq.empty, Some("Jóhanna af Örk"), Some(2025), None))))

    service.settle()
    val afterFirst  = cache.get(cache.keyOf("Joanna d'Arc", Some(2025))).map(_.cinemaSlots.size)
    val stagedFirst = staging.findAll().size
    afterFirst  shouldBe Some(1)   // the settle really did split it…
    stagedFirst shouldBe 1

    service.settle()
    // …and the second settle is a no-op: same row, same staging, nothing moved.
    cache.get(cache.keyOf("Joanna d'Arc", Some(2025))).map(_.cinemaSlots.size) shouldBe afterFirst
    staging.findAll().size shouldBe stagedFirst
    service.stop()
  }

  /** The split must not cost the stray cinema its SHOWTIMES.
   *
   *  Every fixture above wires a bare `InMemoryMovieRepository`, so its records keep
   *  their showtimes inline and a slot carried to staging carries them for free. That
   *  is not production's shape: with `screenings` wired the cache is read-split, so
   *  `MovieCache.persist` puts each record through `ShowtimesDigest.stripForCache`
   *  and every CACHE-RESIDENT slot holds `showtimes = Nil` — the lists live in
   *  `screenings`, keyed by film id.
   *
   *  `splitMixedRows` walks `cache.snapshot()`, which is exactly that stripped view,
   *  and re-stages the slot verbatim. So in production the stray reaches
   *  `pending_movies` with no showtimes at all, folds into a row that has none, and
   *  the film is off the site until that cinema is scraped again — at which point the
   *  listing re-attaches to the original row, making it mixed for the next settle to
   *  split. Measured on prod 2026-08-06: `ktoscalkiemobcy|2024` folded out of `Obcy`
   *  with 0 slots and 0 screenings, on a 30-minute cycle.
   *
   *  The read-split is what makes this reachable, so the fixture has to model it —
   *  see `InMemoryMovieRepository`'s own note on the bug class an inline-showtimes
   *  fake cannot express. */
  "the split" should "carry the stray cinema's showtimes to staging, not just its slot" in {
    val screenings = new InMemoryScreeningsRepository
    val repository = new InMemoryMovieRepository(
      screenings = Some(screenings), slots = Some(new InMemorySlotsRepository), normalizer = titleNormalizer)
    val cache   = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val staging = new InMemoryStagingRepository(normalizer = titleNormalizer)

    def showtime(day: Int, hour: Int) =
      Showtime(LocalDateTime.of(2026, 8, day, hour, 0), bookingUrl = None)
    def screened(base: SourceData, times: Seq[Showtime]) = base.copy(showtimes = times.toList)

    val strayTimes = Seq(showtime(8, 20), showtime(9, 17))
    cache.put(cache.keyOf("Obcy", Some(2025)), MovieRecord(data = Map[Source, SourceData](
      Multikino   -> screened(slot("Obcy", Seq("François Ozon"), Some("L'étranger"), Some(2025), Some(120)),
                              Seq(showtime(8, 18))),
      Helios      -> screened(slot("Obcy", Seq("François Ozon"), Some("L’Étranger"), Some(2025), Some(122)),
                              Seq(showtime(8, 21))),
      KinoMuranow -> screened(slot("Obcy", Seq("Brandt Andersen"), Some("I Was A Stranger"), Some(2024), Some(103)),
                              strayTimes))))

    // The row really is read-split: its cache-resident slots hold no showtime lists.
    cache.get(cache.keyOf("Obcy", Some(2025))).toSeq
      .flatMap(_.cinemaSlots).flatMap(_._2.showtimes) shouldBe empty

    new MixedFilmSplitter(cache, staging).splitMixedRows() shouldBe 1

    val strayed = staging.findAll().head.record.cinemaSlots.head._2
    strayed.director shouldBe Seq("Brandt Andersen")
    withClue("the stray reached staging with no showtimes, so the film has none until " +
             "its cinema is scraped again — and that re-scrape re-mixes the row it just left: ") {
      strayed.showtimes should contain theSameElementsAs strayTimes
    }
  }

  /** ABSENT is not EMPTY, and the difference is a film's board.
   *
   *  `restitchedChecked` reports `readOk = true` whenever the read COMPLETED — including
   *  when it completed and found nothing. That happens because the id is re-derived from the
   *  resident row's DISPLAY title, which can drift from the persisted `_id` (see
   *  `StoredMovieRecord.idOf`, and `MovieCache.rehydrate`, which migrates rows it catches).
   *  Treating that as "this film has no stored cinemas" makes the split fall back to the
   *  CACHE-RESIDENT slot, and under the read-split that slot holds no showtimes — so the
   *  stray is staged with an empty board, which is the `ktoscalkiemobcy|2024` defect reached
   *  through the absent branch rather than the failed one. */
  it should "defer the split when the stored row cannot be found, rather than stage an empty board" in {
    val repository = new InMemoryMovieRepository(
      screenings = Some(new InMemoryScreeningsRepository), slots = Some(new InMemorySlotsRepository),
      normalizer = titleNormalizer) {
      // The read COMPLETES and finds nothing — not a failure, so `readOk` stays true.
      override def findByIdChecked(id: FilmId): (Option[StoredMovieRecord], Boolean)   = (None, true)
      override def findByKeyChecked(key: CacheKey): (Option[StoredMovieRecord], Boolean) = (None, true)
    }
    val cache   = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val staging = new InMemoryStagingRepository(normalizer = titleNormalizer)

    cache.put(cache.keyOf("Obcy", Some(2025)), MovieRecord(data = Map[Source, SourceData](
      Multikino   -> slot("Obcy", Seq("François Ozon"), Some("L'étranger"), Some(2025), Some(120)),
      KinoMuranow -> slot("Obcy", Seq("Brandt Andersen"), Some("I Was A Stranger"), Some(2024), Some(103)))))

    withClue("the row is mixed and the splitter staged the stray anyway, off a resident slot " +
             "that carries no showtimes: ")(
      new MixedFilmSplitter(cache, staging).splitMixedRows() shouldBe 0)
    withClue("nothing may reach staging until the row can be read back: ")(
      staging.findAll() shouldBe empty)
  }

  /** Production `kinowo_us`: five Rooftop Cinema / Bright Star venues screen Tommy
   *  Lee Wallace's 1990 "It" miniseries as a retro event, writing the year straight
   *  into the title ("It (1990)") rather than into their own `releaseYear` field —
   *  so the stray slot reaches the splitter YEARLESS. Staged yearless, it lands in
   *  `StagingFold.planGroup`'s raw `(sanitize, year)` bucket for "it" with NO year at
   *  all, the same bucket the mainstream "It" (2017) row's OWN key normalizes to —
   *  `FilmCanonicalizer.clusterByFilm`'s yearless-and-idless rule then folds it
   *  straight back onto the 2017 cluster by title alone, so every settle re-diverts
   *  the same five venues and the fold puts them straight back: never converges.
   *
   *  `ScrapeLanding`'s ordinary landing path already carries the fix for exactly
   *  this shape — `cm.movie.releaseYear.orElse(EmbeddedYear.of(displayTitle))` — the
   *  splitter's re-diversion path just never got it. This pins that the splitter
   *  now elevates the embedded year too, so the stray lands in staging keyed
   *  `it|1990`, a DIFFERENT raw key from the mainstream `it|2017` row — never
   *  reaching the yearless fallback rule at all. */
  "the row holding both 'It' films" should "elevate the stray's embedded year, not stage it yearless" in {
    // Neither slot publishes an ORIGINAL title (the real US/Flicks shape —
    // `identityTitle`'s doc: "every US and UK row"), so detection falls back to
    // the PLAIN title for the whole row: "It (2017)" / "It (1990)" reduce to the
    // single distinctive word "2017" / "1990" each ("it" alone is too short to
    // count), which differ — corroborated by the runtimes, 33 minutes apart. TWO
    // mainstream cinemas against Helios's one retro screening, so the larger group
    // is `main` and Helios — the YEARLESS one, matching the real incident — is the
    // stray that gets diverted (`identityGroups` sorts by `-slots.size` first).
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino   -> slot("It (2017)", Seq("Andy Muschietti"), None, Some(2017), Some(135)),
      KinoMuranow -> slot("It (2017)", Seq("Andy Muschietti"), None, Some(2017), Some(135)),
      Helios      -> slot("It (1990)", Seq("Tommy Lee Wallace"), None, None, Some(168))))
    val (_, staging, splitter) = fixture(record, "It", Some(2017))

    splitter.splitMixedRows() shouldBe 1

    val staged = staging.findAll().head
    withClue(s"staged with no year — the exact shape that folds straight back onto the 2017 row " +
      s"on every settle instead of converging: $staged\n") {
      staged.year shouldBe Some(1990)
    }
  }

  /** The convergence claim itself, for the shape above: a genuinely unresolvable
   *  stray (TMDB has no "It" 1990 MOVIE entry — Wallace's adaptation is a TV
   *  miniseries there, so `/search/movie` can never match it) must still settle to
   *  a STABLE state — parked in staging under its OWN (title, year) key, same as
   *  "Joanna d'Arc"'s never-resolving stray in "settle should split a mixed row,
   *  and change nothing on a second pass" above — rather than bounce between
   *  staging and the 2017 row forever. Real `settle`, real (empty-result) TMDB
   *  stub, twice. */
  "settle" should "converge a genuinely unresolvable 'It' 1990 stray to a stable staging park, not a bounce" in {
    val repository = new InMemoryMovieRepository()
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val staging    = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val service    = new MovieService(cache, new InProcessEventBus(),
      new TmdbClient(http = new tools.GetOnlyHttpFetch {
        override def get(url: String): String = """{"results":[]}"""
      }, apiKey = Some("stub")),
      staging = staging)

    cache.put(cache.keyOf("It", Some(2017)), MovieRecord(data = Map[Source, SourceData](
      Multikino   -> slot("It (2017)", Seq("Andy Muschietti"), None, Some(2017), Some(135)),
      KinoMuranow -> slot("It (2017)", Seq("Andy Muschietti"), None, Some(2017), Some(135)),
      Helios      -> slot("It (1990)", Seq("Tommy Lee Wallace"), None, None, Some(168)))))

    service.settle()
    val mainstreamAfterFirst = cache.get(cache.keyOf("It", Some(2017))).map(_.cinemaSlots.size)
    val stagedAfterFirst     = staging.findAll()
    mainstreamAfterFirst shouldBe Some(2)
    withClue(s"the 1990 stray should be parked in staging under its OWN year, not lost: $stagedAfterFirst\n")(
      stagedAfterFirst.map(_.year) should contain(Some(1990)))

    service.settle()
    withClue("a converged corpus must not re-divert the stray a second time, nor shrink the mainstream " +
      "row it already left — the churn this test guards: ") {
      cache.get(cache.keyOf("It", Some(2017))).map(_.cinemaSlots.size) shouldBe mainstreamAfterFirst
      staging.findAll().map(_.year) shouldBe stagedAfterFirst.map(_.year)
    }
    service.stop()
  }

  "a second pass" should "find nothing left to split" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      KinoMuranow -> slot("Joanna d'Arc", Seq("Luc Besson"), Some("Joan of Arc"), Some(1999), Some(160)),
      Helios      -> slot("Joanna d'Arc", Seq.empty, Some("Jóhanna af Örk"), Some(2025), None)))
    val (_, _, splitter) = fixture(record, "Joanna d'Arc", Some(2025))

    splitter.splitMixedRows() shouldBe 1
    splitter.splitMixedRows() shouldBe 0
  }
}
