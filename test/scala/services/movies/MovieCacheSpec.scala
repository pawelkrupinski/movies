package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime
import scala.collection.mutable

class MovieCacheSpec extends AnyFlatSpec with Matchers {

  // Fake repo: in-memory store with the same write-through contract as the
  // real MovieRepo. We bypass the Mongo connection entirely by
  // overriding the methods the cache calls.
  private class FakeRepo(seed: Seq[(String, Option[Int], MovieRecord)] = Seq.empty)
      extends MovieRepo {
    private val store = mutable.LinkedHashMap.empty[(String, Option[Int]), MovieRecord]
    val upserts = mutable.ListBuffer.empty[(String, Option[Int], MovieRecord)]
    val deletes = mutable.ListBuffer.empty[(String, Option[Int])]
    seed.foreach { case (t, y, e) => store.put((t, y), e) }
    override def enabled: Boolean = true
    override def findAll(): Seq[(String, Option[Int], MovieRecord)] =
      store.iterator.map { case ((t, y), e) => (t, y, e) }.toSeq
    override def upsert(t: String, y: Option[Int], e: MovieRecord): Unit = {
      store.put((t, y), e)
      upserts.append((t, y, e))
    }
    override def delete(t: String, y: Option[Int]): Unit = {
      store.remove((t, y))
      deletes.append((t, y))
    }
  }

  private def mkEnrichment(imdbId: String, rating: Option[Double] = None): MovieRecord =
    MovieRecord(imdbId = Some(imdbId), imdbRating = rating, metascore = None, originalTitle = None)

  "MovieCache" should "hydrate from the repo on construction" in {
    val seed = Seq(("Drzewo Magii", Some(2024), mkEnrichment("tt1")))
    val cache = new MovieCache(new FakeRepo(seed))

    cache.get(cache.keyOf("Drzewo Magii", Some(2024))) shouldBe Some(mkEnrichment("tt1"))
    cache.snapshot().map { case (t, y, _) => (t, y) } shouldBe Seq(("Drzewo Magii", Some(2024)))
  }

  it should "treat case + diacritics + whitespace differences as the same key" in {
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("Drzewo Magii", Some(2024)), mkEnrichment("tt9"))

    // Different spellings should collapse to the same row. Compare ignoring
    // cinemaTitles since `put` folds the original cleanTitle into it.
    val expected = mkEnrichment("tt9")
    cache.get(cache.keyOf("drzewo magii",   Some(2024))).map(_.copy(cinemaTitles = Set.empty)) shouldBe Some(expected)
    cache.get(cache.keyOf("DRZEWO   MAGII", Some(2024))).map(_.copy(cinemaTitles = Set.empty)) shouldBe Some(expected)
    // Different year is a different row.
    cache.get(cache.keyOf("Drzewo Magii",   Some(2025))) shouldBe None
  }

  "put" should "write through to the repo (cache + Mongo stay in lockstep)" in {
    val repo  = new FakeRepo()
    val cache = new MovieCache(repo)
    cache.put(cache.keyOf("X", Some(2024)), mkEnrichment("tt1"))

    // The written enrichment matches the input on every field except
    // `cinemaTitles`, which `put` folds the row's cleanTitle into.
    repo.upserts.toList.map { case (t, y, e) => (t, y, e.copy(cinemaTitles = Set.empty)) } shouldBe
      List(("X", Some(2024), mkEnrichment("tt1")))
  }

  // Phase-1 plumbing for the MovieCache transition. `cache.put` folds the
  // row's current cleanTitle into `cinemaTitles` so every raw cinema-reported
  // title that has ever resolved here ends up tracked. Used in phase 2 as the
  // corpus anchor for the new merge-key docId.
  "put" should "fold the row's cleanTitle into cinemaTitles" in {
    val repo  = new FakeRepo()
    val cache = new MovieCache(repo)
    cache.put(cache.keyOf("Top Gun: Maverick", Some(2022)), mkEnrichment("tt1745960"))

    cache.get(cache.keyOf("Top Gun: Maverick", Some(2022))).get.cinemaTitles shouldBe Set("Top Gun: Maverick")
    repo.upserts.toList.head._3.cinemaTitles shouldBe Set("Top Gun: Maverick")
  }

  it should "accumulate distinct cleanTitles across writes under the same docId (different casing → same key)" in {
    val repo  = new FakeRepo()
    val cache = new MovieCache(repo)
    // Two writes under keys that MovieService.normalize collapses to the
    // same docId (case + whitespace differences only) — the underlying row
    // accumulates both raw forms.
    cache.put(cache.keyOf("Drzewo Magii",   Some(2024)), mkEnrichment("tt9"))
    cache.put(cache.keyOf("drzewo  magii",  Some(2024)), mkEnrichment("tt9"))

    val titles = cache.get(cache.keyOf("Drzewo Magii", Some(2024))).get.cinemaTitles
    titles should contain allOf ("Drzewo Magii", "drzewo  magii")
  }

  it should "preserve existing cinemaTitles when writing an enrichment that already has some" in {
    val repo  = new FakeRepo()
    val cache = new MovieCache(repo)
    val enrichmentWithVariants = mkEnrichment("tt1").copy(cinemaTitles = Set("Variant A", "Variant B"))
    cache.put(cache.keyOf("Variant C", Some(2024)), enrichmentWithVariants)

    cache.get(cache.keyOf("Variant C", Some(2024))).get.cinemaTitles shouldBe
      Set("Variant A", "Variant B", "Variant C")
  }

  "invalidate" should "remove from both positive cache and repo" in {
    val repo  = new FakeRepo()
    val cache = new MovieCache(repo)
    val key   = cache.keyOf("X", Some(2024))
    cache.put(key, mkEnrichment("tt1"))
    repo.upserts.clear()

    cache.invalidate(key)

    cache.get(key) shouldBe None
    repo.deletes.toList shouldBe List(("X", Some(2024)))
  }

  "markMissing + isNegative" should "let callers track known-non-films without polluting the positive cache" in {
    val cache = new MovieCache(new FakeRepo())
    val key   = cache.keyOf("not-a-real-film", Some(2099))

    cache.isNegative(key) shouldBe false
    cache.markMissing(key)
    cache.isNegative(key) shouldBe true
    cache.get(key) shouldBe None  // negative cache never returns a positive value
  }

  // ── putIfPresent: no-resurrection writes ───────────────────────────────────

  "putIfPresent" should "update an existing row and return true" in {
    val repo  = new FakeRepo()
    val cache = new MovieCache(repo)
    cache.put(cache.keyOf("Existing", Some(2024)), mkEnrichment("tt1"))

    val landed = cache.putIfPresent(cache.keyOf("Existing", Some(2024)), _.copy(imdbRating = Some(8.5)))

    landed shouldBe true
    cache.get(cache.keyOf("Existing", Some(2024))).flatMap(_.imdbRating) shouldBe Some(8.5)
  }

  it should "be a no-op and return false when the row was deleted" in {
    val repo  = new FakeRepo()
    val cache = new MovieCache(repo)
    val key   = cache.keyOf("Gone", Some(2024))
    cache.put(key, mkEnrichment("tt1"))
    cache.invalidate(key)
    repo.upserts.clear()

    val landed = cache.putIfPresent(key, _.copy(imdbRating = Some(8.5)))

    landed shouldBe false
    cache.get(key) shouldBe None
    repo.upserts shouldBe empty
  }

  it should "operate on the current cached value, not a stale snapshot" in {
    // A rating listener that captured the row at T0, made a slow network
    // call, and now wants to update one field shouldn't clobber concurrent
    // updates to other fields. putIfPresent's updater receives the live row.
    val cache = new MovieCache(new FakeRepo())
    val key   = cache.keyOf("Foo", Some(2024))
    cache.put(key, mkEnrichment("tt1"))

    // Simulate "another listener already wrote metascore" between read and write.
    cache.put(key, mkEnrichment("tt1").copy(metascore = Some(70)))

    cache.putIfPresent(key, current => current.copy(imdbRating = Some(8.5)))

    val row = cache.get(key).get
    row.imdbRating shouldBe Some(8.5)   // our update landed
    row.metascore  shouldBe Some(70)    // concurrent update preserved
  }

  // ── recordCinemaScrape: per-cinema slot management ─────────────────────────
  //
  // Each cinema's scrape tick lands a full snapshot of its current showings.
  // recordCinemaScrape merges that into the unified cache: find-or-create the
  // matching record, replace this cinema's slot in cinemaShowings, and prune
  // the cinema's slot from any record whose film stopped appearing in the
  // fresh batch.

  private def showtime(ymdHm: String): Showtime = {
    val parsed = LocalDateTime.parse(ymdHm)
    Showtime(parsed, bookingUrl = None)
  }

  private def cinemaMovie(title: String, cinema: Cinema, year: Option[Int] = Some(2026),
                          poster: Option[String] = None, showtimes: Seq[Showtime] = Seq.empty): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title = title, releaseYear = year),
      cinema    = cinema,
      posterUrl = poster,
      filmUrl   = None,
      synopsis  = None,
      cast      = None,
      director  = None,
      showtimes = showtimes
    )

  // ── Provenance: per-(cinema, title, year) scrape dedup ─────────────────────
  //
  // recordCinemaScrape returns a `(CinemaMovie, CacheKey, isNew)` triple per
  // input. `isNew` is true the first time that exact `(cinema, raw title, raw
  // year)` tuple lands on a row; false on subsequent ticks that report the
  // same combination. ShowtimeCache uses the flag to suppress redundant
  // MovieRecordCreated events — every TMDB / IMDb / rating fetcher's listener
  // re-checks state on each event, so re-publishing for unchanged tuples
  // just churns dispatches. Any change (new cinema, new title spelling, year
  // correction) still flips `isNew` back to true so genuine new context can
  // trigger better enrichment.

  "recordCinemaScrape" should "flag the first scrape of a (cinema, title, year) tuple as new" in {
    val cache = new MovieCache(new FakeRepo())
    val touched = cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022))
    ))
    touched should have size 1
    touched.head._3 shouldBe true
  }

  it should "flag a repeat scrape of the same (cinema, title, year) as not-new" in {
    val cache = new MovieCache(new FakeRepo())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Top Gun: Maverick", Multikino, Some(2022))))
    val secondTick = cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022))
    ))
    secondTick should have size 1
    secondTick.head._3 shouldBe false
  }

  it should "flag the second cinema as new when it scrapes a film already in the cache from another cinema" in {
    val cache = new MovieCache(new FakeRepo())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Top Gun: Maverick", Multikino, Some(2022))))
    val helios = cache.recordCinemaScrape(Helios, Seq(
      cinemaMovie("Top Gun: Maverick", Helios, Some(2022))
    ))
    helios.head._3 shouldBe true   // Helios hasn't reported this row before
  }

  it should "flag a year correction from the same cinema as new" in {
    val cache = new MovieCache(new FakeRepo())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Bez wyjścia", Multikino, None)))
    // Same cinema now reports the same title with a year — different scrape
    // tuple, even though the redirect routes it onto the same row.
    val corrected = cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Bez wyjścia", Multikino, Some(2025))
    ))
    corrected.head._3 shouldBe true
    corrected.head._2 shouldBe cache.keyOf("Bez wyjścia", None)  // canonical key unchanged
  }

  it should "create a new record when no matching row exists yet" in {
    val cache = new MovieCache(new FakeRepo())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022), Some("multikino.jpg"), Seq(showtime("2026-06-01T18:00")))
    ))

    val row = cache.get(cache.keyOf("Top Gun: Maverick", Some(2022))).get
    row.cinemaShowings.keySet shouldBe Set(Multikino)
    row.cinemaShowings(Multikino).posterUrl shouldBe Some("multikino.jpg")
    row.cinemaShowings(Multikino).showtimes.size shouldBe 1
  }

  it should "merge two cinemas' slots into the same record when their titles share a docId" in {
    val cache = new MovieCache(new FakeRepo())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022), Some("multikino.jpg"))
    ))
    cache.recordCinemaScrape(Helios, Seq(
      // Punctuation-only variant — phase 2's docId rule collapses both.
      cinemaMovie("Top Gun Maverick", Helios, Some(2022), Some("helios.jpg"))
    ))

    val row = cache.get(cache.keyOf("Top Gun: Maverick", Some(2022))).get
    row.cinemaShowings.keySet shouldBe Set(Multikino, Helios)
    // Multikino's poster wins the merged view (priority rule).
    row.posterUrl shouldBe Some("multikino.jpg")
    // Both raw titles are recorded as variants.
    row.cinemaTitles should contain allOf ("Top Gun: Maverick", "Top Gun Maverick")
  }

  it should "preserve enrichment-side fields when only cinema data changes" in {
    val cache = new MovieCache(new FakeRepo())
    // Seed with an enriched record (TMDB-resolved, no cinemas yet).
    val seed = MovieRecord(
      imdbId = Some("tt1745960"), imdbRating = Some(8.2), metascore = Some(78),
      originalTitle = Some("Top Gun: Maverick"), tmdbId = Some(361743)
    )
    cache.put(cache.keyOf("Top Gun: Maverick", Some(2022)), seed)

    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022), Some("multikino.jpg"))
    ))

    val row = cache.get(cache.keyOf("Top Gun: Maverick", Some(2022))).get
    row.imdbId        shouldBe Some("tt1745960")
    row.imdbRating    shouldBe Some(8.2)
    row.metascore     shouldBe Some(78)
    row.cinemaShowings.keySet shouldBe Set(Multikino)
  }

  it should "prune a cinema's slot from records that didn't appear in the fresh scrape" in {
    val cache = new MovieCache(new FakeRepo())
    // Tick 1: Multikino reports A + B.
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("A", Multikino),
      cinemaMovie("B", Multikino)
    ))
    cache.get(cache.keyOf("A", Some(2026))).get.cinemaShowings should contain key Multikino
    cache.get(cache.keyOf("B", Some(2026))).get.cinemaShowings should contain key Multikino

    // Tick 2: Multikino only reports A. B's Multikino slot should be pruned.
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("A", Multikino)
    ))

    cache.get(cache.keyOf("A", Some(2026))).get.cinemaShowings should contain key Multikino
    cache.get(cache.keyOf("B", Some(2026))).get.cinemaShowings should not contain key (Multikino)
  }

  // Regression for "Diabeł / ДИЯВОЛ" cross-script accumulation. The row's
  // `cleanTitle` defines its primary script; `cinemaTitles` must only
  // collect variants in the same script. Cross-script spellings from
  // legacy data (or anything else that smuggles them in) get filtered
  // on every cache.put, so the user-visible row never displays a Cyrillic
  // title for a Polish film (or vice versa).
  "put" should "filter cross-script entries out of cinemaTitles" in {
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("Diabeł ubiera się u Prady 2", Some(2026)),
              mkEnrichment("tt33612209").copy(
                cinemaTitles = Set(
                  "Diabeł ubiera się u Prady 2",
                  "Diabeł ubiera się u prady 2",
                  "ДИЯВОЛ НОСИТЬ ПРАДА 2"           // ← Cyrillic, must be dropped
                )
              ))

    val row = cache.get(cache.keyOf("Diabeł ubiera się u Prady 2", Some(2026))).get
    row.cinemaTitles shouldBe Set("Diabeł ubiera się u Prady 2", "Diabeł ubiera się u prady 2")
  }

  "put" should "keep cinemaTitles non-Latin when the row's cleanTitle is non-Latin" in {
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("ДИЯВОЛ НОСИТЬ ПРАДА 2", Some(2026)),
              mkEnrichment("tt33612209").copy(
                cinemaTitles = Set(
                  "ДИЯВОЛ НОСИТЬ ПРАДА 2",
                  "Diabeł ubiera się u Prady 2"   // ← Latin, must be dropped
                )
              ))

    val row = cache.get(cache.keyOf("ДИЯВОЛ НОСИТЬ ПРАДА 2", Some(2026))).get
    row.cinemaTitles shouldBe Set("ДИЯВОЛ НОСИТЬ ПРАДА 2")
  }

  // Phase-3.5: stop the flap loop where a year=None cinema title gets re-
  // created every tick only to be deleted by the merger.
  it should "redirect a fresh year=None scrape onto an existing year=Some row that already knows the title" in {
    val cache = new MovieCache(new FakeRepo())
    // Multikino's prior tick created the year=Some(2025) row with
    // cinemaTitles = {"Bez wyjścia"}.
    val survivor = MovieRecord(
      imdbId = Some("tt1527793"), imdbRating = None, metascore = None,
      originalTitle = Some("어쩔수가없다"), tmdbId = Some(639988),
      cinemaTitles = Set("Bez wyjścia")
    )
    cache.put(cache.keyOf("Bez wyjścia", Some(2025)), survivor)
    cache.snapshot().size shouldBe 1

    // KinoBulgarska now scrapes the same film with year=None. Without the
    // redirect, this would create a duplicate (Bez wyjścia, None) row that
    // the merger has to clean up on the next TmdbResolved.
    cache.recordCinemaScrape(KinoBulgarska, Seq(
      cinemaMovie("Bez wyjścia", KinoBulgarska, year = None, showtimes = Seq(showtime("2026-06-01T18:00")))
    ))

    // The redirect should have folded the new slot into the year=2025 row.
    cache.snapshot().size shouldBe 1
    val row = cache.get(cache.keyOf("Bez wyjścia", Some(2025))).get
    row.cinemaShowings.keySet shouldBe Set(KinoBulgarska)
    row.cinemaShowings(KinoBulgarska).showtimes.size shouldBe 1
    cache.get(cache.keyOf("Bez wyjścia", None)) shouldBe None
  }

  it should "record the incoming raw cinema title in cinemaTitles even on the redirect path" in {
    val cache = new MovieCache(new FakeRepo())
    // Survivor row knows the "Mortal Kombat II" variant (e.g. previously
    // merged from a year=None row by IdentityMerger), but NOT "Mortal Kombat 2".
    val survivor = MovieRecord(
      imdbId = Some("tt17490712"), imdbRating = None, metascore = None,
      originalTitle = None, tmdbId = Some(931285),
      cinemaTitles = Set("Mortal Kombat II")
    )
    cache.put(cache.keyOf("Mortal Kombat II", Some(2026)), survivor)

    // A cinema reports the Arabic-numeral variant for the first time. The
    // primary key ("Mortal Kombat 2", 2026) doesn't exist, so we'd expect
    // a fresh row UNLESS the redirect catches it — but the redirect uses
    // cinemaTitles for matching, so unless "Mortal Kombat 2" is already in
    // some row's cinemaTitles, redirect won't fire. Here it doesn't fire.
    cache.recordCinemaScrape(KinoBulgarska, Seq(cinemaMovie("Mortal Kombat 2", KinoBulgarska, Some(2026))))

    // New row at ("Mortal Kombat 2", 2026) was created — its `cinemaTitles`
    // must include "Mortal Kombat 2" so the NEXT scrape of that variant
    // (after IdentityMerger folds it into the survivor) can redirect.
    val newRow = cache.get(cache.keyOf("Mortal Kombat 2", Some(2026))).get
    newRow.cinemaTitles should contain ("Mortal Kombat 2")
  }

  it should "record the incoming raw title on the redirect path so the next scrape uses the same row" in {
    val cache = new MovieCache(new FakeRepo())
    // Survivor already knows "Bez wyjścia" from a prior Multikino scrape.
    cache.put(cache.keyOf("Bez wyjścia", Some(2025)),
              MovieRecord(imdbId = None, imdbRating = None, metascore = None, originalTitle = None,
                         cinemaTitles = Set("Bez wyjścia")))

    // First KinoBulgarska scrape with year=None — redirect to survivor.
    cache.recordCinemaScrape(KinoBulgarska, Seq(cinemaMovie("Bez wyjścia", KinoBulgarska, None)))

    val row = cache.get(cache.keyOf("Bez wyjścia", Some(2025))).get
    // The raw title is recorded (it happens to equal the survivor's cleanTitle
    // here, but the same path applies when they differ).
    row.cinemaTitles should contain ("Bez wyjścia")
    cache.get(cache.keyOf("Bez wyjścia", None)) shouldBe None
  }

  it should "NOT redirect to a different-script row that incidentally carries the variant (cross-script protection)" in {
    val cache = new MovieCache(new FakeRepo())
    // Cyrillic survivor carries the Latin spelling in its cinemaTitles
    // (legacy from before cross-script merges were forbidden). A new Latin
    // scrape must NOT redirect into it.
    cache.put(cache.keyOf("МОРТАЛ КОМБАТ ІІ", Some(2026)),
              MovieRecord(imdbId = Some("tt17490712"), imdbRating = None, metascore = None,
                         originalTitle = None, tmdbId = Some(931285),
                         cinemaTitles = Set("МОРТАЛ КОМБАТ ІІ", "Mortal Kombat II")))

    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Mortal Kombat II", Multikino, Some(2026), showtimes = Seq(showtime("2026-06-01T18:00")))
    ))

    // No redirect — a fresh Latin row gets created, distinct from Cyrillic.
    cache.get(cache.keyOf("Mortal Kombat II", Some(2026))) shouldBe defined
    cache.get(cache.keyOf("Mortal Kombat II", Some(2026))).get.cinemaShowings.keySet shouldBe Set(Multikino)
    // Cyrillic row keeps whatever showings it had (none here) — not touched.
    cache.get(cache.keyOf("МОРТАЛ КОМБАТ ІІ", Some(2026))) shouldBe defined
  }

  it should "NOT redirect when two existing rows both know the cinema title (ambiguous)" in {
    val cache = new MovieCache(new FakeRepo())
    // Two different films share the cinema-reported title "Wspinaczka" —
    // one row per year, each pinned to a different imdbId. Redirecting a
    // year=None scrape would be a coin-flip → keep them distinct, let the
    // merger sort it out post-TMDB.
    cache.put(cache.keyOf("Wspinaczka", Some(2017)),
              MovieRecord(imdbId = Some("tt5157682"), imdbRating = None, metascore = None,
                         originalTitle = None, cinemaTitles = Set("Wspinaczka")))
    cache.put(cache.keyOf("Wspinaczka", Some(2025)),
              MovieRecord(imdbId = Some("tt36437006"), imdbRating = None, metascore = None,
                         originalTitle = None, cinemaTitles = Set("Wspinaczka")))

    cache.recordCinemaScrape(KinoBulgarska, Seq(cinemaMovie("Wspinaczka", KinoBulgarska, year = None)))

    cache.get(cache.keyOf("Wspinaczka", None)) shouldBe defined
    cache.snapshot().size shouldBe 3
  }

  // Regression for the user-visible "Milcząca przyjaciółka" triple-row:
  // the TMDB stage was creating a fresh row at the raw (title, year) key
  // even though `recordCinemaScrape`'s redirect had already attached the
  // cinema slot to a resolved sibling row. `hasResolvedSiblingByTitle`
  // gives `scheduleTmdbStage` a way to short-circuit.
  "hasResolvedSiblingByTitle" should "return true when a resolved row knows the raw cinema title" in {
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("Milcząca przyjaciółka", Some(2025)),
              MovieRecord(imdbId = Some("tt27811632"), imdbRating = None, metascore = None,
                         originalTitle = None, tmdbId = Some(1168719),
                         cinemaTitles = Set("Milcząca przyjaciółka")))

    cache.hasResolvedSiblingByTitle("Milcząca przyjaciółka") shouldBe true
  }

  it should "return false when the matching row has no tmdbId yet" in {
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("Foo", None),
              MovieRecord(imdbId = None, imdbRating = None, metascore = None, originalTitle = None,
                         cinemaTitles = Set("Foo")))

    cache.hasResolvedSiblingByTitle("Foo") shouldBe false
  }

  it should "return false when no row carries the title in cinemaTitles" in {
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("Something else", Some(2024)),
              MovieRecord(imdbId = None, imdbRating = None, metascore = None, originalTitle = None,
                         tmdbId = Some(42), cinemaTitles = Set("Something else")))

    cache.hasResolvedSiblingByTitle("Unrelated") shouldBe false
  }

  // Regression: a Cyrillic-titled row whose cinemaTitles incidentally lists
  // the Latin spelling (from a previous cross-script collapse) must NOT
  // short-circuit a Latin scrape. The Latin variant should get its own row.
  it should "return false when only a different-script row carries the variant (cross-script protection)" in {
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("МОРТАЛ КОМБАТ ІІ", Some(2026)),
              MovieRecord(imdbId = Some("tt17490712"), imdbRating = None, metascore = None,
                         originalTitle = None, tmdbId = Some(931285),
                         // Cyrillic row carries the Latin spelling from a
                         // prior collapse — but its cleanTitle is Cyrillic,
                         // which normalises differently from the Latin.
                         cinemaTitles = Set("МОРТАЛ КОМБАТ ІІ", "Mortal Kombat II")))

    cache.hasResolvedSiblingByTitle("Mortal Kombat II") shouldBe false
  }

  // ── Scrape-failure semantics ────────────────────────────────────────────
  //
  // A cinema's `fetch()` can return Seq.empty for two very different reasons:
  //   (a) The cinema is genuinely showing zero films right now (a holiday, a
  //       closure, or a brand-new site with no schedule yet).
  //   (b) The scraper failed silently — Cloudflare challenge, parser regex
  //       mismatch, ScrapingAnt 503, blank HTML, etc. — and returned [] even
  //       though the cinema is actually screening films normally.
  //
  // In case (b) the prune-on-tick would wipe EVERY slot that cinema holds
  // across the cache, then the next successful tick puts them all back. That
  // produces exactly the "row appears and disappears completely" symptom the
  // user observed: between a failed scrape and the next successful one, the
  // row's cinemaShowings is empty → toSchedules filters it out of the page.
  //
  // Since (a) is vanishingly rare in practice (cinemas don't go dark for 5
  // minutes), we treat Seq.empty as a likely failure and skip the prune.
  // Slots from previous ticks stay until the cinema's NEXT successful scrape
  // (which will prune anything genuinely dropped).
  it should "NOT prune when the scrape returned zero films (likely a scraper failure)" in {
    val cache = new MovieCache(new FakeRepo())
    // Seed with a film the cinema was previously showing.
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Foo", Multikino, Some(2026))))
    cache.get(cache.keyOf("Foo", Some(2026))).get.cinemaShowings.keySet shouldBe Set(Multikino)

    // Scraper "fails": returns empty list. With the safeguard this is a no-op.
    cache.recordCinemaScrape(Multikino, Seq.empty)

    cache.get(cache.keyOf("Foo", Some(2026))).get.cinemaShowings.keySet shouldBe Set(Multikino)
  }

  it should "still prune a slot when the scrape returned other films but not this one" in {
    val cache = new MovieCache(new FakeRepo())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Foo", Multikino, Some(2026)),
      cinemaMovie("Bar", Multikino, Some(2026))
    ))

    // Bar drops out — Foo stays, Bar's slot should be pruned (cinema genuinely
    // stopped showing it, scrape returned a non-empty alternative list).
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Foo", Multikino, Some(2026))
    ))

    cache.get(cache.keyOf("Foo", Some(2026))).get.cinemaShowings.keySet shouldBe Set(Multikino)
    cache.get(cache.keyOf("Bar", Some(2026))).get.cinemaShowings.keySet shouldBe Set.empty
  }

  // Regression for the "Mortal Kombat row appears and disappears" symptom: a
  // merged survivor with slots from multiple cinemas should not lose ALL of
  // one cinema's slot just because that cinema's scrape blanked momentarily.
  it should "preserve all cinemas' slots on a survivor row when one cinema's scrape blanks" in {
    val cache = new MovieCache(new FakeRepo())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Mortal Kombat", Multikino, Some(2026))))
    cache.recordCinemaScrape(Helios,    Seq(cinemaMovie("Mortal Kombat", Helios,    Some(2026))))
    val key = cache.keyOf("Mortal Kombat", Some(2026))
    cache.get(key).get.cinemaShowings.keySet shouldBe Set(Multikino, Helios)

    // Multikino's scraper has a hiccup and returns nothing this tick.
    cache.recordCinemaScrape(Multikino, Seq.empty)

    // Helios slot definitely stays. Multikino's slot is preserved by the
    // safeguard so the row keeps rendering until Multikino's next successful
    // tick (which is the "row reappears" the user saw 60s later).
    val row = cache.get(key).get
    row.cinemaShowings.keySet should contain (Helios)
    row.cinemaShowings.keySet should contain (Multikino)
  }

  it should "not prune other cinemas' slots when one cinema's tick lands" in {
    val cache = new MovieCache(new FakeRepo())
    // Both cinemas report the same film initially.
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Shared", Multikino, Some(2026), Some("mu.jpg"))))
    cache.recordCinemaScrape(Helios,    Seq(cinemaMovie("Shared", Helios,    Some(2026), Some("he.jpg"))))

    val before = cache.get(cache.keyOf("Shared", Some(2026))).get
    before.cinemaShowings.keySet shouldBe Set(Multikino, Helios)

    // Helios genuinely drops Shared (scrape returns other films, just not
    // Shared) — Helios's slot is pruned, Multikino's slot is untouched.
    cache.recordCinemaScrape(Helios, Seq(cinemaMovie("Different Film", Helios, Some(2026))))

    val after = cache.get(cache.keyOf("Shared", Some(2026))).get
    after.cinemaShowings.keySet shouldBe Set(Multikino)
  }

  "snapshot" should "return rows sorted by title (case-insensitive)" in {
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("Zorro", None),   mkEnrichment("tt3"))
    cache.put(cache.keyOf("alpha", None),   mkEnrichment("tt1"))
    cache.put(cache.keyOf("Beta", None),    mkEnrichment("tt2"))

    cache.snapshot().map { case (t, _, _) => t } shouldBe Seq("alpha", "Beta", "Zorro")
  }
}
