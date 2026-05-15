package services.movies

import models.{Cinema, CinemaShowings, MovieRecord, Helios, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class IdentityMergerSpec extends AnyFlatSpec with Matchers {

  private class FakeRepo(seed: Seq[(String, Option[Int], MovieRecord)] = Seq.empty) extends MovieRepo {
    private val store = mutable.LinkedHashMap.empty[(String, Option[Int]), MovieRecord]
    seed.foreach { case (t, y, e) => store.put((t, y), e) }
    override def enabled: Boolean = true
    override def findAll(): Seq[(String, Option[Int], MovieRecord)] =
      store.iterator.map { case ((t, y), e) => (t, y, e) }.toSeq
    override def upsert(t: String, y: Option[Int], e: MovieRecord): Unit = { store.put((t, y), e); () }
    override def delete(t: String, y: Option[Int]): Unit = { store.remove((t, y)); () }
  }

  private def mkEnrichment(
    tmdbId:    Option[Int]    = None,
    imdbId:    Option[String] = None,
    imdb:      Option[Double] = None,
    showings:  Map[Cinema, CinemaShowings] = Map.empty,
    variants:  Set[String]    = Set.empty
  ): MovieRecord =
    MovieRecord(
      imdbId         = imdbId,
      imdbRating     = imdb,
      metascore      = None,
      originalTitle  = None,
      tmdbId         = tmdbId,
      cinemaTitles   = variants,
      cinemaShowings = showings
    )

  private def slot(cinema: Cinema): CinemaShowings =
    CinemaShowings(filmUrl = None, posterUrl = None, synopsis = None, cast = None,
                   director = None, runtimeMinutes = None, releaseYear = None,
                   originalTitle = None, showtimes = Seq.empty)

  "mergeForTrigger" should "collapse a year=Some and a year=None row that share a tmdbId" in {
    val cache = new MovieCache(new FakeRepo())
    val a = mkEnrichment(tmdbId = Some(671), showings = Map(Multikino -> slot(Multikino)))
    val b = mkEnrichment(tmdbId = Some(671), showings = Map(Helios -> slot(Helios)))
    cache.put(cache.keyOf("Harry Potter", Some(2001)), a)
    cache.put(cache.keyOf("Harry Potter", None),       b)
    val merger = new IdentityMerger(cache)

    merger.mergeForTrigger(cache.keyOf("Harry Potter", None))

    // Survivor is year=Some, both cinema slots union into it.
    cache.get(cache.keyOf("Harry Potter", Some(2001))).get.cinemaShowings.keySet shouldBe Set(Multikino, Helios)
    cache.get(cache.keyOf("Harry Potter", None)) shouldBe None
  }

  // Regression: legacy duplicates that already live in Mongo with both rows
  // already TMDB-resolved never re-emit a TmdbResolved event (MovieService
  // skips already-resolved rows), so the event-driven merger never fires for
  // them. `mergeAll()` is the catch-up sweep that AppLoader calls after
  // hydration so these duplicates can't survive a restart.
  "mergeAll" should "collapse pre-hydrated duplicates that never fire a TmdbResolved event" in {
    val dup = mkEnrichment(tmdbId = Some(1432547), imdbId = Some("tt36148135"),
                           showings = Map(Multikino -> slot(Multikino)))
    val seeded = Seq(
      ("All You Need Is Kill", Some(2025), dup.copy(cinemaShowings = Map(Multikino -> slot(Multikino)))),
      ("All You Need Is Kill", None,       dup.copy(cinemaShowings = Map(Helios   -> slot(Helios))))
    )
    val cache  = new MovieCache(new FakeRepo(seeded))
    val merger = new IdentityMerger(cache)

    cache.snapshot().size shouldBe 2  // hydrated as-is before the sweep

    merger.mergeAll()

    cache.snapshot().size shouldBe 1
    val survivor = cache.get(cache.keyOf("All You Need Is Kill", Some(2025)))
    survivor                                 shouldBe defined
    survivor.get.cinemaShowings.keySet       shouldBe Set(Multikino, Helios)
    cache.get(cache.keyOf("All You Need Is Kill", None)) shouldBe None
  }

  it should "be a no-op when the trigger has no siblings (single row in group)" in {
    val cache = new MovieCache(new FakeRepo())
    val only  = mkEnrichment(tmdbId = Some(99), showings = Map(Multikino -> slot(Multikino)))
    cache.put(cache.keyOf("Only", Some(2025)), only)
    val merger = new IdentityMerger(cache)

    merger.mergeForTrigger(cache.keyOf("Only", Some(2025)))

    cache.get(cache.keyOf("Only", Some(2025))).get.tmdbId shouldBe Some(99)
    cache.snapshot().size shouldBe 1
  }

  it should "prefer year=Some over year=None when picking the survivor" in {
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("Foo", None),       mkEnrichment(tmdbId = Some(7)))
    cache.put(cache.keyOf("Foo", Some(2026)), mkEnrichment(tmdbId = Some(7)))
    val merger = new IdentityMerger(cache)

    merger.mergeForTrigger(cache.keyOf("Foo", None))

    cache.get(cache.keyOf("Foo", Some(2026))) shouldBe defined
    cache.get(cache.keyOf("Foo", None)) shouldBe None
  }

  it should "match siblings on imdbId when tmdbId differs (rare TMDB-dupe case) — only when the title matches too" in {
    val cache = new MovieCache(new FakeRepo())
    // Two rows with the SAME normalised title but different tmdbIds (TMDB
    // has two entries pointing to the same imdbId — happens with restored
    // releases, anniversary re-issues, etc.). These should merge.
    cache.put(cache.keyOf("Same Film", Some(2024)), mkEnrichment(tmdbId = Some(1), imdbId = Some("tt99")))
    cache.put(cache.keyOf("Same Film", Some(2025)), mkEnrichment(tmdbId = Some(2), imdbId = Some("tt99")))
    val merger = new IdentityMerger(cache)

    merger.mergeForTrigger(cache.keyOf("Same Film", Some(2024)))

    cache.snapshot().size shouldBe 1
    cache.snapshot().head._3.imdbId shouldBe Some("tt99")
  }

  // Regression: cross-script (Cyrillic vs Latin) variants of the same film
  // resolve to the same tmdbId, but they read very differently on the page
  // (Polish vs Ukrainian dub releases, for instance). User requirement is
  // that they stay as separate rows — the merger should NOT fold them
  // even when they share a tmdbId/imdbId.
  it should "NOT merge rows whose cleanTitles differ after normalisation, even with a matching tmdbId" in {
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("Mortal Kombat II",  Some(2026)),
              mkEnrichment(tmdbId = Some(931285), imdbId = Some("tt17490712")))
    cache.put(cache.keyOf("МОРТАЛ КОМБАТ ІІ", Some(2026)),
              mkEnrichment(tmdbId = Some(931285), imdbId = Some("tt17490712")))
    val merger = new IdentityMerger(cache)

    merger.mergeForTrigger(cache.keyOf("МОРТАЛ КОМБАТ ІІ", Some(2026)))

    // Both rows still exist — different scripts → different normalised
    // docIds → not siblings under the new rule.
    cache.snapshot().size shouldBe 2
    cache.get(cache.keyOf("Mortal Kombat II",  Some(2026))) shouldBe defined
    cache.get(cache.keyOf("МОРТАЛ КОМБАТ ІІ", Some(2026))) shouldBe defined
  }

  it should "NOT merge two unrelated films that happen to share an imdbId but have different titles" in {
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("Film A", Some(2024)), mkEnrichment(imdbId = Some("tt-shared")))
    cache.put(cache.keyOf("Film B", Some(2024)), mkEnrichment(imdbId = Some("tt-shared")))
    val merger = new IdentityMerger(cache)

    merger.mergeForTrigger(cache.keyOf("Film A", Some(2024)))

    cache.snapshot().size shouldBe 2
  }

  it should "not merge unrelated films (different tmdbId AND different imdbId)" in {
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("Film X", Some(2024)), mkEnrichment(tmdbId = Some(1), imdbId = Some("tt1")))
    cache.put(cache.keyOf("Film Y", Some(2024)), mkEnrichment(tmdbId = Some(2), imdbId = Some("tt2")))
    val merger = new IdentityMerger(cache)

    merger.mergeForTrigger(cache.keyOf("Film X", Some(2024)))
    merger.mergeForTrigger(cache.keyOf("Film Y", Some(2024)))

    cache.snapshot().size shouldBe 2
  }

  it should "fold non-None enrichment fields from the loser into the survivor" in {
    val cache = new MovieCache(new FakeRepo())
    val survivor = mkEnrichment(tmdbId = Some(50), imdb = None)
    val loser    = mkEnrichment(tmdbId = Some(50), imdb = Some(7.5))
    cache.put(cache.keyOf("A", Some(2024)), survivor)
    cache.put(cache.keyOf("A", None),       loser)
    val merger = new IdentityMerger(cache)

    merger.mergeForTrigger(cache.keyOf("A", Some(2024)))

    cache.get(cache.keyOf("A", Some(2024))).get.imdbRating shouldBe Some(7.5)
  }

  it should "union cinemaTitles across merged rows (same title, different years)" in {
    val cache = new MovieCache(new FakeRepo())
    // Same cleanTitle ⇒ same normalised docId base ⇒ siblings under the
    // title-equality rule. Different years keep them as separate cache
    // entries until the merger collapses them.
    cache.put(cache.keyOf("Same Title", Some(2024)),
              mkEnrichment(tmdbId = Some(8), variants = Set("Same Title", "Variant A")))
    cache.put(cache.keyOf("Same Title", None),
              mkEnrichment(tmdbId = Some(8), variants = Set("Same Title")))
    val merger = new IdentityMerger(cache)

    merger.mergeForTrigger(cache.keyOf("Same Title", Some(2024)))

    val survivor = cache.get(cache.keyOf("Same Title", Some(2024))).get
    survivor.cinemaTitles should contain allOf ("Same Title", "Variant A")
  }

  "onTmdbResolved" should "trigger the merge (eventually, async on worker pool)" in {
    import services.events.{EventBus, TmdbResolved}
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("A", Some(2024)), mkEnrichment(tmdbId = Some(42)))
    cache.put(cache.keyOf("A", None),       mkEnrichment(tmdbId = Some(42)))
    val bus = new EventBus()
    val merger = new IdentityMerger(cache)
    bus.subscribe(merger.onTmdbResolved)

    bus.publish(TmdbResolved("A", None, "tt42"))

    // Async — poll until the merge lands or fail after a generous bound.
    val deadline = System.currentTimeMillis() + 2000
    while (cache.snapshot().size > 1 && System.currentTimeMillis() < deadline)
      Thread.sleep(20)

    cache.snapshot().size shouldBe 1
    cache.get(cache.keyOf("A", Some(2024))) shouldBe defined

    merger.stop()
  }

  it should "leave deleted losers gone even when a rating listener writes after the merger" in {
    // Simulates the async race: merger deletes the loser, then a rating
    // listener (slow IMDb fetch) tries to write to the loser's key.
    // putIfPresent must NOT resurrect.
    val cache = new MovieCache(new FakeRepo())
    cache.put(cache.keyOf("A", Some(2024)), mkEnrichment(tmdbId = Some(42)))
    cache.put(cache.keyOf("A", None),       mkEnrichment(tmdbId = Some(42)))
    val merger = new IdentityMerger(cache)

    // Step 1: merger runs, deletes loser (year=None).
    merger.mergeFor("A", None)
    cache.get(cache.keyOf("A", None)) shouldBe None

    // Step 2: a delayed rating listener tries to write to the now-deleted loser key.
    val landed = cache.putIfPresent(
      cache.keyOf("A", None),
      _.copy(imdbRating = Some(8.5))
    )

    landed shouldBe false
    cache.get(cache.keyOf("A", None)) shouldBe None
    cache.snapshot().size shouldBe 1
  }
}
