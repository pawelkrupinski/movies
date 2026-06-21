package services.staging

import models.{Helios, Multikino, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CacheKey, CaffeineMovieCache, EnrichmentRetrigger, InMemoryMovieRepository, RetriggerKind}

class InMemoryStagingFolderSpec extends AnyFlatSpec with Matchers {

  private def resolved(cinema: Source, year: Int): MovieRecord =
    MovieRecord(tmdbId = Some(1454157), data = Map[Source, SourceData](
      cinema -> SourceData(title = Some("Kumotry"), releaseYear = Some(year)),
      Tmdb   -> SourceData(title = Some("Kumotry"), releaseYear = Some(year))))

  /** The fold must leave `movies` in the SAME state the periodic settle would — so
   *  loading the folded corpus and running `canonicalizeBySanitize` changes nothing
   *  (no re-key, no enrichment re-kick). Catches a folder that left a split (e.g. a
   *  cross-title duplicate the settle would later have to merge). */
  private def settleIsANoOpOver(movies: InMemoryMovieRepository): Unit = {
    val retriggered = scala.collection.mutable.ListBuffer.empty[Set[RetriggerKind]]
    val cache = new CaffeineMovieCache(movies, retrigger = new EnrichmentRetrigger {
      def retrigger(key: CacheKey, record: MovieRecord, kinds: Set[RetriggerKind]): Unit = { retriggered += kinds; () }
    })
    val before = cache.snapshot().map(r => (r.title, r.year)).toSet
    cache.canonicalizeBySanitize()
    withClue(s"a settle changed the folded movies state — folder ≠ settle: $before -> ${cache.snapshot().map(r => (r.title, r.year)).toSet}\n")(
      cache.snapshot().map(r => (r.title, r.year)).toSet shouldBe before)
    withClue(s"a settle re-kicked enrichment after the fold: ${retriggered.toList}\n")(retriggered shouldBe empty)
  }

  "foldGroup" should "move a film's staging rows into movies and delete them" in {
    val staging  = new InMemoryStagingRepository
    val movies   = new InMemoryMovieRepository
    staging.upsert(Helios, "Kumotry", Some(2026), resolved(Helios, 2026))
    staging.upsert(Multikino, "Kumotry", Some(2026), resolved(Multikino, 2026))
    val folder = new InMemoryStagingFolder(staging, movies)

    folder.foldGroup("Kumotry")

    staging.findAll() shouldBe empty
    val rows = movies.findAll()
    rows should have size 1
    rows.head.record.tmdbId shouldBe Some(1454157)
    rows.head.record.data.keySet shouldBe Set(Helios, Multikino, Tmdb)
    settleIsANoOpOver(movies)
  }

  it should "collapse a film's ±1-year variants into one movies row (settle absorbed into the fold)" in {
    // Cinema City reports 'Kumotry' at the production year 2025, the rest at the
    // release year 2026 (tmdbYear 2026). The group-scoped fold settles them into
    // ONE row keyed to 2026 — no separate settle pass.
    val staging = new InMemoryStagingRepository
    val movies  = new InMemoryMovieRepository
    staging.upsert(Multikino, "Kumotry", Some(2025), resolved(Multikino, 2025).copy(
      data = resolved(Multikino, 2025).data + (Tmdb -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026)))))
    staging.upsert(Helios, "Kumotry", Some(2026), resolved(Helios, 2026))

    new InMemoryStagingFolder(staging, movies).foldGroup("Kumotry")

    val rows = movies.findAll()
    rows should have size 1
    rows.head.year shouldBe Some(2026)
    rows.head.record.data.keySet shouldBe Set(Multikino, Helios, Tmdb)
    settleIsANoOpOver(movies)
  }

  it should "be a no-op when no staging rows match (already folded)" in {
    val staging = new InMemoryStagingRepository
    val movies  = new InMemoryMovieRepository
    new InMemoryStagingFolder(staging, movies).foldGroup("Ghost") shouldBe empty
    movies.findAll() shouldBe empty
  }

  it should "return the brand-new film as a promotion so its ratings can be scheduled" in {
    val staging = new InMemoryStagingRepository
    val movies  = new InMemoryMovieRepository
    staging.upsert(Helios, "Kumotry", Some(2026), resolved(Helios, 2026))

    val promotions = new InMemoryStagingFolder(staging, movies).foldGroup("Kumotry")

    promotions.map(_._1) shouldBe Seq(CacheKey("Kumotry", Some(2026)))
    promotions.head._2.tmdbId shouldBe Some(1454157)
    settleIsANoOpOver(movies)
  }

  it should "NOT return a promotion when the staging row merges into an existing movie" in {
    val staging = new InMemoryStagingRepository
    val movies  = new InMemoryMovieRepository
    movies.upsert("Kumotry", Some(2026), resolved(Helios, 2026))   // already in movies
    staging.upsert(Multikino, "Kumotry", Some(2026), resolved(Multikino, 2026))

    val promotions = new InMemoryStagingFolder(staging, movies).foldGroup("Kumotry")

    promotions shouldBe empty                                       // merged, not promoted
    movies.findAll().head.record.data.keySet shouldBe Set(Helios, Multikino, Tmdb)  // merge still happened
    settleIsANoOpOver(movies)
  }

  it should "merge a cross-title same-tmdbId sibling already in movies when the other-language newcomer folds" in {
    // The Polish "Gwiezdne wojny: Mandalorian i Grogu" is already a movies row; the
    // English "The Mandalorian and Grogu" newcomer folds, same tmdbId. The folder
    // must pull the cross-language sibling in (reconcileTmdbIds) and collapse them to
    // ONE row HERE — not leave a duplicate for the periodic settle.
    val staging = new InMemoryStagingRepository
    val movies  = new InMemoryMovieRepository
    movies.upsert("Gwiezdne wojny: Mandalorian i Grogu", Some(2026),
      MovieRecord(tmdbId = Some(700), data = Map[Source, SourceData](
        Tmdb   -> SourceData(title = Some("Gwiezdne wojny: Mandalorian i Grogu"),
                             englishTitle = Some("The Mandalorian and Grogu"), releaseYear = Some(2026)),
        Helios -> SourceData(title = Some("Gwiezdne wojny: Mandalorian i Grogu"), releaseYear = Some(2026)))))
    staging.upsert(Multikino, "The Mandalorian and Grogu", Some(2026),
      MovieRecord(tmdbId = Some(700), data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("The Mandalorian and Grogu"), releaseYear = Some(2026)),
        Tmdb      -> SourceData(title = Some("The Mandalorian and Grogu"), releaseYear = Some(2026)))))

    new InMemoryStagingFolder(staging, movies).foldGroup("The Mandalorian and Grogu")

    withClue(s"cross-title duplicate not merged at fold time: ${movies.findAll().map(r => (r.title, r.year))}\n")(
      movies.findAll() should have size 1)
    movies.findAll().head.record.data.keySet shouldBe Set(Helios, Multikino, Tmdb) // both languages' cinemas
    settleIsANoOpOver(movies)
  }
}
