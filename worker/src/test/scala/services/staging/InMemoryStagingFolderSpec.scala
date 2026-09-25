package services.staging

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{Helios, MikroBronowice, Multikino, MovieRecord, Source, SourceData, Tmdb}
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
    }, normalizer = titleNormalizer)
    val before = cache.snapshot().map(r => (r.title, r.year)).toSet
    cache.canonicalizeBySanitize()
    withClue(s"a settle changed the folded movies state — folder ≠ settle: $before -> ${cache.snapshot().map(r => (r.title, r.year)).toSet}\n")(
      cache.snapshot().map(r => (r.title, r.year)).toSet shouldBe before)
    withClue(s"a settle re-kicked enrichment after the fold: ${retriggered.toList}\n")(retriggered shouldBe empty)
  }

  "foldGroup" should "move a film's staging rows into movies and delete them" in {
    val staging  = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val movies   = new InMemoryMovieRepository(normalizer = titleNormalizer)
    staging.upsert(Helios, "Kumotry", Some(2026), resolved(Helios, 2026))
    staging.upsert(Multikino, "Kumotry", Some(2026), resolved(Multikino, 2026))
    val folder = new InMemoryStagingFolder(staging, movies, normalizer = titleNormalizer)

    folder.foldGroup("Kumotry")

    staging.findAll() shouldBe empty
    val rows = movies.findAll()
    rows should have size 1
    rows.head.record.tmdbId shouldBe Some(1454157)
    rows.head.record.data.keySet shouldBe Set(Helios, Multikino, Tmdb)
    settleIsANoOpOver(movies)
  }

  // A brand-new film's id is minted by asking whether each candidate is a live id. An
  // unreadable answer used to count as "free", so the fold could write the new film over
  // the document that holds that id — here a film since retitled away from the key.
  it should "abort, not mint over a live document, when it cannot read whether an id is taken" in {
    val staging = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val movies  = new services.movies.UnreadableByIdMovieRepository(keyReadsFail = false, titleNormalizer = titleNormalizer)
    movies.failing = false
    val key      = CacheKey("Kumotry", Some(2026), titleNormalizer)
    val occupied = services.movies.FilmId.fresh(key, _ => false)
    val other    = MovieRecord(tmdbId = Some(42), data = Map[Source, SourceData](Helios -> SourceData(title = Some("Inny Film"))))
    movies.upsert(occupied, CacheKey("Inny Film", None, titleNormalizer), other)
    staging.upsert(Multikino, "Kumotry", Some(2026), resolved(Multikino, 2026))

    movies.failing = true
    an[Exception] should be thrownBy new InMemoryStagingFolder(staging, movies, normalizer = titleNormalizer).foldGroup("Kumotry")
    movies.failing = false

    movies.findById(occupied).map(_.record.tmdbId) shouldBe Some(Some(42))   // the live film is untouched
    staging.findAll() should have size 1                                     // and the fold retries later
  }

  it should "collapse a film's ±1-year variants into one movies row (settle absorbed into the fold)" in {
    // Cinema City reports 'Kumotry' at the production year 2025, the rest at the
    // release year 2026 (tmdbYear 2026). The group-scoped fold settles them into
    // ONE row keyed to 2026 — no separate settle pass.
    val staging = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val movies  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    staging.upsert(Multikino, "Kumotry", Some(2025), resolved(Multikino, 2025).copy(
      data = resolved(Multikino, 2025).data + (Tmdb -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026)))))
    staging.upsert(Helios, "Kumotry", Some(2026), resolved(Helios, 2026))

    new InMemoryStagingFolder(staging, movies, normalizer = titleNormalizer).foldGroup("Kumotry")

    val rows = movies.findAll()
    rows should have size 1
    rows.head.year shouldBe Some(2026)
    rows.head.record.data.keySet shouldBe Set(Multikino, Helios, Tmdb)
    settleIsANoOpOver(movies)
  }

  it should "fold + delete a row whose display title sanitizes away from its _id (the Toy Story 5 drift)" in {
    // Prod regression: "Toy Story 5- dubbing" keys `_id` on sanitize toystory5,
    // but `chooseDisplay` strips "- dubbing" → "Toy Story 5" → sanitize toystoryv.
    // The reaper folds on the re-derived title; selection must follow sanitize(r.title),
    // not the `_id` middle, or the row strands and re-folds forever.
    val staging = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val movies  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    staging.upsert(MikroBronowice, "Toy Story 5- dubbing", None,
      MovieRecord(tmdbAttempt = Some(services.resolution.TmdbAttempt.Legacy), searchTitle = Some("Toy Story 5- dubbing"),
        data = Map[Source, SourceData](MikroBronowice -> SourceData(title = Some("Toy Story 5- dubbing")))))
    val reaperTitle = staging.findAll().head.title   // what StagingReaper passes to the fold

    new InMemoryStagingFolder(staging, movies, normalizer = titleNormalizer).foldGroup(reaperTitle)

    withClue(s"drifting staging row not deleted — it would re-fold forever: ${staging.findAll().map(_.id)}\n")(
      staging.findAll() shouldBe empty)
    movies.findAll() should have size 1
    movies.findAll().head.record.tmdbNoMatch shouldBe true
  }

  it should "be a no-op when no staging rows match (already folded)" in {
    val staging = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val movies  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    new InMemoryStagingFolder(staging, movies, normalizer = titleNormalizer).foldGroup("Ghost") shouldBe empty
    movies.findAll() shouldBe empty
  }

  it should "return the brand-new film as a promotion so its ratings can be scheduled" in {
    val staging = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val movies  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    staging.upsert(Helios, "Kumotry", Some(2026), resolved(Helios, 2026))

    val promotions = new InMemoryStagingFolder(staging, movies, normalizer = titleNormalizer).foldGroup("Kumotry")

    promotions.map(_._1) shouldBe Seq(CacheKey("Kumotry", Some(2026), titleNormalizer))
    promotions.head._2.tmdbId shouldBe Some(1454157)
    settleIsANoOpOver(movies)
  }

  it should "NOT return a promotion when the staging row merges into an existing movie" in {
    val staging = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val movies  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    movies.upsert("Kumotry", Some(2026), resolved(Helios, 2026))   // already in movies
    staging.upsert(Multikino, "Kumotry", Some(2026), resolved(Multikino, 2026))

    val promotions = new InMemoryStagingFolder(staging, movies, normalizer = titleNormalizer).foldGroup("Kumotry")

    promotions shouldBe empty                                       // merged, not promoted
    movies.findAll().head.record.data.keySet shouldBe Set(Helios, Multikino, Tmdb)  // merge still happened
    settleIsANoOpOver(movies)
  }

  it should "merge a cross-title same-tmdbId sibling already in movies when the other-language newcomer folds" in {
    // The Polish "Gwiezdne wojny: Mandalorian i Grogu" is already a movies row; the
    // English "The Mandalorian and Grogu" newcomer folds, same tmdbId. The folder
    // must pull the cross-language sibling in (reconcileTmdbIds) and collapse them to
    // ONE row HERE — not leave a duplicate for the periodic settle.
    val staging = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val movies  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    movies.upsert("Gwiezdne wojny: Mandalorian i Grogu", Some(2026),
      MovieRecord(tmdbId = Some(700), data = Map[Source, SourceData](
        Tmdb   -> SourceData(title = Some("Gwiezdne wojny: Mandalorian i Grogu"),
                             englishTitle = Some("The Mandalorian and Grogu"), releaseYear = Some(2026)),
        Helios -> SourceData(title = Some("Gwiezdne wojny: Mandalorian i Grogu"), releaseYear = Some(2026)))))
    staging.upsert(Multikino, "The Mandalorian and Grogu", Some(2026),
      MovieRecord(tmdbId = Some(700), data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("The Mandalorian and Grogu"), releaseYear = Some(2026)),
        Tmdb      -> SourceData(title = Some("The Mandalorian and Grogu"), releaseYear = Some(2026)))))

    new InMemoryStagingFolder(staging, movies, normalizer = titleNormalizer).foldGroup("The Mandalorian and Grogu")

    withClue(s"cross-title duplicate not merged at fold time: ${movies.findAll().map(r => (r.title, r.year))}\n")(
      movies.findAll() should have size 1)
    movies.findAll().head.record.data.keySet shouldBe Set(Helios, Multikino, Tmdb) // both languages' cinemas
    settleIsANoOpOver(movies)
  }

  // The in-memory twin of `StagingFoldConcurrentTmdbRaceIntegrationSpec`'s merge-order case:
  // an unresolved spelling folds INTO a sibling that already holds the tmdbId. The survivor
  // takes over the retired row's tmdbId, so the retired row has to go first — written first,
  // the survivor collides with it (the unique index in Mongo; `IdentityHeld` here), and the
  // fold then deletes the one row that held the film.
  it should "retire the rows it folds away before writing the survivor that takes their identity" in {
    val staging = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val movies  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val tmdbId  = 424353
    val folder  = new InMemoryStagingFolder(staging, movies, normalizer = titleNormalizer)
    def concluded(cinema: Source, title: String) =
      MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](cinema -> SourceData(title = Some(title))))
    staging.upsert(Multikino, "Lalka reż. mergeorder", Some(2026), concluded(Multikino, "Lalka reż. mergeorder"))
    folder.foldGroup("Lalka reż. mergeorder")
    // The later spelling was promoted before TMDB answered, so it already has a bare row.
    movies.upsert("Ladies Night - Mergeorder", Some(2026),
      MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(title = Some("Ladies Night - Mergeorder")))))
    staging.upsert(Helios, "Ladies Night - Mergeorder", Some(2026), concluded(Helios, "Ladies Night - Mergeorder"))

    folder.foldGroup("Ladies Night - Mergeorder")

    val holders = movies.findAll().filter(_.record.tmdbId.contains(tmdbId))
    withClue(s"rows: ${movies.findAll().map(r => (r.id, r.record.tmdbId))}\n") { holders should have size 1 }
    holders.head.record.cinemaData.keySet shouldBe Set(Multikino, Helios)
  }
}
