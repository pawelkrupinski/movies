package services.movies

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{Multikino, MovieRecord, Showtime, Source, SourceData}
import org.mongodb.scala.{MongoClient, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.model.Filters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * PROBE: does a settle-time RE-KEY keep the film's showtimes?
 *
 * The hypothesis under test. `MovieRepository.upsert` re-stitches showtimes out of
 * `screenings` before writing, precisely so a cache record stripped of them isn't written
 * back as "this film has no showtimes" — but it re-stitches under the id it is WRITING to.
 * On a re-key that id is the NEW one, while the showtimes are still filed under the OLD
 * one, so the re-stitch would find nothing and `replaceFilm` would store nothing. If that
 * is right, every settle silently empties the rows it re-keys, and prod's 30-minute
 * sawtooth (~10k PL showtimes deleted and re-scraped every cycle, films left intact) is
 * explained.
 *
 * Driven through the PUBLIC settle entry point `backfillEmbeddedYears()` — the one
 * `SettleReaper` calls every 30 minutes — rather than the private `rekey`, so this
 * exercises what prod actually runs. Package `services.movies` for the internals it needs.
 *
 * This spec is written to be WRONG-ABLE: it asserts the showtimes survive. A failure
 * proves the hypothesis; a pass disproves it and sends me back to the evidence.
 */
class RekeyScreeningsIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val uri = Env.get("MONGODB_URI").get
  // Its own corpus: this suite hydrates a `CaffeineMovieCache` over the WHOLE `movies`
  // collection and settles it, which is not survivable for a neighbouring suite's rows.
  // Dropped when each leg's scope closes, so a run leaves no `*_rekey-screenings` behind.
  private val CorpusSuite = "rekey-screenings"

  // A yearless row whose cinema slot carries a delimited year — exactly what
  // `backfillEmbeddedYears` re-keys onto that year.
  private val title  = "__rekey-probe-sentinel__"
  private val when   = java.time.LocalDateTime.now().plusDays(3).withHour(20).withMinute(0).withSecond(0).withNano(0)

  it should "keep a film's showtimes when the settle re-keys it onto its embedded year" in
    tools.IntegrationCorpusDatabase.withDatabase(uri, CorpusSuite) { db =>
    val screenings = new MongoScreeningsRepository(Some(db))
    val slots      = new MongoSlotsRepository(Some(db))
    val repository = new MongoMovieRepository(Some(db), screenings = Some(screenings), slots = Some(slots), normalizer = titleNormalizer)
    val yearlessId = StoredMovieRecord.idFor(title, None, titleNormalizer)
    val yearedId   = StoredMovieRecord.idFor(title, Some(2026), titleNormalizer)
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    try {
      // A yearless row, one cinema, real showtimes — the shape a scrape leaves behind
      // before TMDB concludes. The slot title carries the year the settle will key on.
      cache.put(CacheKey(title, None, titleNormalizer), MovieRecord(
        tmdbId = Some(9911),
        data = Map[Source, SourceData](Multikino -> SourceData(
          title = Some(s"$title (2026)"), showtimes = Seq(Showtime(when, None))))))

      withClue("premise — the yearless row's showtimes are stored under its own id: ")(
        screenings.findForFilm(yearlessId).values.flatten should not be empty)

      // THE SETTLE. This is the 30-minute job.
      cache.backfillEmbeddedYears()

      // Whatever key the film ended up under, its showtimes must still exist.
      val movedTo  = if (screenings.findForFilm(yearedId).nonEmpty) yearedId else yearlessId
      val surviving = screenings.findForFilm(yearedId).values.flatten.toSeq ++
                      screenings.findForFilm(yearlessId).values.flatten.toSeq
      info(s"after settle: yearless($yearlessId)=${screenings.findForFilm(yearlessId).values.flatten.size} " +
           s"yeared($yearedId)=${screenings.findForFilm(yearedId).values.flatten.size} (settled at $movedTo)")

      withClue("the film was re-keyed and its showtimes did not come with it: ")(
        surviving should not be empty)
    } finally cache.stop()
  }

  // The settle above is a WHOLE-CORPUS operation, and this suite runs in parallel with
  // every other it suite. So the corpus it settles must not be the one the neighbours are
  // using: `canonicalizeBySanitize` merges same-tmdbId year-variants and DELETES the
  // losers, cascading their `screenings` and `movie_slots` away — it has no notion of
  // which spec owns a row. Sharing a database, seeding these two rows and running only
  // THIS suite logged `[movies.delete] film removed: id=foldorphansitsentinel|2026`,
  // which is `StagingFoldIntegrationSpec`'s sentinel pair: exactly the rows whose survival
  // that suite asserts, deleted from under it. Hence [[tools.IntegrationCorpusDatabase]].
  it should "settle its own corpus, not the database the other suites share" in {
    val neighbourTitle = "__neighbour-corpus-sentinel__"
    val neighbours     = Seq(Some(2025), Some(2026)).map(y => StoredMovieRecord.idFor(neighbourTitle, y, titleNormalizer))
    val client = MongoClient(uri)
    // The SHARED database — deliberately not this suite's own.
    val shared = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo")).getCollection("movies")
    try {
      // Two year-variants of one film under a single tmdbId: the shape a settle collapses.
      neighbours.foreach(id => Await.result(shared.replaceOne(Filters.eq("_id", id),
        org.mongodb.scala.Document("_id" -> id, "tmdbId" -> 9913,
          "sourceData" -> org.mongodb.scala.Document(),
          "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds))

      tools.IntegrationCorpusDatabase.withDatabase(uri, CorpusSuite) { own =>
        val cache = new CaffeineMovieCache(new MongoMovieRepository(
          Some(own),
          screenings = Some(new MongoScreeningsRepository(Some(own))),
          slots      = Some(new MongoSlotsRepository(Some(own))), normalizer = titleNormalizer), normalizer = titleNormalizer)
        try cache.backfillEmbeddedYears() finally cache.stop()
      }

      val left = Await.result(shared.find(Filters.in("_id", neighbours*)).toFuture(), 10.seconds)
        .flatMap(_.get("_id").map(_.asString().getValue))
      withClue("the settle reached into the shared corpus and folded a neighbour's rows: ")(
        left.sorted shouldBe neighbours.sorted)
    } finally {
      Await.ready(shared.deleteMany(Filters.in("_id", neighbours*)).toFuture(), 10.seconds)
      client.close()
    }
  }
}
