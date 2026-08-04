package services.movies

import models.{Helios, KinoMuranow, Multikino, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.InMemoryStagingRepository
import services.movies.SingleCountryNormalizer.titleNormalizer

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
    // …and the other is back in staging, carrying its OWN identity so it resolves
    // on Besson/1999 rather than on the row it was merged into.
    val staged = staging.findAll()
    staged should have size 1
    staged.head.record.cinemaSlots.head._2.director shouldBe Seq("Luc Besson")
    staged.head.record.cinemaSlots.head._2.releaseYear shouldBe Some(1999)
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

  "a row describing ONE film" should "be left alone" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot("Dreams", Seq("Michel Franco"), Some("Dreams: Sueños"), Some(2025), Some(98)),
      Helios    -> slot("Dreams", Seq.empty, None, None, None)))
    val (cache, staging, splitter) = fixture(record, "Dreams", Some(2025))

    splitter.splitMixedRows() shouldBe 0
    cache.get(cache.keyOf("Dreams", Some(2025))).map(_.cinemaSlots.size) shouldBe Some(2)
    staging.findAll() shouldBe empty
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
