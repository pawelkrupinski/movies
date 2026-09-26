package tools

import models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepository, StoredMovieRecord}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.readmodel.{Derivation, DerivationScope, DerivationVersion}

import java.time.LocalDateTime

/**
 * The derivation corpus decides whether a change moved what the projection derives from a row —
 * a new derivation, and a pass on every worker — or only moved the rows. On 2026-09-26 the whole
 * read-model snapshot's fingerprint decided it, so every scraper change counted, and five deploys
 * in an hour re-projected every country's corpus for 36 rewritten documents.
 *
 * A code change cannot be staged inside a spec, so a derivation change is staged the other way
 * round: the checked-in hashes record something other than what the current code projects the
 * checked-in rows to — which is exactly what an older projection having made them looks like.
 */
class ReadModelDerivationCorpusSpec extends AnyFlatSpec with Matchers {
  import ReadModelDerivationCorpus.*

  private val n = titleNormalizer

  private def record(title: String, tmdbId: Int, poster: String = "https://mk/poster.jpg"): MovieRecord =
    MovieRecord(imdbRating = Some(7.0), tmdbId = Some(tmdbId), data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some(title), releaseYear = Some(2024), filmUrl = Some(s"https://mk/$tmdbId"),
        posterUrl = Some(poster + s"?$tmdbId"),
        showtimes = Seq(Showtime(LocalDateTime.parse("2026-06-12T20:00"), bookingUrl = Some("https://book"))))))

  private def rows(films: Seq[(String, Int)], poster: String = "https://mk/poster.jpg"): Seq[StoredMovieRecord] = {
    val repository = new InMemoryMovieRepository(normalizer = n)
    films.foreach { case (title, id) => repository.upsert(title, Some(2024), record(title, id, poster)) }
    repository.findAll()
  }

  private val Films = Seq("Anora" -> 1, "Flow" -> 2, "Conclave" -> 3)
  private val Current = Derivation(DerivationVersion("current"), DerivationScope.Full)

  /** What is checked in after a regeneration that named `derivation`. */
  private def checkedIn(corpus: Seq[StoredMovieRecord], derivation: Derivation = Current): (String, String) = {
    val text = renderRows(corpus)
    text -> renderHashes(derivation, parseRows(text, n).map(hashes(_, n)))
  }

  /** The same hashes, with `part` of `title`'s row recorded as something the current code does not make. */
  private def recordedDifferently(hashesText: String, title: String, part: String): String =
    hashesText.linesIterator.map { line =>
      if (line.endsWith("\t" + title)) line.replaceAll(s"\t$part=[0-9a-f]+", s"\t$part=0000000000000000") else line
    }.mkString("", "\n", "\n")

  "the rows file" should "be the same text however the scrape happened to mint the rows' ids" in {
    renderRows(rows(Films)) shouldBe renderRows(rows(Films.reverse))
  }

  // A map past four entries iterates in hash order, not insertion order: a film listed by five
  // cinemas wrote its `sourceData` in a different order on the second regeneration, under a
  // different content-derived id, and the rows file never settled.
  it should "be the same text however the pipeline happened to order a film's cinemas" in {
    val cinemas: Seq[Source] = Seq(Helios, Multikino, KinoMuranow, CinemaCityPoznanPlaza, KinoPalacowe, Rialto)
    def film(order: Seq[Source]) = {
      val repository = new InMemoryMovieRepository(normalizer = n)
      val slot = (c: Source) => c -> SourceData(title = Some("Anora"), releaseYear = Some(2024), filmUrl = Some(s"https://x/${c.displayName}"),
        showtimes = Seq(Showtime(LocalDateTime.parse("2026-06-12T20:00"), bookingUrl = Some("https://book"))))
      repository.upsert("Anora", Some(2024), MovieRecord(tmdbId = Some(1), data = scala.collection.immutable.ListMap(order.map(slot)*)))
      repository.findAll()
    }
    renderRows(film(cinemas)) shouldBe renderRows(film(cinemas.reverse))
  }

  it should "round-trip every row through the prod codec to the same projection" in {
    val corpus = rows(Films)
    val (rowsText, hashesText) = checkedIn(corpus)
    moved(parseHashes(hashesText), parseRows(rowsText, n), n) shouldBe empty
    parseRows(rowsText, n).map(_.title).sorted shouldBe Films.map(_._1).sorted
  }

  "a regeneration over the same corpus" should "keep the derivation it had" in {
    val regeneration = regenerate(rows(Films), n, Some(checkedIn(rows(Films))))
    regeneration.bumped shouldBe false
    regeneration.derivation shouldBe Current
  }

  // THE CASE THE CORPUS EXISTS FOR: a scraper change moves the rows (a new film, a new poster
  // URL), and the checked-in rows still project exactly as recorded — so no pass is owed.
  "a regeneration after the ROWS moved" should "keep the derivation, whatever changed in the rows" in {
    val before = checkedIn(rows(Films))
    val after  = rows(Films :+ ("Nosferatu" -> 4), poster = "https://mk/new-poster-host.jpg")
    val regeneration = regenerate(after, n, Some(before))
    regeneration.bumped shouldBe false
    regeneration.derivation shouldBe Current
    regeneration.rowsText should not be before._1
  }

  "a regeneration after the projection of an unchanged row moved" should
    "name a new derivation, owing only the cards when only a card's part moved" in {
    val (rowsText, hashesText) = checkedIn(rows(Films))
    val regeneration = regenerate(rows(Films), n, Some(rowsText -> recordedDifferently(hashesText, "Flow", "poster")))
    regeneration.bumped shouldBe true
    regeneration.derivation.scope shouldBe DerivationScope.Cards
    regeneration.derivation.version should not be Current.version
    regeneration.moved.map(m => m.title -> m.parts) shouldBe Seq("Flow" -> Seq("poster"))
    withClue("the new version is a function of what moved, so every machine names the same one: ") {
      regenerate(rows(Films), n, Some(rowsText -> recordedDifferently(hashesText, "Flow", "poster"))).derivation shouldBe
        regeneration.derivation
    }
    parseHashes(regeneration.hashesText).derivation shouldBe regeneration.derivation
  }

  it should "owe everything when a row's screenings moved" in {
    val (rowsText, hashesText) = checkedIn(rows(Films))
    val regeneration = regenerate(rows(Films), n, Some(rowsText -> recordedDifferently(hashesText, "Anora", "screenings")))
    regeneration.derivation.scope shouldBe DerivationScope.Full
  }

  it should "owe everything when the cards a row produces moved" in {
    val (rowsText, hashesText) = checkedIn(rows(Films))
    val regeneration = regenerate(rows(Films), n, Some(rowsText -> recordedDifferently(hashesText, "Anora", "cards")))
    regeneration.derivation.scope shouldBe DerivationScope.Full
  }

  "a regeneration with nothing checked in" should "name a new derivation owing everything — nothing could be compared" in {
    val regeneration = regenerate(rows(Films), n, None)
    regeneration.bumped shouldBe true
    regeneration.derivation.scope shouldBe DerivationScope.Full
  }

  "the history entry" should "be the Scala line to append" in {
    historyEntry(Derivation(DerivationVersion("abc"), DerivationScope.Cards)) shouldBe
      """Derivation(DerivationVersion("abc"), DerivationScope.Cards)"""
  }
}
