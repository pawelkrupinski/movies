package tools

import models.{Cinema, CinemaMovie, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.scrapes.{ArchivedScrape, SuccessfulScrape}

import java.time.{Instant, LocalDateTime}
import scala.util.Random
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * The slice the fast convergence leg replays. Worth its own spec because the sample
 * has to be a corpus in the same sense the whole one is — the leg runs the identical
 * fixpoint, no-loss and production-band assertions over it, and any of them will
 * misfire on a slice that is subtly malformed rather than merely smaller.
 */
class CorpusSampleSpec extends AnyFlatSpec with Matchers {

  private def film(cinema: Cinema, title: String) = CinemaMovie(
    Movie(title), cinema, None, None, None, Nil, Nil,
    Seq(Showtime(LocalDateTime.of(2026, 8, 2, 18, 0), None, None, Nil)))

  private def venue(cinema: Cinema, titles: String*) = ArchivedScrape(
    cinema      = cinema,
    city        = Some("Poznań"),
    lastSuccess = Some(SuccessfulScrape(at = Instant.parse("2026-08-01T06:00:00Z"),
                                        listingComplete = true, films = titles.map(film(cinema, _)))),
    lastBarren  = None)

  private val corpus = Seq(
    venue(models.Helios,    "Diuna", "Arco",   "Brzezina"),
    venue(models.Multikino, "DIUNA", "Zawodowcy"))

  "filmKeys" should "collapse every spelling of a film to one entry" in {
    // "Diuna" and "DIUNA" are one film — sampling them separately would waste a pick
    // and, worse, could take one venue's copy and leave the other's behind.
    CorpusSample.filmKeys(corpus, titleNormalizer) should contain theSameElementsAs
      Seq("diuna", "arco", "brzezina", "zawodowcy")
  }

  "pick" should "take the whole universe when it is smaller than the sample" in {
    CorpusSample.pick(corpus, 100, new Random(1), titleNormalizer) should have size 4
  }

  it should "take exactly the requested number when the universe is larger" in {
    CorpusSample.pick(corpus, 2, new Random(1), titleNormalizer) should have size 2
  }

  /** Sampled by FILM: a picked film arrives from EVERY cinema that reports it, spelt
   *  however each spells it. Picking listings instead would usually take one venue's
   *  copy and lose the cross-venue fold that makes the corpus interesting. */
  "trim" should "keep every venue's listing of a sampled film" in {
    val trimmed = CorpusSample.trim(corpus, Set("diuna"), titleNormalizer)

    trimmed.map(_.cinema) should contain theSameElementsAs Seq(models.Helios, models.Multikino)
    trimmed.flatMap(_.films.map(_.movie.title)) should contain theSameElementsAs Seq("Diuna", "DIUNA")
  }

  // A venue left with nothing must DROP OUT, not linger empty: the no-loss assertion
  // compares the cinemas the archive holds against the cinemas the read model emits,
  // and an empty row would claim a cinema that has nothing to emit.
  it should "drop a venue whose every film was left out" in {
    CorpusSample.trim(corpus, Set("zawodowcy"), titleNormalizer).map(_.cinema) shouldBe Seq(models.Multikino)
  }

  // The slice has to stay a faithful corpus — the replay reads the scrape instant and
  // the completeness flag off these rows and renders against them.
  it should "carry each row's scrape instant and completeness through untouched" in {
    val trimmed = CorpusSample.trim(corpus, Set("diuna"), titleNormalizer).head

    trimmed.lastSuccess.map(_.at)              shouldBe Some(Instant.parse("2026-08-01T06:00:00Z"))
    trimmed.lastSuccess.map(_.listingComplete) shouldBe Some(true)
    trimmed.city                               shouldBe Some("Poznań")
  }

  it should "keep the showtimes of the films it keeps" in {
    CorpusSample.trim(corpus, Set("arco"), titleNormalizer).flatMap(_.films.flatMap(_.showtimes)) should have size 1
  }

  /** The join to production. Prod keys a film by its FOLDED display title, which no
   *  cinema necessarily wrote; what both sides can derive from the same (cinema, title)
   *  is the SLOT KEY, so the baseline is taken by matching those — one key per venue
   *  spelling, so a prod row is found whichever venue's wording it folded under. */
  "slotKeysOf" should "return one slot key per venue spelling of the sampled films" in {
    CorpusSample.slotKeysOf(corpus, Set("diuna"), titleNormalizer) should
      contain theSameElementsAs Seq("Helios Posnania␟diuna", "Multikino Stary Browar␟diuna")
  }

  /** Two venues shouting a title differently still key alike, but a DECORATED listing
   *  keys on its own decoration — which is why the key has to come from
   *  `CinemaShowing.keyFor` rather than from anything this object spells out. */
  it should "key a decorated listing under its own spelling" in {
    CorpusSample.slotKeysOf(Seq(venue(models.Helios, "Diuna (dubbing)")), Set("diunadubbing"), titleNormalizer) shouldBe
      Set("Helios Posnania␟diunadubbing")
  }

  it should "return nothing for a film that was not sampled" in {
    CorpusSample.slotKeysOf(corpus, Set("arco"), titleNormalizer) shouldBe Set("Helios Posnania␟arco")
    CorpusSample.slotKeysOf(corpus, Set.empty, titleNormalizer) shouldBe empty
  }
}
