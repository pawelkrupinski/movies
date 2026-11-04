package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityMeasures.{Film, Listing, ListingFilm, Qualifiers}

/** `certified-title` ([[Corroboration.certifiedAlone]]): the venue's own certificate and running time each rule
 *  namesakes out, and only one left standing is taken. */
class CertifiedTitleSpec extends AnyFlatSpec with Matchers {
  private val calibration = IdentityCalibration.resolver
  private val acceptance  = new Acceptance(calibration)
  private val houses      = StageRelays.Houses.Unknown

  /** `films` scored for a UK venue's `listing`, as [[AcceptanceSpec]] scores them. */
  private def ranked(listing: Listing, films: (Int, Film, Option[Int])*): Seq[Scored] = {
    def rivalling(film: Film) = IdentityMeasures.Rivalling(IdentityMeasures.titleRelation(listing, film, houses, Qualifiers.Unknown).value)
    val close = films.count { case (_, film, _) => rivalling(film) }
    films.map { case (tmdbId, film, rank) =>
      val measures = IdentityMeasures.listingFilm(listing, film, rank, close - (if (rivalling(film)) 1 else 0), 0, houses, country = Some("GB"))
      Scored(Candidate(tmdbId, film), calibration.probability(ListingFilm, measures), measures, denial = None, listing, rank)
    }.sortBy(scored => (-scored.probability, scored.candidate.tmdbId))
  }
  private def certified(candidates: Seq[Scored]): Option[Int] = acceptance.byRule("certified-title")(candidates).map(_._1.candidate.tmdbId)
  /** The own fact `Corroboration.facts` reads (and the namesake-ambiguous rule with it). */
  private def singles(candidates: Seq[Scored], film: Int): Boolean =
    Corroboration.certifiedAlone(candidates.find(_.candidate.tmdbId == film).get, candidates)

  // UK Plough Arts Centre Torrington's bare "Dracula", 87′ and 12A: TMDB's namesakes by the venue's running time and GB
  // certificate. The 1931 film fits a longer slot, so the running time alone leaves it beside Hammer's; its PG does not.
  private val torrington = Listing("Dracula", runtime = Some(87), certificate = Some("12A"))
  private val hammer     = (11868, Film("Dracula", year = Some(1958), runtime = Some(82), certificates = Some("GB=12A")), Some(2))
  private val browning   = (138, Film("Dracula", year = Some(1931), runtime = Some(74), certificates = Some("GB=PG")), Some(3))
  private val untold     = (1655673, Film("Dracula", year = Some(2002), runtime = Some(104), certificates = Some("GB=12A")), Some(4))
  private val besson     = (1246049, Film("Dracula", year = Some(2025), runtime = Some(130), certificates = Some("GB=15")), Some(1))

  "certified-title" should "take the one namesake the venue's certificate and running time leave" in {
    val dracula = ranked(torrington, hammer, browning, untold, besson)
    certified(dracula) shouldBe Some(11868)
    acceptance.aloneNamed(dracula).map(_._1._1.candidate.tmdbId) shouldBe Some(11868)
    // the namesake-ambiguous rule reads the same fact as singling it out
    singles(dracula, 11868) shouldBe true
    singles(dracula, 138) shouldBe false
  }

  it should "take nothing where the running time alone rules the namesakes out: that is the runtime fact's" in {
    certified(ranked(torrington, hammer, untold, besson)) shouldBe None
  }

  it should "never rule a namesake out by a certificate TMDB does not record in the venue's country" in {
    val unrated = (138, Film("Dracula", year = Some(1931), runtime = Some(74), certificates = Some("US=G")), Some(3))
    certified(ranked(torrington, hammer, unrated, untold, besson)) shouldBe None
    val none    = (138, Film("Dracula", year = Some(1931), runtime = Some(74)), Some(3))
    certified(ranked(torrington, hammer, none, untold, besson)) shouldBe None
  }

  it should "take nothing where two namesakes stand" in {
    val reissue = (99999, Film("Dracula", year = Some(1979), runtime = Some(80), certificates = Some("GB=12A")), Some(5))
    certified(ranked(torrington, hammer, browning, untold, besson, reissue)) shouldBe None
  }

  it should "take nothing where the venue states no certificate, or its country has no scale" in {
    certified(ranked(torrington.copy(certificate = None), hammer, browning, untold, besson)) shouldBe None
    certified(ranked(torrington.copy(certificate = Some("TBC")), hammer, browning, untold, besson)) shouldBe None
  }
}
