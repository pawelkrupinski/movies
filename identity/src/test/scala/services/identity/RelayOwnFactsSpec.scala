package services.identity

import models.{Helios, KinoMuza, KinoPalacowe, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer
import tools.IndependentCases

import FilmTable.{F, listing}

/**
 * A plain listing is denied an event-cinema relay's record when its OWN facts contradict that record (the owner,
 * 2026-11-01: the narrower fix after design B measured not landable). UK Chichester's bare "Prima Facie", whose own text
 * credits Susanna White (the 2026 film's director) and never Justin Martin (the National Theatre's 2022 recording's), took
 * the recording by its own match in the drift replay at uk seed 17 and then went back to the film: a flip.
 *
 * One direction only: a plain listing whose own text credits another candidate's director, and none of the relay's, never
 * takes the relay's record — whatever else is present, in whatever order. A relay listing is not held to the converse
 * (box offices bill relays bare, TMDB files relays under plain titles: design B lost ~1,000 right takes).
 */
class RelayOwnFactsSpec extends AnyFlatSpec with Matchers {
  private val normalizer  = TitleNormalizer.forCountry(models.Country.UnitedKingdom)
  private val calibration = IdentityCalibration.resolver

  private val Recording = 963568
  private val Film2026  = 1127282
  private val films = Seq(F(Recording, "National Theatre Live: Prima Facie", 2022, "Justin Martin", 108, 4.0),
    F(Film2026, "Prima Facie", 2026, "Susanna White", 106, 9.0), F(1089557, "Prima Facie", 2022, "Someone Else", 95, 1.0))
  private val lookups = new FilmTable(films, normalizer)

  /** Chichester's own text, as the recording holds it: it names the film's cast and director, and the play's star. */
  private val synopsis = "A young, ambitious trial lawyer (Cynthia Erivo) brilliantly navigates the ruthless London court system. " +
    "Unlike Suzie Miller's one-woman West End play of the same name starring Jodie Comer, this is an ensemble film, with " +
    "an ensemble cast which includes Simon Russell Beale. UK 2026 Susanna White 106m"
  private val chichester = listing(KinoMuza, "Prima Facie").copy(names = VenueNames.of(Seq(synopsis)))
  private val ntLive     = listing(Multikino, "NT Live: Prima Facie", Some(2022), Some("Justin Martin"), Some(108))
  private val bare       = Seq(listing(KinoPalacowe, "Prima Facie"), listing(Helios, "Prima Facie (15)", runtime = Some(109)))
  private val all        = chichester +: ntLive +: bare

  private def takeOf(listings: Seq[Listing], of: Listing): Option[Int] =
    IdentityResolver.resolveWith(listings, lookups, normalizer, calibration, IdentityResolver.Mutation.None).decisionOf(of.key).film

  "a bare listing whose own text credits the film's director" should "not take the relay's record alone" in {
    takeOf(Seq(chichester), chichester) should not be Some(Recording)
  }

  it should "never take it, whatever other listings are present, whole or incrementally in any order" in {
    val subsets = (1 to all.size).flatMap(all.combinations).filter(_.contains(chichester))
    IndependentCases.flatMap(subsets) { subset =>
      val whole = Option.when(takeOf(subset, chichester).contains(Recording))(s"whole {${subset.map(_.title).mkString(" / ")}}")
      whole.orElse(subset.permutations.flatMap { order =>
        val model = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None)
        order.iterator.zipWithIndex.flatMap { case (l, step) =>
          model.batch(seen = Seq(l))
          Option.when(model.decisions.find(_.listings(chichester.key)).flatMap(_.film).contains(Recording))(
            s"order ${order.map(_.title).mkString(" > ")}, step $step")
        }.nextOption()
      }.nextOption())
    } shouldBe empty
  }

  "a relay's record crediting nobody" should "be contradicted by no credit the venue's text names" in {
    // UK "Live from the Met: Parsifal" (recording 573): the Met's 2026/27 record credits nobody; the venue's text naming
    // another Parsifal's director lost it the record under the first form of the rule
    val met   = F(1703636, "The Metropolitan Opera 2026/27: Parsifal", 2027, "", 0, 2.0)
    val other = F(36848, "Parsifal", 1982, "Hans-Jürgen Syberberg", 255, 3.0)
    val dca   = listing(KinoMuza, "Live from the Met: Parsifal").copy(names = VenueNames.of(Seq("Unlike Hans-Jürgen Syberberg's film, live from New York")))
    RelayRecords.contradictedByOwnText(Seq(dca), new FilmTable(Seq(met), normalizer).film(met.id).answered.flatten.get,
      new FilmTable(Seq(other), normalizer).film(other.id).answered.toSeq.flatten) shouldBe false
  }

  "the relay's own listing" should "still take its record" in {
    takeOf(all, ntLive) shouldBe Some(Recording)
  }
}
