package services.identity

import models.{KinoMikro, Rialto}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

/** Two programme spellings of one film, on the PRODUCTION calibration: its learned listing-listing cut is what
 *  kept them apart. PL Kino Piast's "Egon Schiele. Poza tabu | FKS" and Rialto's "Wielka Sztuka w Kinoteatrze
 *  Rialto - Egon Schiele. Poza tabu" both search as "Egon Schiele. Poza tabu"; compared as decorated titles
 *  they only overlapped. */
class ProgrammeSpellingsSpec extends AnyFlatSpec with Matchers {
  import FilmTable.{F, listing}
  private val normalizer = SingleCountryNormalizer.titleNormalizer

  "Two programme spellings sharing a search form" should "not be kept apart for their programmes" in {
    val films  = Seq(F(1657234, "Tabù - Egon Schiele", 2026, "Michele Mally", 90))
    // Their pages give 93 and 90 minutes: as decorated titles that overlap, three minutes apart cut them.
    val piast  = listing(KinoMikro, "Egon Schiele. Poza tabu | FKS", runtime = Some(93))
    val rialto = listing(Rialto, "Wielka Sztuka w Kinoteatrze Rialto - Egon Schiele. Poza tabu", runtime = Some(90))
    val r = IdentityResolver.resolve(Seq(piast, rialto), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    withClue(r.decisionOf(piast.key).render)(r.decisionOf(piast.key) shouldBe theSameInstanceAs(r.decisionOf(rialto.key)))
  }
}
