package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `strictNearest` chooses BETWEEN candidate films; `plausible` vetoes a single
 *  one. Prod, 2026-09-05: two rows were resolved to a film a fraction of the
 *  length every cinema advertised —
 *
 *    vivaldiija|2023  → an 18-minute "STABAT MATER RV621 … Jakub Józef Orliński"
 *                       concert short, while 46 venues screened the 110-minute
 *                       feature "Vivaldi i ja";
 *    homosapiens|1960 → Ion Popescu-Gopo's 9-minute animated short, while the
 *                       venues screened a 95-minute "Homo sapiens?".
 *
 *  Nothing checked. Title search matched, and the row then carried that film's
 *  year, poster and ratings — actively misleading, and worse than no metadata.
 */
class RuntimeCorroborationSpec extends AnyFlatSpec with Matchers {

  "plausible" should "reject a short matched to a feature the cinemas advertise" in {
    RuntimeCorroboration.plausible(Seq(110, 112), Some(18)) shouldBe false
    RuntimeCorroboration.plausible(Seq(95), Some(9))         shouldBe false
  }

  it should "reject a candidate far LONGER than anything the cinemas advertise" in {
    // The mirror case: a 4-hour cut matched to a 90-minute listing.
    RuntimeCorroboration.plausible(Seq(90), Some(240)) shouldBe false
  }

  // The band has to be wide enough to survive how cinemas actually publish
  // runtimes: padded with ads, rounded, or shaved. Multikino advertises the
  // 162-minute "Lalka" at 147, and a repertory house prints 121 for a 118-minute
  // print — those must all stay plausible.
  it should "accept the ordinary drift between a cinema's minutes and the film's" in {
    RuntimeCorroboration.plausible(Seq(147, 162), Some(162)) shouldBe true
    RuntimeCorroboration.plausible(Seq(121), Some(118))      shouldBe true
    RuntimeCorroboration.plausible(Seq(101), Some(123))      shouldBe true
    RuntimeCorroboration.plausible(Seq(140), Some(93))       shouldBe true
  }

  it should "abstain when either side published nothing" in {
    RuntimeCorroboration.plausible(Seq.empty, Some(18)) shouldBe true
    RuntimeCorroboration.plausible(Seq(110), None)      shouldBe true
    RuntimeCorroboration.plausible(Seq(0), Some(18))    shouldBe true
  }
}
