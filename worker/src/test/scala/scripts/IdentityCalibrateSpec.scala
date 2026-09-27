package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityMeasures
import services.identity.IdentityMeasures.{Category, Film, Listing}

/** The calibration's LABEL rule — what a listing's own evidence says about production's proposal —
 *  and how a signal's table is fitted from the labels. */
class IdentityCalibrateSpec extends AnyFlatSpec with Matchers {

  private def m(l: Listing, f: Film) = IdentityMeasures.listingFilm(l, f, None, 0, 0)

  "a proposal" should "be denied by the title when the listing names ANOTHER film of its own search more closely" in {
    // Cinesa's "Vengadores: Endgame (Reestreno)", 2026, the Russos: filed under Avengers: Doomsday
    // (2026, the Russos) — year and director agree, but the title names Endgame, which its own
    // search returned.
    val listing  = Listing("Vengadores: Endgame (Reestreno)", year = Some(2026), directors = Seq("Anthony Russo", "Joe Russo"))
    val doomsday = Film("Vengadores: Doomsday", year = Some(2026), directors = Some(Seq("Anthony Russo", "Joe Russo")))
    val endgame  = Film("Vengadores: Endgame", year = Some(2019), directors = Some(Seq("Anthony Russo", "Joe Russo")))
    val (_, deny) = IdentityCalibrate.proposalAgreement(m(listing, doomsday), Seq(m(listing, endgame)))
    deny should contain ("title")
  }

  it should "not be denied by the title when no other film of its search is named more closely" in {
    // A truncated spelling names its film only by shared words — and nothing names another film better.
    val listing = Listing("Niesamowite przygody skarpetek 3. Ale ko", year = Some(2026))
    val film    = Film("Niesamowite przygody skarpetek 3. Ale kosmos!", year = Some(2026))
    val (agree, deny) = IdentityCalibrate.proposalAgreement(m(listing, film), Seq(m(listing, Film("Skarpety", year = Some(2020)))))
    deny shouldBe empty
    agree should contain ("year")
  }

  "the fitted signals" should "be every measure a pair emits, but those kept out for a stated reason" in {
    // A measure the resolver computes but the calibration never fits weighs nothing: the bracket
    // year of "Scary Movie (1991)" and a broadcast's season would be read and ignored.
    val lf = m(Listing("Scary Movie (1991)"), Film("Scary Movie", year = Some(1991))).keySet
    val ll = IdentityMeasures.listingListing(Listing("A"), Listing("A"), sameVenue = false, sharedChainId = None).keySet
    IdentityCalibrate.LfSignals.toSet ++ IdentityCalibrate.Unweighted.keySet shouldBe lf
    IdentityCalibrate.LlSignals.toSet ++ IdentityCalibrate.Unweighted.keySet.intersect(ll) shouldBe ll
  }

  /** `same` same-film and `different` different-film units measuring `title` as `category`. */
  private def units(category: String, same: Int, different: Int): Seq[IdentityCalibrate.Row] =
    (0 until same + different).map { i =>
      val y = i < same
      IdentityCalibrate.Row("train", s"$category-$i", s"$category-$i", "us", Map("title" -> Category(category)), _ => Some(y))
    }

  "a title table" should "never weigh a title naming more of the film below one naming less" in {
    // The round-2 fit: decorations ("Ken Russell's The Devils") were few and mostly hard negatives,
    // so alone they weighed below a mere shared word and even below no shared word at all.
    val rows = units("exact", 300, 60) ++ units("decorated", 2, 110) ++ units("overlap", 4, 93) ++ units("none", 1, 25)
    val w = IdentityCalibrate.fitSignal("title", rows, _ => Set.empty).weights.categories
    w("decorated") should be >= w("overlap")
    w("overlap") should be >= w("none")
    w("exact") should be > w("decorated")
  }

  it should "pool only the categories that violate the order, from their summed counts" in {
    val rows = units("exact", 300, 60) ++ units("decorated", 2, 110) ++ units("overlap", 4, 93) ++ units("none", 1, 25)
    val t = IdentityCalibrate.fitSignal("title", rows, _ => Set.empty)
    val pooled = IdentityCalibrate.llr(2 + 4 + 1, 110 + 93 + 25, t.positives, t.negatives, 4)
    t.weights.categories("decorated") shouldBe pooled +- 1e-9
    t.weights.categories("none") shouldBe pooled +- 1e-9
    t.weights.categories("exact") shouldBe IdentityCalibrate.llr(300, 60, t.positives, t.negatives, 4) +- 1e-9
    t.weights.counts("decorated") shouldBe Seq(2, 110) // the table still reports each category's own counts
  }

  it should "keep a fit that already respects the order untouched" in {
    val rows = units("decorated", 20, 10) ++ units("overlap", 5, 50) ++ units("none", 1, 40)
    val t = IdentityCalibrate.fitSignal("title", rows, _ => Set.empty)
    t.weights.categories("decorated") shouldBe IdentityCalibrate.llr(20, 10, t.positives, t.negatives, 3) +- 1e-9
    t.weights.categories("overlap") shouldBe IdentityCalibrate.llr(5, 50, t.positives, t.negatives, 3) +- 1e-9
  }

  private def numbers(signal: String, value: Double, same: Int, different: Int): Seq[IdentityCalibrate.Row] =
    (0 until same + different).map { i =>
      val u = s"$signal-$value-$i"
      IdentityCalibrate.Row("train", u, u, "pl", Map(signal -> IdentityMeasures.Number(value)), _ => Some(i < same))
    }

  "a numeric table" should "never weigh more corroborating venues below fewer" in {
    // r5: 0 same-film and 4 different-film units at 153-155 venues fitted a bin of -0.42 between
    // +6.21 and +4.72, and CI recording 36342710818's bare "Lalka" (2026) took the French film
    // backed by 108 venues over Kawalski's backed by 153.
    val rows = numbers("venues.corroborating", 0, 995, 23410) ++ numbers("venues.corroborating", 40, 211, 2) ++
      numbers("venues.corroborating", 154, 0, 4) ++ numbers("venues.corroborating", 170, 9, 0)
    val t = IdentityCalibrate.fitSignal("venues.corroborating", rows, _ => Set.empty)
    val ws = t.weights.bins.map(_.weight)
    withClue(t.weights.bins.mkString("\n")) {
      ws.sliding(2).foreach { case Seq(a, b) => b should be >= a; case _ => }
      t.weights.weight(Some(IdentityMeasures.Number(153))) should be >= t.weights.weight(Some(IdentityMeasures.Number(108)))
    }
  }

  it should "never weigh a lower search rank above a higher one, nor a longer runtime gap above a shorter" in {
    val rank = IdentityCalibrate.fitSignal("search.rank",
      numbers("search.rank", 1, 500, 50) ++ numbers("search.rank", 5, 20, 200) ++ numbers("search.rank", 12, 0, 300) ++
        numbers("search.rank", 20, 6, 40), _ => Set.empty).weights.bins.map(_.weight)
    rank.sliding(2).foreach { case Seq(a, b) => b should be <= a; case _ => }
    val runtime = IdentityCalibrate.fitSignal("runtime.delta",
      numbers("runtime.delta", 0, 800, 40) ++ numbers("runtime.delta", 15, 30, 120) ++ numbers("runtime.delta", 40, 0, 90) ++
        numbers("runtime.delta", 60, 12, 30), _ => Set.empty).weights.bins.map(_.weight)
    runtime.sliding(2).foreach { case Seq(a, b) => b should be <= a; case _ => }
  }

  it should "leave a numeric signal with no stated direction to the data" in {
    // A year difference is signed: evidence peaks at 0 and falls both ways.
    val ws = IdentityCalibrate.fitSignal("year.delta",
      numbers("year.delta", -5, 2, 200) ++ numbers("year.delta", 0, 900, 50) ++ numbers("year.delta", 5, 3, 200), _ => Set.empty)
      .weights.bins.map(_.weight)
    ws should have size 3
    ws(1) should be > ws(0)
    ws(1) should be > ws(2)
  }
}
