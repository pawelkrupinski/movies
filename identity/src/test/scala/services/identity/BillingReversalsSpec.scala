package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

class BillingReversalsSpec extends AnyFlatSpec with Matchers {

  private def reversal(venue: String, title: String) = TakeChange(ListingKey.Published(venue, title, None, Nil), s"film-$title", Some(2), Some(1))

  "billing reversals" should "count a projection's reversals once per billing, however many venues bill it" in {
    // UK "RBO Cinema Season 2026-27: Macbeth": one projection put 57 listings back to the Met's record at once
    val billings = new BillingReversals(capacity = 10)
    val noted = billings.note(Seq(reversal("Cineworld Bolton", "RBO Cinema Season 2026-27: Macbeth"),
      reversal("Cineworld Ely", "RBO Cinema Season 2026-27: Macbeth"), reversal("Kino Muza", "The Ties That Bind Us")))
    noted.reversed shouldBe Seq("rbo cinema season 2026 27 macbeth", "the ties that bind us")
    noted.forgotten shouldBe empty
  }

  it should "spell a billing alike whatever its case, accents or punctuation" in {
    val billings = new BillingReversals(capacity = 10)
    billings.note(Seq(reversal("Vue Swindon", "BTS WORLD TOUR 'ARIRANG' IN SÃO PAULO"))).reversed shouldBe Seq("bts world tour arirang in sao paulo")
    billings.note(Seq(reversal("Odeon Leeds", "BTS World Tour Arirang in Sao Paulo"))).reversed shouldBe Seq("bts world tour arirang in sao paulo")
  }

  it should "forget the billing that reversed least recently past its capacity, so its labels stay bounded" in {
    val billings = new BillingReversals(capacity = 2)
    billings.note(Seq(reversal("A", "One"), reversal("A", "Two")))
    billings.note(Seq(reversal("A", "One")))
    val third = billings.note(Seq(reversal("A", "Three")))
    third.reversed shouldBe Seq("three")
    third.forgotten shouldBe Seq("two")   // "one" reversed again since
  }

  it should "cut a long billing to a bounded label" in {
    val long = "x" * 300
    new BillingReversals().note(Seq(reversal("A", long))).reversed.head.length shouldBe BillingReversals.LabelLength
  }
}
