package services.review

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.CatalogueId
import services.movies.ListingKey

import java.time.Instant

/** The venue's poster a review card shows: the first any venue source holds, never a site-wide default image. */
class VenuePosterSpec extends AnyFlatSpec with Matchers {

  private val listing = ListingKey.Native("Bielański Ośrodek Kultury", "https://biletyna.pl/x?eid=1", "Podróżniczek")
  private def slot(poster: Option[String]) = Some(SlotFacts(VenueFacts(poster = poster), Instant.EPOCH))
  private def feed(poster: Option[String]) = Some(ListingFeed(Nil, 1, None, None, poster))
  private def page(poster: Option[String]) = Some(VenueFacts(poster = poster))

  "a member's poster" should "be its slot's, else its listing's, else its venue page's" in {
    MemberView(listing, slot(Some("https://a/slot.jpg")), page(Some("https://a/page.jpg")), feed(Some("https://a/listing.jpg"))).poster shouldBe
      Some("https://a/slot.jpg")
    MemberView(listing, slot(None), page(Some("https://a/page.jpg")), feed(Some("https://biletyna.pl/file/get/id/414402"))).poster shouldBe
      Some("https://biletyna.pl/file/get/id/414402")
    MemberView(listing, None, page(Some("https://a/page.jpg")), feed(None)).poster shouldBe Some("https://a/page.jpg")
    MemberView(listing, None, None, None).poster shouldBe None
  }

  it should "pass over a site-wide default image for the next source's poster" in {
    MemberView(listing, slot(Some("https://kinoluna.bilety24.pl/wp-content/uploads/2023/12/PAN-BILET_warsztaty_svg.svg")), None,
      feed(Some("https://a/listing.jpg"))).poster shouldBe Some("https://a/listing.jpg")
    MemberView(listing, slot(Some("https://kino/kino_share.png")), None, None).poster shouldBe None
  }

  "a card's venue posters" should "list each distinct poster once, a venue's own before a feed catalogue's" in {
    val fed = MemberView(ListingKey.Published("Planken", "Queen", Some(2020), Nil), slot(Some("https://webedia/queen.jpg")), None,
      Some(ListingFeed(Seq(CatalogueId("webedia", "1")), 1, None, None)))
    val a = MemberView(ListingKey.Native("Muza", "https://muza/queen", "Queen"), None, None, feed(Some("https://muza/queen.jpg")))
    val b = MemberView(ListingKey.Native("Bajka", "https://bajka/queen", "Queen"), None, None, feed(Some("https://muza/queen.jpg")))
    val card = ReviewCard(ReviewCluster(models.Country.Germany, Seq(fed.key, a.key, b.key), None, 0.2,
      services.identity.ResolverDecision.Basis.BelowThreshold, Nil, fallback = false, Nil), Seq(fed, a, b), Map.empty, Nil, None, None)
    card.posters shouldBe Seq("https://muza/queen.jpg", "https://webedia/queen.jpg")
  }

  "the real prod sample" should "give a venue poster to every card some venue source has one for" in {
    val cards = ProdReviewSample.Databases.toSeq.flatMap { case (db, country) =>
      val source = ProdReviewSample.source(db)
      ReviewCards.build(source, source.decisions(unmatchedOnly = false).map(d => ReviewCluster.of(country, d) -> None), Nil,
        new ReviewAnswers.Index(Nil))
    }
    cards.size shouldBe 49
    // 19 had one when a card read its slot rows alone (2026-10-06)
    cards.count(_.posters.nonEmpty) shouldBe 42
  }
}
