package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.FamilyClosure.Edge
import services.identity.KnownCasePins._
import services.movies.{ListingConstraints, ListingKey}

/**
 * Pins as HARD constraints: what the resolver reads from the pin set through
 * `ListingConstraints.pinned`.
 */
class PinConstraintsSpec extends AnyFlatSpec with Matchers {

  private val pins = ListingConstraints.pinned(KnownCasePins.all)
  private val stranger = ListingKey.Published("Kino Amok", "Samson i Dalila", None, Nil)

  "a same-film pin" should "must-link every pinned listing, from the group's smallest key" in {
    val edges = pins.mustLinks.filter(e => lalka.contains(e.a))
    edges.map(e => Set(e.a, e.b)).toSet shouldBe lalka.sorted.tail.map(k => Set(lalka.min, k)).toSet
    edges.foreach { e => e.must shouldBe true; e.reason shouldBe ListingConstraints.MustLink.Pinned.toString }
  }

  it should "give its listings one block key, so the family closure keeps them in one family" in {
    val keys = lalka.map(k => k -> pins.blockKeys(k)).toMap
    val family = FamilyClosure.families(keys)
    lalka.map(family).distinct should have size 1
    FamilyClosure.crossings(family, pins.mustLinks.filter(e => lalka.contains(e.a))) shouldBe empty
  }

  it should "override a derived cannot-link inside the group, and nothing outside it" in {
    val derived = Edge(mockingjay2015, mockingjay2026, must = false, reason = "different bracketed years")
    pins.admits(derived) shouldBe false
    pins.admits(derived.copy(b = stranger)) shouldBe true
  }

  "a film pin" should "decide the listing's film over whatever its own lookup answered" in {
    pins.resolvedFilm(opetanie, looked = Some(999)) shouldBe Some(Possession1981)
    pins.resolvedFilm(stranger, looked = Some(999)) shouldBe Some(999)
    pins.blockKeys(opetanie) should contain(s"id:$Possession1981")
  }

  "a never-film pin" should "drop the denied answer, and cannot-link the listing from every listing that is that film" in {
    pins.resolvedFilm(metSamson, looked = Some(DeMilleSamson)) shouldBe None
    val filmOf = Map[ListingKey, Int](stranger -> DeMilleSamson)
    pins.cannotLinks(Seq(metSamson, stranger, opetanie), filmOf.get) shouldBe
      Seq(Edge(metSamson, stranger, must = false, reason = ListingConstraints.CannotLink.PinnedNotFilm.toString))
    // Sharing the denied film's block key keeps that cannot-link inside one family.
    pins.blockKeys(metSamson) should contain(s"id:$DeMilleSamson")
  }

  it should "refuse a derived must-link onto the denied film" in {
    val onto = ListingConstraints.pinned(KnownCasePins.all :+
      Pin(Seq(stranger), PinClaim.IsFilm(DeMilleSamson), "fixture", "the bare listing is the 1949 film", At))
    onto.admits(Edge(metSamson, stranger, must = true, reason = "same title")) shouldBe false
    onto.admits(Edge(opetanie, stranger, must = true, reason = "same title")) shouldBe false // two pinned films
  }

  "the pin set" should "report pins that contradict each other, and no conflict for the fixture" in {
    pins.conflicts shouldBe empty
    val contradicting = ListingConstraints.pinned(KnownCasePins.all ++ Seq(
      Pin(Seq(opetanie), PinClaim.NeverFilm(Possession1981), "fixture", "changed my mind", At),
      Pin(Seq(lalka.head), PinClaim.IsFilm(1), "fixture", "a", At),
      Pin(Seq(lalka.last), PinClaim.IsFilm(2), "fixture", "b", At)))
    contradicting.conflicts should have size 2
  }

  it should "be a function of the pin SET, not its order" in {
    ListingConstraints.pinned(KnownCasePins.all.reverse).mustLinks shouldBe pins.mustLinks
    ListingConstraints.pinned(KnownCasePins.all.reverse).blockKeys(lalka.last) shouldBe pins.blockKeys(lalka.last)
  }

  "a pin's id" should "be its content: the same claim on the same listings is the same pin" in {
    Pin(lalka.reverse, PinClaim.SameFilm, "someone else", "other words", At.plusSeconds(60)).id shouldBe lalkaPin.id
    Pin(lalka, PinClaim.IsFilm(1), "fixture", "x", At).id should not be lalkaPin.id
  }
}
