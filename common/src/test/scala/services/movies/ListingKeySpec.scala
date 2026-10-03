package services.movies

import models.{CinemaShowing, CineworldChain, Filmweb, Imdb, KinoMuranow, Multikino, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The stored form of a `ListingKey` — what `movie_slots.listingKey` and `screenings.listingKey`
 * hold — and the one derivation of a stored slot row's key. Uniqueness over the recorded corpora
 * is `ListingKeyCorpusSpec`'s; this is the form and the row derivation.
 */
class ListingKeySpec extends AnyFlatSpec with Matchers {

  private val native    = ListingKey.Native("Kino Muranów", "https://muranow.pl/film/belle", "Belle (2013)")
  private val published = ListingKey.Published("Arc Cinema", "Belle", Some(2021), Seq("Mamoru Hosoda", "Someone Else"))
  private val bare      = ListingKey.Published("Arc Cinema", "Belle", None, Seq.empty)

  "serialised" should "round-trip through parse for both shapes, absent year and no directors included" in {
    Seq(native, published, bare).foreach(k => ListingKey.parse(ListingKey.serialised(k)) shouldBe Some(k))
  }

  it should "tell apart keys that differ only in the field a naive join would lose" in {
    val forms = Seq(
      native,
      native.copy(rawTitle = "Belle (2021)"),
      published,
      published.copy(year = None),
      published.copy(directors = Seq("Mamoru Hosoda")),
      bare,
      ListingKey.Published("Arc Cinema", "Belle", None, Seq("")),
    ).map(ListingKey.serialised)
    forms.distinct should have size forms.size
  }

  "parse" should "refuse a string that is not a serialised key" in {
    ListingKey.parse("") shouldBe None
    ListingKey.parse("X\u0000a\u0000b") shouldBe None
    ListingKey.parse("N\u0000only-venue") shouldBe None
    ListingKey.parse("P\u0000v\u0000t\u0000not-a-year") shouldBe None
  }

  "ofSlotRow" should "key a per-title cinema slot by the venue and the slot's own listing fields" in {
    val slot = SourceData(title = Some("Belle"), rawTitle = Some("Belle (2013)"), releaseYear = Some(2013),
                          director = Seq("Amma Asante"))
    ListingKey.ofSlotRow(CinemaShowing(KinoMuranow, "belle").displayName, slot) shouldBe
      Some(ListingKey.Published(KinoMuranow.displayName, "Belle (2013)", Some(2013), Seq("Amma Asante")))
  }

  it should "key a legacy bare-cinema slot the same way, and prefer the page when there is one" in {
    val slot = SourceData(title = Some("Belle"), filmUrl = Some(" https://x/belle "))
    ListingKey.ofSlotRow(Multikino.displayName, slot) shouldBe
      Some(ListingKey.Native(Multikino.displayName, "https://x/belle", "Belle"))
  }

  it should "give no key to a slot that is not a venue's listing (an enrichment slot, a chain's detail slot, a retired venue)" in {
    ListingKey.ofSlotRow(Tmdb.displayName, SourceData(title = Some("Belle"))) shouldBe None
    // Measured on prod 2026-09-26: the Cineworld / Regal / Cinema City network slots carry no
    // title, so as "listings" every film's collapsed onto one key per chain and director.
    ListingKey.ofSlotRow(CineworldChain.displayName, SourceData(director = Seq("Christopher Nolan"))) shouldBe None
    ListingKey.ofSlotRow("Kino That Closed␟belle", SourceData(title = Some("Belle"))) shouldBe None
  }

  "isVenueRow" should "hold exactly for the wire keys ofSlotRow keys, whatever the slot holds" in {
    val slot = SourceData(title = Some("Belle"))
    val rows = Seq(CinemaShowing(KinoMuranow, "belle").displayName, Multikino.displayName, Tmdb.displayName,
                   CineworldChain.displayName, "Kino That Closed␟belle", Imdb.displayName, Filmweb.displayName)
    rows.map(r => r -> ListingKey.isVenueRow(r)) shouldBe rows.map(r => r -> ListingKey.ofSlotRow(r, slot).isDefined)
    rows.filter(ListingKey.isVenueRow) shouldBe rows.take(2)
  }

  // The order every projection and resolve sorts listings by, as the tuple it used to build per comparison gave it.
  "the listing order" should "be the (venue, kind, page, raw, year as text, directors joined by NUL) order, building nothing per comparison" in {
    def legacy(k: ListingKey) = k match {
      case ListingKey.Native(v, id, raw)          => (v, 0, id, raw, "", "")
      case ListingKey.Published(v, raw, year, ds) => (v, 1, "", raw, year.fold("")(_.toString), ds.mkString("\u0000"))
    }
    val venues = Seq("Helios", "Kino Muza")
    val keys = for {
      v <- venues
      k <- Seq[ListingKey](ListingKey.Native(v, "p/1", "Lalka"), ListingKey.Native(v, "p/1", "Diuna"), ListingKey.Native(v, "p/2", "A"),
        ListingKey.Published(v, "Lalka", None, Nil), ListingKey.Published(v, "Lalka", Some(2026), Nil), ListingKey.Published(v, "Lalka", Some(999), Nil),
        ListingKey.Published(v, "Lalka", Some(2026), Seq("Ab")), ListingKey.Published(v, "Lalka", Some(2026), Seq("Ab", "C")),
        ListingKey.Published(v, "Lalka", Some(2026), Seq("Abc")), ListingKey.Published(v, "", None, Nil))
    } yield k
    val rnd = new scala.util.Random(7)
    (1 to 20).foreach { _ =>
      val shuffled = rnd.shuffle(keys)
      shuffled.sorted shouldBe shuffled.sortBy(legacy)
    }
    // Allocation-free: a sort of many keys allocates the sort's own arrays, not an object per comparison.
    val many    = (1 to 20000).map(i => ListingKey.Published("Helios", s"Film ${i % 500}", Some(2000 + i % 30), Seq("A", s"B$i")))
    val threads = java.lang.management.ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    def allocated(sort: => Any): Long = { sort; val before = threads.getCurrentThreadAllocatedBytes; sort; threads.getCurrentThreadAllocatedBytes - before }
    val byTuple = allocated(many.sortBy(legacy))
    val byKey   = allocated(many.sorted)
    info(s"sorting ${many.size} keys allocated ${byKey / 1000} kB, by the tuple ${byTuple / 1000} kB")
    byKey should be < byTuple / 10
  }
}
