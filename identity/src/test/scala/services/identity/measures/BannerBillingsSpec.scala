package services.identity.measures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[StageRelays.billings]] and [[StageRelays.stageBilling]] read only the works a listing bills under a banner of its
 *  own ([[MeasuredListing.banneredWorks]]): a listing billing none bills nothing, and no film's works are read for it --
 *  a take-up bills every node against every film it reached (docs/design/corpus-context-persistence.md). Their answers
 *  are the frozen readings' below, which intersected every billed work of both sides. */
class BannerBillingsSpec extends AnyFlatSpec with Matchers {
  import StageRelays.Billing

  /** The readings before they read the bannered works only, frozen. */
  private object Frozen {
    private def bannerOf(title: Seq[String], work: Seq[String]): Option[Seq[String]] =
      Option.when(title.lengthIs > work.length)(
        if (title.endsWith(work)) Some(title.dropRight(work.length)) else if (title.startsWith(work)) Some(title.drop(work.length)) else None
      ).flatten
    private def billedUnder(listing: MeasuredListing, listingWork: Seq[String], film: Film, filmWork: Seq[String], work: String): Seq[Billing] =
      (for {
        listingHouse <- listing.billedTitles.flatMap(bannerOf(_, listingWork))
        filmHouse    <- film.billedTitles.flatMap(bannerOf(_, filmWork))
      } yield Billing(listingHouse, filmHouse, work))
        .sortBy(billed => (billed.listingHouse, billed.filmHouse, billed.listingWords.mkString(" "), billed.filmWords.mkString(" ")))
    def billings(listing: MeasuredListing, film: Film): Seq[Billing] = {
      val works = (listing.billedWorks intersect film.billedWorks.filterNot(StageRelays.broadcastLabel)).toSeq
        .sortBy(work => (-work.length, work.mkString(" ")))
      works.iterator.map(work => billedUnder(listing, work, film, work, work.mkString)).find(_.nonEmpty).getOrElse(Nil)
    }
    def stageBilling(listing: MeasuredListing, film: Film): Option[Billing] = {
      def staged(works: Set[Seq[String]]) = works.toSeq.flatMap(work =>
        StageWorks.resolver.named(IdentityMeasures.key(work.mkString(" "))).map(_ -> work))
      val filmWorks = staged(film.billedWorks).groupMap(_._1)(_._2)
      staged(listing.billedWorks).sortBy { case (id, work) => (-work.length, id, work.mkString(" ")) }.iterator
        .map { case (id, listingWork) => filmWorks.getOrElse(id, Nil).sortBy(_.mkString(" ")).flatMap(billedUnder(listing, listingWork, film, _, s"work:$id")) }
        .find(_.nonEmpty).flatMap(_.headOption)
    }
  }

  private val listings = Seq(
    "NT Live: Hamlet", "National Theatre Live: Dr. Strangelove", "Hamlet", "Hamlet (2026)", "NT LIVE: Golden Boy (15)",
    "Makbet | metropolitan opera: live in hd 2026/27", "The Metropolitan Opera 2026/27: Macbeth", "Royal Ballet & Opera: Carmen",
    "Carmen", "BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES: LIVE VIEWING", "André Rieu: Let It Snow", "Andre Rieu W KINIE: Maastricht",
    "Paris Opera Ballett: Romeo und Julia", "Romeo und Julia", "Kino Seniora: Lalka", "Lalka", "Hitchcock & Chabrol: Spellbound",
    "Spellbound", "Exhibition on Screen: Turner", "Turner").map(MeasuredListing(_))
  private val films = Seq(
    Film("National Theatre Live: Hamlet"), Film("Hamlet"), Film("NT Live: Dr. Strangelove", alternativeTitles = Seq("Dr. Strangelove")),
    Film("National Theatre Live: Golden Boy"), Film("The Metropolitan Opera: Macbeth", year = Some(2026)), Film("Macbeth"),
    Film("Royal Ballet and Opera: Carmen"), Film("Carmen"), Film("BTS World Tour 'Arirang' in Buenos Aires: Live Viewing"),
    Film("André Rieu's 2026 Christmas Concert: Let It Snow"), Film("Romeo und Julia"),
    Film("Romeo and Juliet", alternativeTitles = Seq("Romeo und Julia")), Film("Lalka"),
    Film("Spellbound", alternativeTitles = Seq("Alfred Hitchcock's Spellbound")), Film("Exhibition on Screen: Turner"), Film("Turner"))

  "a billing" should "read every listing against every film as the frozen reading did" in {
    for (l <- listings; f <- films) {
      withClue(s"${l.title} | ${f.title}: ") {
        StageRelays.billings(l, f) shouldBe Frozen.billings(l, f)
        StageRelays.stageBilling(l, f) shouldBe Frozen.stageBilling(l, f)
      }
    }
    // the cases bill something: a reading that billed nothing would agree with no frozen billing
    listings.flatMap(l => films.flatMap(f => StageRelays.billings(l, f))) should not be empty
  }

  it should "read no film's works for a listing that bills none under a banner" in {
    val plain = MeasuredListing("Hamlet")
    plain.banneredWorks shouldBe empty
    val film  = Film("National Theatre Live: Hamlet", alternativeTitles = (1 to 30).map(n => s"National Theatre Live $n: Hamlet"))
    StageRelays.billings(plain, film) shouldBe empty
    StageRelays.stageBilling(plain, film) shouldBe empty
    assume(tools.ThreadAllocation.supported, "this JVM cannot say what a thread allocates")
    // each billing against a fresh copy of the record, as the corpus context copies it for a billing pass
    def allocated(bill: (MeasuredListing, Film) => Any): Long = {
      (1 to 50).foreach(_ => bill(plain, film.copy()))
      tools.ThreadAllocation.of((1 to 500).foreach(_ => bill(plain, film.copy())))._2
    }
    val before = allocated(Frozen.billings)
    val now    = allocated(StageRelays.billings)
    info(s"500 billings of a bannerless listing allocated ${now / 1000} kB, the frozen reading ${before / 1000} kB")
    now should be < before / 10
  }
}
