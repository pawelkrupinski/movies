package services.identity

import models.KinoMuza
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.SourceReliability.{Fact, Trust}

/** The one model of how far each source of a listing's facts is trusted ([[SourceReliability]]): every source a rule
 *  for every fact, with its reason; each listing's facts recognised as the right source's. */
class SourceReliabilitySpec extends AnyFlatSpec with Matchers {
  import FilmTable.listing

  private val own = listing(KinoMuza, "To The Bone", year = Some(2014), director = Some("Erin Li"))

  "Every source" should "say how far each fact it states is trusted, and why" in {
    (SourceReliability.Venue +: SourceReliability.Others).foreach { source =>
      Fact.values.foreach(fact => withClue(s"${source.name} ${fact}") { source.rules(fact).why should not be empty })
    }
    Fact.values.foreach(SourceReliability.Venue.trust(_) shouldBe Trust.Decide)
  }

  it should "let no feed decide a film: its link and the facts it copies only support one, its poster and text are none" in {
    SourceReliability.Others.foreach { source =>
      withClue(source.name) {
        Seq(Fact.Link, Fact.Year, Fact.Directors, Fact.Runtime, Fact.OriginalTitle).map(source.trust) should contain only Trust.Support
        Seq(Fact.Poster, Fact.Names).map(source.trust) should contain only Trust.Ignore
      }
    }
  }

  "A listing's facts" should "be the source's that linked it — by its id, its page, or a listings site's entry read" in {
    SourceReliability.factsOf(own) shouldBe SourceReliability.Venue
    SourceReliability.factsOf(own.copy(catalogueIds = Seq(CatalogueId("webedia", "227420")))) shouldBe SourceReliability.Webedia
    SourceReliability.factsOf(own.copy(catalogueIds = Seq(CatalogueId(CatalogueSources.Cineamo, "9695m")))) shouldBe SourceReliability.Cineamo
    SourceReliability.factsOf(own.copy(page = Some("https://www.kinoprogramm.com/kinofilm/to-the-bone-1"))) shouldBe SourceReliability.Kinoprogramm
    SourceReliability.factsOf(own.copy(page = Some("https://www.kinoheld.de/film/to-the-bone"))) shouldBe SourceReliability.Kinoheld
    // a box office's own id, a ticketing platform's page: the venue's own
    SourceReliability.factsOf(own.copy(catalogueIds = Seq(CatalogueId("letterboxd", "to-the-bone")),
      page = Some("https://www.bilety24.pl/kino/776-crash"))) shouldBe SourceReliability.Venue
    // Flicks' film page: its facts once read off it, the day card's before
    val flicks = own.copy(page = Some("https://www.flicks.us/movie/to-the-bone/"))
    SourceReliability.factsOf(flicks) shouldBe SourceReliability.Venue
    SourceReliability.factsOf(flicks.copy(pageFacts = true)) shouldBe SourceReliability.Flicks
    SourceReliability.venueOnly(flicks) shouldBe false
    SourceReliability.venueOnly(own) shouldBe true
  }

  "A listing's claim" should "be one per origin: a feed entry's however many venues it lists, else the venue's own" in {
    val elsewhere = listing(models.Rialto, "To The Bone")
    val webedia   = Seq(CatalogueId("webedia", "227420"))
    SourceReliability.claimOf(own.copy(catalogueIds = webedia)) shouldBe SourceReliability.claimOf(elsewhere.copy(catalogueIds = webedia))
    SourceReliability.claimOf(own) shouldBe own.venue
    SourceReliability.claimOf(elsewhere) should not be SourceReliability.claimOf(own)
    // US Burns Court Sarasota and Film Streams Omaha read ONE Flicks page for "NT Live: Hamlet", its 2015 year
    // Cumberbatch's: one claim, not one per venue
    val hamlet = Some("https://www.flicks.us/movie/nt-live-hamlet/")
    SourceReliability.claimOf(own.copy(page = hamlet)) shouldBe SourceReliability.claimOf(elsewhere.copy(page = hamlet))
    SourceReliability.claimOf(own.copy(page = hamlet)) should not be
      SourceReliability.claimOf(own.copy(page = Some("https://www.flicks.us/movie/hamlet-2026/")))
  }

  "A feed's id" should "name its TMDB film when the feed is TMDB-keyed, and decide nothing alone" in {
    val linked = own.copy(catalogueIds = Seq(CatalogueId(CatalogueSources.Cineamo, "9695m")))
    SourceReliability.tmdbLinked(linked) shouldBe Some(CatalogueId(CatalogueSources.Cineamo, "9695m") -> 9695)
    SourceReliability.tmdbLinked(own.copy(catalogueIds = Seq(CatalogueId("webedia", "227420")))) shouldBe None
    SourceReliability.linkDecides(CatalogueId("webedia", "227420")) shouldBe false
    SourceReliability.linkDecides(CatalogueId("letterboxd", "to-the-bone")) shouldBe true
  }

  "A listing stated at one trust" should "keep the facts its source states so, and only those" in {
    val fed = own.copy(catalogueIds = Seq(CatalogueId("webedia", "227420")), runtime = Some(8))
    SourceReliability.stated(own, Trust.Decide) should be theSameInstanceAs own
    val decided = SourceReliability.stated(fed, Trust.Decide)
    (decided.year, decided.directors, decided.runtime) shouldBe ((None, Nil, None))
    SourceReliability.stated(fed, Trust.Support) should be theSameInstanceAs fed
  }
}
