package services.identity

import models.{CinemaMovie, KinoApollo, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

import java.time.LocalDateTime

/** A listing searches the title the venue's rules ask external lookups for (`TitleNormalizer.apiQuery`) —
 *  what the old pipeline searched — while what relates it to other listings stays the venue's own title. */
class ListingSearchTitleSpec extends AnyFlatSpec with Matchers {

  private def listing(title: String): Listing =
    Listing.of(KinoApollo, CinemaMovie(Movie(title), KinoApollo, None, None, None, Nil, Nil,
      Seq(Showtime(LocalDateTime.of(2099, 3, 1, 18, 0), None))), SingleCountryNormalizer.titleNormalizer)

  "A programme's listing" should "search its film's title without the programme and the accessibility tags" in {
    // Kino Pałacowe's "Kino bez barier: Lalka (AD + CC + PJM) + spotkanie z twórcami": the old pipeline asked
    // TMDB for "Lalka"; the resolver's own shapes never cut "+ spotkanie…" off "Lalka (AD + CC + PJM)".
    val l = listing("Kino bez barier: Lalka (AD + CC + PJM) + spotkanie z twórcami")
    l.searchTitle shouldBe Some("Lalka (AD + CC + PJM)")
    Evidence.of(l, None).measured.shapes should contain ("Lalka")
    // Relating it to other listings reads the venue's title alone: the card stays the programme's own.
    Evidence.of(l, None).published.searchTitles shouldBe empty
  }

  "A screening's title" should "search the film it shows, however the venue joins the extras to it" in {
    // PL corpus, 2026-10-02: each found no candidate, or the film's title only with what the venue bolted on.
    def search(title: String) = SingleCountryNormalizer.titleNormalizer.searchQuery(title)
    // a film announced after a "+" survives the "+ <event>" strip, which used to take the film with it
    search("Spotkanie z Kamilą Urzędowską + pokaz filmu \"Lalka\"") shouldBe "Lalka"
    // accessibility subtitles, joined with or without a spaced hyphen
    search("Lalka-napisy dla niesłyszących") shouldBe "Lalka"
    search("Lalka - napisy dla niesłyszących") shouldBe "Lalka"
    // a bracket only a truncated title leaves open
    search("Lalka (+ ENG") shouldBe "Lalka"
    // a closed bracket and a plain "+ <event>" are as before
    search("Hamnet + pokaz filmu") shouldBe "Hamnet"
    // "pokaz filmu" followed by what the screening comes WITH, not a film's name:
    // the film is the one before it, never "z prelekcją"
    search("Lalka – pokaz filmu z prelekcją") shouldBe "Lalka"
    search("Lalka - pokaz filmu i spotkanie z reżyserem") shouldBe "Lalka"
    // an unquoted capitalised film after it is still the film
    search("Klub Filmowy: pokaz filmu Mira") shouldBe "Mira"
  }

  "A broadcaster's banner under one of its short names" should "search the name the broadcaster files its records under" in {
    // US Burns Court / Film Streams "NT Live: Hamlet": TMDB holds the National Theatre's 2026 Hamlet (1507580) as
    // "National Theatre Live: Hamlet", which neither "NT Live: Hamlet" nor "Hamlet" reaches. The canonical tier's
    // proper-name rule (`xtra-canonical-nt-live`) spells it so; the search asks that spelling too.
    val l = listing("NT Live: Hamlet")
    l.searchTitle shouldBe Some("National Theatre Live: Hamlet")
    IdentityMeasures.searchQueries(Evidence.of(l, None).measured) should contain allOf ("National Theatre Live: Hamlet", "NT Live: Hamlet")
    // the venue's dash after the banner is no part of the work (UK Keswick Alhambra's "NT Live - Hayfever")
    listing("NT Live - Hayfever").searchTitle shouldBe Some("National Theatre Live: Hayfever")
    // a strip the search asks still wins: the venue's programme off is the search title it always was
    listing("Kino bez barier: Lalka (AD + CC + PJM) + spotkanie z twórcami").searchTitle shouldBe Some("Lalka (AD + CC + PJM)")
    listing("Kino bez barier: Lalka (AD + CC + PJM) + spotkanie z twórcami").properTitle shouldBe None
    listing("NT Live: Hamlet").properTitle shouldBe None
  }

  it should "search the proper name of the query its rules ask too, the event bolted on after it off" in {
    // "+ Q&A" off, the cleaned query still bills the banner by its short name: both are searched
    val l = listing("NT Live: Hamlet + Q&A")
    (l.searchTitle, l.properTitle) shouldBe ((Some("NT Live: Hamlet"), Some("National Theatre Live: Hamlet")))
    IdentityMeasures.searchQueries(Evidence.of(l, None).measured) should contain allOf ("National Theatre Live: Hamlet", "NT Live: Hamlet")
  }

  "A plain title" should "have no search title of its own" in {
    listing("Lalka").searchTitle shouldBe None
  }
}
