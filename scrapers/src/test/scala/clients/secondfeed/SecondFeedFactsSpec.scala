package clients.secondfeed

import clients.tools.{FakeHttpFetch, WriteSecondFeedFacts}
import models.{Cinema, CinemaMovie, GermanCinema}
import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{CineamoClient, CinemaScraper, KinoheldClient, ListingReads, SecondFeedFacts}
import services.identity.SourceReliability
import tools.HttpFetch

import java.io.IOException
import java.time.LocalDate

/** Replays four German venues' Cineamo and kinoheld.de programmes, both read on 2026-11-05 by [[WriteSecondFeedFacts]]
 *  (see it for why each venue is there): kinoheld's facts lent to the showings Cineamo bills bare. */
class SecondFeedFactsSpec extends AnyFlatSpec with Matchers with LoneElement {
  private val Today = LocalDate.of(2026, 11, 5)

  private def feeds(name: String, http: HttpFetch = new FakeHttpFetch(WriteSecondFeedFacts.Dir)): (CinemaScraper, CinemaScraper) = {
    val (_, cineamo, kinoheld) = WriteSecondFeedFacts.Venues.find(_._1 == name).getOrElse(fail(s"no recorded venue $name"))
    val cinema = new GermanCinema(name, name)
    (new CineamoClient(new FakeHttpFetch(WriteSecondFeedFacts.Dir), cineamo, cinema, today = Today),
     new KinoheldClient(http, kinoheld, cinema, today = Today))
  }

  private def scrape(name: String): Seq[CinemaMovie] = { val (primary, second) = feeds(name); new SecondFeedFacts(primary, second).fetch() }

  private def row(rows: Seq[CinemaMovie], title: String): CinemaMovie =
    rows.filter(_.movie.title == title).loneElement

  private lazy val borken  = scrape("Cinema Borken")
  private lazy val babylon = scrape("Babylon (Mitte)")

  "SecondFeedFacts" should "lend a bare Cineamo showing the facts kinoheld's show of the same screening states" in {
    val (primary, _) = feeds("Cinema Borken")
    val listed = row(primary.fetch(), "Bad Apples FK")
    SecondFeedFacts.bare(listed) shouldBe true

    val apples = row(borken, "Bad Apples FK")
    apples.movie.releaseYear shouldBe Some(2026)
    apples.movie.runtimeMinutes shouldBe Some(100)
    apples.director shouldBe Seq("Jonatan Etzler")
    apples.movie.countries shouldBe Seq("United Kingdom")
    apples.filmUrl shouldBe Some("https://www.kinoheld.de/film/bad-apples")
    // the venue's own title and showtimes stay Cineamo's
    apples.showtimes shouldBe listed.showtimes
  }

  // A feed's facts stay the feed's claim: kinoheld's page names them kinoheld's, trusted to support a take, never to
  // decide one alone — as on a venue kinoheld serves as primary. Without the page they would read as the venue's own.
  it should "carry kinoheld's page with them, so the facts stay kinoheld's claim" in {
    val apples = row(borken, "Bad Apples FK")
    (SourceReliability.factsOf(Nil, apples.filmUrl, pageFacts = false) eq SourceReliability.Kinoheld) shouldBe true
    SourceReliability.Kinoheld.decides(SourceReliability.Fact.Year) shouldBe false
  }

  // Cineamo's record name restating the venue's title in other letters is no fact (Neues Arena's "IN THE MOOD FOR LOVE",
  // unlinked in the recording of 2026-11-04, its record "In the Mood for Love"); kinoheld's entry, named with a year the
  // venue does not bill, is still the same billing.
  it should "lend to a showing whose original title only restates its title, and past a year the entry's name adds" in {
    val (primary, second) = feeds("Neues Arena")
    val listed = row(primary.fetch(), "IN THE MOOD FOR LOVE")
    listed.movie.originalTitle shouldBe Some("In the Mood for Love")
    val unlinked = listed.copy(externalIds = Map.empty)
    SecondFeedFacts.bare(unlinked) shouldBe true
    val mood = SecondFeedFacts.lend(unlinked, second.fetch())
    mood.director shouldBe Seq("Wong Kar-Wai")
    mood.movie.runtimeMinutes shouldBe Some(98)
    mood.movie.originalTitle shouldBe Some("Fa yeung nin wah")
    mood.filmUrl shouldBe Some("https://www.kinoheld.de/film/in-the-mood-for-love-2000")
  }

  it should "lend the running time a venue typed into its own kinoheld entry" in {
    row(babylon, "Cicle Gaudí: LA FURIA").movie.runtimeMinutes shouldBe Some(107)
    row(babylon, "City Lights live mit Babylon Orchester Berlin").movie.runtimeMinutes shouldBe Some(81)
  }

  it should "leave a showing that states facts of its own, or links a film, as Cineamo listed it" in {
    val (primary, second) = feeds("Babylon (Mitte)")
    val listed = primary.fetch()
    val lent   = new SecondFeedFacts(primary, second).fetch()
    listed.filterNot(SecondFeedFacts.bare) should not be empty
    lent.zip(listed).foreach { (after, before) => if (!SecondFeedFacts.bare(before)) after shouldBe before }
    lent.count(_.filmUrl.exists(_.startsWith(KinoheldClient.FilmPagePrefix))) should be > 0
  }

  // kinoheld links a show to a catalogue entry by guess. Universum-City Karlsruhe bills the 2021 "Lauras Stern"; kinoheld
  // links it to the 2009 "Lauras Stern und der geheimnisvolle Drache Nian" — that film's year and directors, not this one's.
  it should "lend nothing from an entry naming more than the venue bills" in {
    val (primary, second) = feeds("Universum-City Karlsruhe")
    val kinoheld = row(second.fetch(), "Lauras Stern und der geheimnisvolle Drache Nian")
    val stern    = row(primary.fetch(), "Lauras Stern")
    // Cineamo links this one; billed bare, the same screening is still not the entry's film
    val bareStern = stern.copy(externalIds = Map.empty, movie = stern.movie.copy(runtimeMinutes = None, releaseYear = None,
      originalTitle = None, releaseYearUpperBound = None))
    SecondFeedFacts.bare(bareStern) shouldBe true
    bareStern.showtimes.map(_.dateTime).toSet.intersect(kinoheld.showtimes.map(_.dateTime).toSet) should not be empty
    SecondFeedFacts.lend(bareStern, Seq(kinoheld)) shouldBe theSameInstanceAs(bareStern)
  }

  it should "lend nothing across dates: a show of the same title at another time is not this screening" in {
    val (primary, second) = feeds("Cinema Borken")
    val apples   = row(primary.fetch(), "Bad Apples FK")
    val kinoheld = second.fetch().filter(SecondFeedFacts.sameScreening(apples, _)).loneElement
    val elsewhen = kinoheld.copy(showtimes = kinoheld.showtimes.map(s => s.copy(dateTime = s.dateTime.plusDays(1))))
    SecondFeedFacts.lend(apples, Seq(elsewhen)) shouldBe theSameInstanceAs(apples)
  }

  it should "list Cineamo's programme as it came, complete, when kinoheld fails" in {
    val failing = new HttpFetch {
      def get(url: String): String = throw new IOException("kinoheld refused")
      override def post(url: String, body: String, contentType: String): String = throw new IOException("kinoheld refused")
    }
    val (primary, second) = feeds("Cinema Borken", failing)
    val (rows, reads) = ListingReads.during(new SecondFeedFacts(primary, second).fetch())
    rows shouldBe primary.fetch()
    reads.complete shouldBe true
  }

  it should "not read the second feed when Cineamo bills nothing bare" in {
    val (primary, _) = feeds("Cinema Borken")
    val linkedOnly = new CinemaScraper {
      val cinema: Cinema = primary.cinema
      def scrapeHosts: Set[String] = primary.scrapeHosts
      def fetch(): Seq[CinemaMovie] = primary.fetch().filterNot(SecondFeedFacts.bare)
    }
    val unread = new CinemaScraper {
      val cinema: Cinema = primary.cinema
      def scrapeHosts: Set[String] = Set.empty
      def fetch(): Seq[CinemaMovie] = fail("the second feed was read")
    }
    new SecondFeedFacts(linkedOnly, unread).fetch() shouldBe linkedOnly.fetch()
  }
}
