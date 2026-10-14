package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `scripts/venue-replay` on the scraper cases of 2026-10-14, each from what was already recorded — the recording's own
 * row for the venue (run 38498856157, cut to the listings that matter), the detail pages it holds, and the client fixture
 * trees checked in with each fix. The before side of each fix is the two-ref run its comment names.
 */
class VenueReplaySpec extends AnyFlatSpec with Matchers {

  private val Cases = "test/resources/fixtures/venue-replay"

  private def replay(args: String*): VenueReplay.Outcome = VenueReplay.run(VenueReplay.parse(args))
  private def listing(o: VenueReplay.Outcome, raw: String): Map[String, String] =
    o.rows.map(_.toMap).find(_("raw") == raw).getOrElse(fail(s"no listing '$raw' in:\n${o.text}"))
  private def recorded(name: String, cc: String): Seq[String] =
    Seq(cc, "--corpus", s"$Cases/$name/corpus/cinema-scrapes-$cc.json.gz", "--recording", s"$Cases/$name")

  // d880f62cc~1 vs d880f62cc: `scripts/venue-replay pl "ziemi rybnickiej" --recording 38498856157 --title trzy
  //   --fixtures test/resources/fixtures/teatr-ziemi-rybnickiej --ref d880f62cc~1 --ref d880f62cc` — the synopsis moves
  //   from the "Scenariusz:" credit (synopsis-is-credits) to the prose, and the runtime from none to 120.
  "a recorded row" should "be read again through the client's own detail parse, naming what the page moved" in {
    val o = replay(recorded("teatr-ziemi-rybnickiej", "pl") ++
      Seq("--venue", "ziemi rybnickiej", "--fixtures", "test/resources/fixtures/teatr-ziemi-rybnickiej")*)
    val trzy = listing(o, "Trzy pożegnania")
    trzy("synopsis") should startWith("Po rozstaniu z ukochanym Marta")
    trzy("runtime") shouldBe "120"
    trzy("recorded") should include("synopsis was Scenariusz: Enrico Audenino")
    trzy("checks") shouldBe "-"
    o.misses shouldBe empty
  }

  it should "fail by name on a request no recorded tree holds" in {
    val o = replay(recorded("teatr-ziemi-rybnickiej", "pl") ++ Seq("--venue", "ziemi rybnickiej")*)
    o.misses shouldBe Seq("GET https://www.teatrziemirybnickiej.pl/wydarzenia/trzy-pozegnania.html")
    listing(o, "Trzy pożegnania")("detail") should startWith("detail page failed")
  }

  it should "hold every listing to the corpus-shape checks, and name the title-trace line for a title a rule moved" in {
    val o = replay(recorded("astor-hafencity", "de") ++ Seq("--venue", "astor film lounge hafencity")*)
    // Cineamo billed the screen format in front of the title (808ed55a7 peels it): this code reads it off as a format
    val michael = listing(o, "ATMOS: Michael")
    michael("clean") shouldBe "Michael"
    michael("formats") should include("ATMOS")
    michael("trace") shouldBe "scripts/title-trace 'ATMOS: Michael' --country de --venue 'Astor Film Lounge HafenCity Hamburg'"
    // the venue's box office cuts names at 60 characters: CinemaSlotInvariantsSpec names it (and allowlists it)
    listing(o, "Lieblingsmenschen - Die außergewöhnliche  Freundschaft von A")("checks") should
      include("CinemaSlotInvariants title-ends-mid-phrase")
  }

  // afa874093~1 vs afa874093: `scripts/venue-replay de filmwerk --scrape --fixtures test/resources/fixtures/kinoheld
  //   --date 2026-10-11 --title sneak --ref afa874093~1 --ref afa874093` — "Sneak Over" (the 8-minute short kinoheld
  //   linked November's sneak to, SearchQueryMarkers' "sneak") only at the parent.
  "--scrape" should "run the venue's scraper over a client's recorded tree" in {
    val o = replay("de", "--venue", "filmwerk", "--scrape", "--fixtures", "test/resources/fixtures/kinoheld", "--date", "2026-10-11")
    o.misses shouldBe empty
    o.rows should not be empty
    o.rows.map(row => row.toMap.apply("raw")) should not contain "Sneak Over"
    o.text should include("scraped again by KinoheldClient on 2026-10-11")
  }

  "a detail-enricher venue" should "re-read every detail page from the recording's tree" in {
    val o = replay(recorded("cineworld-barnsley", "uk") ++ Seq("--venue", "cineworld barnsley")*)
    o.misses shouldBe empty
    o.rows.map(row => row.toMap.apply("detail")).distinct shouldBe Seq("detail page re-read")
    val michael = listing(o, "(4DX Rewind) Michael")
    michael("clean") shouldBe "(4DX Rewind) Michael"
    michael("search") shouldBe "Michael"
    michael("director") shouldBe "Antoine Fuqua"
    michael("runtime") shouldBe "128"
  }

  "--prod" should "show the venue's current prod row from the mirror's export, read by the same code" in {
    val o = replay(recorded("teatr-ziemi-rybnickiej", "pl") ++ Seq("--venue", "ziemi rybnickiej", "--title", "trzy",
      "--fixtures", "test/resources/fixtures/teatr-ziemi-rybnickiej", "--prod", s"$Cases/teatr-ziemi-rybnickiej/prod-mirror.json")*)
    val prod = o.text.substring(o.text.indexOf("== PROD now"))
    prod should include("the mirror's cinema_scrapes (scraped 2026-10-14T10:24:53.097Z)")
    prod should include("Po rozstaniu z ukochanym Marta")
    prod should include("identity_listings: the same 1 listing(s)")
  }

  "a venue selector" should "take an exact name over the names containing it, and refuse an ambiguous one" in {
    VenueReplay.pickVenue("Kino Luna", Seq("Kino Luna", "Kino Lunapark")) shouldBe Right("Kino Luna")
    VenueReplay.pickVenue("żerań", Seq("Kino Zerań")) shouldBe Right("Kino Zerań")
    VenueReplay.pickVenue("luna", Seq("Kino Luna", "Kino Lunapark")).left.toOption.get should startWith("'luna' matches 2 venues")
  }
}
