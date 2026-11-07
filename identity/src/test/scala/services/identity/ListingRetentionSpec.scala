package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer

import scala.util.Random

/**
 * [[ListingRetention]] keeps a listing that left while a present listing of the country bills its title, read by its
 * NORMALIZED title alone (never by a take): the case it is billed in, a trailing year, an edition or format tag a venue
 * adds, none of them another title. DE Filmhaus Kino Koeln's "BLACK WATER (2025)" leaves while Kinocenter Giessen bills
 * "Black Water (GLOBALE)": kept by its sanitized title alone it went, and Giessen's take came back to a film it left
 * with the order its answers arrived in (drift slice kinocenter-giessen-black-water-globale, seed 29).
 */
class ListingRetentionSpec extends AnyFlatSpec with Matchers {

  private val normalizer = TitleNormalizer.forCountry(models.Country.Germany)
  private val base       = GeneratedIdentityCorpus.generate(1L, normalizer, films = 2, listings = 2).listings

  private def listing(n: Int, title: String, titles: TitleNormalizer = normalizer): Listing =
    base(n).copy(title = title, rawTitle = title, cleanTitle = services.movies.ScrapeListing.cleanTitle(base(n).cinema, title, titles)._1)

  /** The decorations a venue bills a title in: case, a trailing year, an edition or format tag. */
  private val Decorations: Seq[String => String] = Seq(identity, _.toUpperCase(java.util.Locale.ROOT), _ + " (2025)",
    _ + " (GLOBALE)", _ + " (OmU)", _ + " (OV)", _ + " 3D", _ + " - IMAX")

  private def kept(gone: String, present: String): Boolean = {
    ListingRetention.retained(Seq(listing(0, gone)), Seq(listing(1, present)), normalizer).map(_.key) == Seq(base(0).key)
  }

  "a listing that leaves" should "be kept while another venue bills its title: Koeln's \"BLACK WATER (2025)\" beside Giessen's \"Black Water (GLOBALE)\"" in {
    kept("BLACK WATER (2025)", "Black Water (GLOBALE)") shouldBe true
  }

  it should "be kept, on every seed, whatever case, year or edition tag either venue bills the title in" in {
    val titles = Seq("Black Water", "Die Ältern", "Der Spitzname", "One Battle After Another")
    val missed = (1 to 64).flatMap { seed =>
      val random  = new Random(seed)
      val title   = titles(random.nextInt(titles.size))
      val (g, p)  = (Decorations(random.nextInt(Decorations.size))(title), Decorations(random.nextInt(Decorations.size))(title))
      Option.unless(kept(g, p))(s"seed $seed: '$g' gone beside '$p'")
    }
    missed shouldBe empty
  }

  it should "be kept while a strand bills its title as a piece: Keighley's \"The Ladykillers (1955)\" beside " +
    "Nerve Centre's \"The Ladykillers | Cinematic Breakfast\" (drift slice nerve-centre-londonderry-the-ladykillers)" in {
    val uk   = TitleNormalizer.forCountry(models.Country.UnitedKingdom)
    ListingRetention.retained(Seq(listing(0, "The Ladykillers (1955)", uk)),
      Seq(listing(1, "The Ladykillers | Cinematic Breakfast", uk)), uk).map(_.key) shouldBe Seq(base(0).key)
  }

  // Prod DE, 2026-11-06: 947 kept listings beside their own live twin. SecondFeedFacts lends a bare Cineamo billing
  // kinoheld's page and year, so the same billing comes back under another key (Published -> Native, no year -> 2026), and
  // a venue's feed re-cases a title ("2M²" -> "2m²"). Kept by key alone, the old key stayed an input beside the new one
  // and split its family (BTS Jakarta at Olympia Schwarzenberg, Verify scrape recording #14).
  private def at(n: Int, raw: String, year: Option[Int], page: Option[String], venue: models.Cinema = base(0).cinema): Listing = {
    val l = listing(n, raw).copy(cinema = venue, year = year, page = page)
    l.copy(key = page.fold[services.movies.ListingKey](services.movies.ListingKey.Published(venue.displayName, raw, year, Nil))(
      url => services.movies.ListingKey.Native(venue.displayName, url, raw)))
  }

  "a listing that left" should "go when its own venue bills it again under another key: a lent page and year, or another case" in {
    val bare  = at(0, "BTS WORLD TOUR 'ARIRANG' IN JAKARTA: LIVE VIEWING", None, None)
    val lent  = at(1, "BTS WORLD TOUR 'ARIRANG' IN JAKARTA: LIVE VIEWING", Some(2026), Some("https://www.kinoheld.de/film/x"))
    val upper = at(0, "2M² - Wo ist dein Platz für die Ewigkeit?", None, None)
    val lower = at(1, "2m² - Wo ist dein Platz für die Ewigkeit?", Some(2026), None)
    ListingRetention.retained(Seq(bare), Seq(lent), normalizer) shouldBe empty
    ListingRetention.retained(Seq(lent), Seq(bare), normalizer) shouldBe empty
    ListingRetention.retained(Seq(upper), Seq(lower), normalizer) shouldBe empty
  }

  it should "still be kept beside another film its venue bills under the same title and another year " +
    "(Cinema-Arthouse's \"Sinn und Sinnlichkeit\": Ang Lee 1995 and Georgia Oakley 2026)" in {
    val lee    = at(0, "Sinn und Sinnlichkeit", Some(1995), None)
    val oakley = at(1, "Sinn und Sinnlichkeit", Some(2026), None)
    ListingRetention.retained(Seq(lee), Seq(oakley), normalizer).map(_.key) shouldBe Seq(lee.key)
  }

  it should "still be kept beside another venue billing it under the same title" in {
    val gone  = at(0, "Black Water", None, None)
    val other = at(1, "Black Water", Some(2026), None, venue = base(1).cinema)
    assume(base(0).cinema != base(1).cinema)
    ListingRetention.retained(Seq(gone), Seq(other), normalizer).map(_.key) shouldBe Seq(gone.key)
  }

  it should "go when no present listing bills its title" in {
    kept("BLACK WATER (2025)", "White Water (GLOBALE)") shouldBe false
  }

  "a listing that left" should "be kept again the moment a present listing bills its title, and not once none does: " +
    "read from the inputs, never from what was kept before" in {
    val departed = Seq(listing(0, "BLACK WATER (2025)"))
    ListingRetention.retained(departed, Seq(listing(1, "White Water")), normalizer) shouldBe empty
    ListingRetention.retained(departed, Seq(listing(1, "Black Water (GLOBALE)")), normalizer).map(_.key) shouldBe Seq(base(0).key)
    ListingRetention.retained(departed, Seq(listing(1, "White Water")), normalizer) shouldBe empty
  }

  it should "be marked kept: an input fact the resolve reads" in {
    ListingRetention.retained(Seq(listing(0, "Black Water")), Seq(listing(1, "Black Water")), normalizer).map(_.kept) shouldBe Seq(true)
  }

  it should "be neither kept nor dropped once it is billed again: its current data wins" in {
    ListingRetention.retained(Seq(listing(0, "Black Water")), Seq(listing(0, "Black Water"), listing(1, "Black Water")), normalizer) shouldBe empty
  }

  it should "be an input no longer past the retention window, whatever is billed: the store's bound is the departure's age alone" in {
    val left    = ListingRetention.Departed(listing(0, "Black Water"), at = 1000L)
    val present = Seq(listing(1, "Black Water"))
    def keptAt(now: Long) = ListingRetention.retainedAt(Seq(left), present, now, normalizer, TitleRelation.of(_, normalizer)).map(_.key)
    keptAt(1000L + ListingRetention.DefaultMaxAge.toMillis) shouldBe Seq(base(0).key)
    keptAt(1001L + ListingRetention.DefaultMaxAge.toMillis) shouldBe empty
  }
}
