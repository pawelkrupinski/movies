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
    val left = ListingRetention.leave(Seq(listing(0, gone)), Seq(listing(1, present)), Nil, normalizer)
    left.kept.map(_.key) == Seq(base(0).key)
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
    val left = ListingRetention.leave(Seq(listing(0, "The Ladykillers (1955)", uk)),
      Seq(listing(1, "The Ladykillers | Cinematic Breakfast", uk)), Nil, uk)
    left.kept.map(_.key) shouldBe Seq(base(0).key)
  }

  it should "go when no present listing bills its title" in {
    kept("BLACK WATER (2025)", "White Water (GLOBALE)") shouldBe false
  }

  "a listing kept before" should "be kept again by a restore while a present listing bills its title, and dropped once none does" in {
    val stored = Seq(listing(0, "BLACK WATER (2025)"))
    ListingRetention.restored(stored, Seq(listing(1, "Black Water (GLOBALE)")), normalizer).kept.map(_.key) shouldBe Seq(base(0).key)
    val gone = ListingRetention.restored(stored, Seq(listing(1, "White Water")), normalizer)
    (gone.kept, gone.dropped) shouldBe ((Nil, Seq(base(0).key)))
  }

  it should "be neither kept nor dropped once it is billed again: its current data wins" in {
    val back = ListingRetention.restored(Seq(listing(0, "Black Water")), Seq(listing(0, "Black Water"), listing(1, "Black Water")), normalizer)
    (back.kept, back.dropped) shouldBe ((Nil, Nil))
  }
}
