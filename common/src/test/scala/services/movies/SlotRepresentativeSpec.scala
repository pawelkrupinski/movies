package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime
import scala.util.Random

/**
 * Which same-slot listing speaks for a cinema's slot.
 *
 * Modelled on a real shape the generated corpus cannot produce and the production
 * archive does: Kino Kultura lists "Ghost in the Shell" as two screening series,
 * neither carrying a detail link, each with its own poster. Both collapse to one
 * slot key, so one has to win — and which one must not depend on the order the
 * scraper emitted them in.
 */
class SlotRepresentativeSpec extends AnyFlatSpec with Matchers {

  private def listing(poster: Option[String], filmUrl: Option[String] = None, title: String = "Ghost in the Shell",
                      synopsis: Option[String] = None, cast: Seq[String] = Nil) =
    CinemaMovie(
      movie     = Movie(title, None, None, Nil, Nil, None, None),
      cinema    = Cinema.all.head,
      posterUrl = poster,
      filmUrl   = filmUrl,
      synopsis  = synopsis,
      cast      = cast,
      director  = Nil,
      showtimes = Seq(Showtime(LocalDateTime.parse("2026-03-22T18:00"), bookingUrl = None))
    )

  // The regression. Before the poster joined the rank, `(filmUrl, title)` tied for
  // both of these and `minBy` fell through to input order.
  "the slot representative" should "not depend on the order the venue's listings arrived in" in {
    val first  = listing(Some("https://kinokultura.pl/foto,20677,117444d5,jpg.html"))
    val second = listing(Some("https://kinokultura.pl/foto,20729,46b3c104,jpg.html"))

    val forwards  = MovieRecordMerge.slotRepresentative(Seq(first, second))
    val backwards = MovieRecordMerge.slotRepresentative(Seq(second, first))

    forwards shouldBe backwards
  }

  it should "survive any shuffle of a larger same-slot group" in {
    val group = (1 to 6).map(i => listing(Some(s"https://kinokultura.pl/foto,$i,jpg.html")))
    val random = new Random(0x5107L)
    val picks  = (1 to 25).map(_ => MovieRecordMerge.slotRepresentative(random.shuffle(group.toList)))

    picks.distinct should have size 1
  }

  // A slot that renders no image at all because a poster-less duplicate happened to
  // sort first is a visible regression, not just a determinism one.
  it should "prefer a listing that has a poster over one that has none" in {
    val withPoster = listing(Some("https://kinokultura.pl/foto,20677,117444d5,jpg.html"))
    val without    = listing(None)

    MovieRecordMerge.slotRepresentative(Seq(without, withPoster)).posterUrl shouldBe withPoster.posterUrl
    MovieRecordMerge.slotRepresentative(Seq(withPoster, without)).posterUrl shouldBe withPoster.posterUrl
  }

  // filmUrl outranks the poster, so a slot's detail link is decided first.
  it should "let filmUrl decide before the poster does" in {
    val linkedDullPoster = listing(Some("https://kinokultura.pl/z.jpg"), filmUrl = Some("https://kinokultura.pl/a"))
    val unlinked         = listing(Some("https://kinokultura.pl/a.jpg"))

    MovieRecordMerge.slotRepresentative(Seq(linkedDullPoster, unlinked)) shouldBe linkedDullPoster
  }

  // The counterpart of the poster rule, and it costs more: a slot that keeps the
  // link-less duplicate renders the cinema as plain text instead of a deep link,
  // AND is never detail-enriched — DetailReaper has no page to fetch — so the film
  // silently loses its synopsis and cast.
  it should "prefer a listing that has a detail link over one that has none" in {
    val linked   = listing(None, filmUrl = Some("https://kinokultura.pl/ghost-in-the-shell"))
    val unlinked = listing(None)

    MovieRecordMerge.slotRepresentative(Seq(unlinked, linked)).filmUrl shouldBe linked.filmUrl
    MovieRecordMerge.slotRepresentative(Seq(linked, unlinked)).filmUrl shouldBe linked.filmUrl
  }

  // The convergence suite found this one against KinoGram's duplicated "Spider-Man":
  // one listing carried a 242-character synopsis, the other none, and the winner was
  // whichever arrived first. Longest wins, as `mergeRetainedSynopses` already does.
  it should "keep the longest synopsis among same-slot listings" in {
    val blurbed = listing(None, synopsis = Some("A" * 242))
    val bare    = listing(None)

    MovieRecordMerge.slotRepresentative(Seq(bare, blurbed)).synopsis shouldBe blurbed.synopsis
    MovieRecordMerge.slotRepresentative(Seq(blurbed, bare)).synopsis shouldBe blurbed.synopsis
  }

  // The point of the total tie-break. This rank was extended twice — for the poster,
  // then the detail link — and each time the next un-enumerated field silently took
  // over as the one decided by arrival order. A field nobody ranked must still not
  // be able to do that.
  it should "stay order-independent on a field the rank never mentions" in {
    val one = listing(None, cast = Seq("Scarlett Johansson"))
    val two = listing(None, cast = Seq("Takeshi Kitano"))

    MovieRecordMerge.slotRepresentative(Seq(one, two)) shouldBe MovieRecordMerge.slotRepresentative(Seq(two, one))
  }
}
