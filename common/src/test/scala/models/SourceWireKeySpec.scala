package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `displayName` is not a label — it is the STORAGE KEY. Every per-cinema slot in
 * `movie_slots`, and every row in `screenings`, is filed under it, and `Source.byWireKey`
 * is the only way back. So the map from display name to source has to be a bijection.
 *
 * It was not. `byDisplayName` is `all.map(s => s.displayName -> s).toMap`, so two venues
 * sharing a name silently collapse to whichever comes LAST in `Cinema.all` — and the loser
 * becomes unaddressable: reading back any of its slots hands you the WINNER's `Source`.
 *
 * The damage is not cosmetic. A read rebuilds the film's slot map under the wrong venue, so
 * (1) the next scrape sees its own slot as missing and rewrites it — on every tick, forever,
 * which is how this was found (`ReScrapeIdempotencySpec` never reaching a fixpoint once the
 * fake modelled the split), and (2) the showtimes are attributed to a venue in the wrong
 * CITY, so the city listing that should show them does not.
 *
 * Nothing caught it before because with slots embedded in the `movies` document the `Source`
 * is stored as itself and never round-trips through its name.
 */
class SourceWireKeySpec extends AnyFlatSpec with Matchers {

  "every source's displayName" should "be unique — it is the wire key slots are stored under" in {
    val collisions = Source.all
      .groupBy(_.displayName)
      .filter(_._2.size > 1)
      .view.mapValues(_.map(s => s"${s.getClass.getSimpleName}(${Source.cinemaOf(s).map(_.toString).getOrElse("—")})"))
      .toMap

    withClue(
      "these display names map to more than one Source, so `byDisplayName` keeps only the last " +
      "and every stored slot of the others reads back as the wrong venue:\n" +
      collisions.toSeq.sortBy(_._1).map { case (n, ss) => s"  '$n' -> ${ss.mkString(", ")}" }.mkString("\n") + "\n") {
      collisions shouldBe empty
    }
  }

  it should "round-trip through the wire key it is stored under" in {
    Source.all.foreach { source =>
      withClue(s"'${source.displayName}' did not resolve back to itself: ") {
        Source.byWireKey(source.displayName) shouldBe Some(source)
      }
    }
  }

  it should "round-trip a per-title cinema slot too" in {
    Source.all.collect { case cinema: Cinema => cinema }.foreach { cinema =>
      val showing = CinemaShowing(cinema, "sometitlekey")
      withClue(s"per-title slot for '${cinema.displayName}' did not resolve back to itself: ") {
        Source.byWireKey(showing.displayName) shouldBe Some(showing)
      }
    }
  }
}
