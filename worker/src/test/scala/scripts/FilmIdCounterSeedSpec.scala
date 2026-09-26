package scripts

import models.{CinemaShowing, KinoMuranow, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.ListingKeyBackfill.SlotRow
import services.movies.{ListingKey, SlotKeyed}

/** Which films the FilmId map is seeded with, and the listings each is ranked by. */
class FilmIdCounterSeedSpec extends AnyFlatSpec with Matchers {

  private val belle = CinemaShowing(KinoMuranow, "belle").displayName
  private val slot  = SourceData(title = Some("Belle"), filmUrl = Some("https://muranow.pl/belle"))
  private def row(film: String, slotKey: String, sd: SourceData = slot) = SlotRow(SlotKeyed.idOf(film, slotKey), film, slotKey, sd, None)

  "films" should "hold every film id either collection names, each with its venue listings only" in {
    val films = FilmIdCounterSeed.films(
      Seq(row("belle|2013", belle), row("belle|2013", Tmdb.displayName), row("orphan|2020", Tmdb.displayName)),
      movieIds = Seq("belle|2013", "slotless|2021"))
    films.map(f => f.id -> f.listings) shouldBe Seq(
      "belle|2013"    -> ListingKey.ofSlotRow(belle, slot).toSet,
      "orphan|2020"   -> Set.empty,
      "slotless|2021" -> Set.empty)
  }
}
