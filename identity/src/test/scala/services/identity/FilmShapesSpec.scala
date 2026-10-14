package services.identity

import models.{CinemaShowing, Helios, KinoMuza, Multikino, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingKey, SingleCountryNormalizer}

/**
 * What the projection keeps of each film from one projection to the next is the object it held when nothing about it
 * moved: kept until the next projection, minutes on worker-us, every copy made anew is promoted to the old generation
 * and dies there (prod histograms 2026-10-05: 570k dead venue shapes against 99k live).
 */
class FilmShapesSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private def slot(cinema: models.Cinema, director: String) =
    (CinemaShowing.keyFor(cinema, "Lalka", normalizer): models.Source) -> SourceData(title = Some("Lalka"), director = Seq(director))
  private def record(directors: String*) =
    MovieRecord(data = Seq(Multikino, Helios, KinoMuza).zip(directors).map { case (c, d) => slot(c, d) }.toMap)
  private val keys = Seq(ListingKey.Native("Multikino", "lalka", "Lalka"), ListingKey.Native("Helios", "lalka", "Lalka"))
  private val venue = VenueShape(keys, VenueSlotMemo.Key("Multikino", 1, 2, 3, 2), Nil, 7L)

  "a film shape put over its members read again" should "keep its venues, the very objects, when their keys are the ones held" in {
    val shape = FilmShape(keys.toSet, keys, Map("Lalka" -> 2), Map(Multikino -> venue))
    val again = shape.over(keys.toSet.map(identity) + keys.head, identity)
    (again.venues eq shape.venues) shouldBe true
    (again.keys eq shape.keys) shouldBe true
  }

  it should "copy only a venue holding another key object" in {
    val atMultikino = VenueShape(keys.take(1), VenueSlotMemo.Key("Multikino", 1, 1, 1, 1), Nil, 7L)
    val atHelios    = VenueShape(keys.drop(1), VenueSlotMemo.Key("Helios", 1, 1, 1, 1), Nil, 3L)
    val shape = FilmShape(keys.toSet, keys, Map("Lalka" -> 2), Map(Multikino -> atMultikino, Helios -> atHelios))
    val fresh = ListingKey.Native(new String("Helios"), new String("lalka"), new String("Lalka"))
    val again = shape.over(keys.toSet + fresh, k => if (k == fresh) fresh else k)
    again.venues(Multikino) should be theSameInstanceAs atMultikino
    again.venues(Helios).keys.head should be theSameInstanceAs fresh
  }

  "a film's priors by venue" should "be the map already held when its record is written again alike" in {
    val shapes = FilmShapes()
    val held   = shapes.priorsOf("f1", record("A", "B", "C"))
    shapes.commit(whole = false)
    shapes.priorsOf("f1", record("A", "B", "C")) should be theSameInstanceAs held
  }

  it should "read a venue that moved anew, and every other as it was" in {
    val shapes = FilmShapes()
    val held   = shapes.priorsOf("f1", record("A", "B", "C"))
    shapes.commit(whole = false)
    val moved  = shapes.priorsOf("f1", record("A", "B", "D"))
    moved shouldBe FilmShapes().priorsOf("f1", record("A", "B", "D"))
    moved(KinoMuza) should not be held(KinoMuza)
    moved(Multikino) shouldBe held(Multikino)
  }

  "a venue drafted again" should "be the venue kept when it is drafted alike, and the new one when anything moved" in {
    val drafted = venue.copy(keys = keys.map(identity))
    FilmShape.reused(Some(venue), drafted) should be theSameInstanceAs venue
    FilmShape.reused(Some(venue), drafted.copy(inputs = 8L)).inputs shouldBe 8L
    FilmShape.reused(None, drafted) should be theSameInstanceAs drafted
  }
}
