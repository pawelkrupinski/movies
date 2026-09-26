package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdSeeding.Film
import services.movies.ListingKey

/**
 * The persisted FilmId map's rules, as a value: injective both ways, append-only, and new films
 * numbered after every counter ever handed out, largest film first.
 */
class FilmIdCountersSpec extends AnyFlatSpec with Matchers {

  private def film(id: String, listings: Int) =
    Film(id, (1 to listings).map(i => ListingKey.Native(s"venue$i", s"https://venue$i/$id", id): ListingKey).toSet)

  "additionsFor" should "number unmapped films from 1, largest first, ties by id" in {
    FilmIdCounters.empty.additionsFor(Seq(film("b", 2), film("a", 2), film("big", 5), film("a", 2))) shouldBe
      Seq(FilmIdCounter("big", 1), FilmIdCounter("a", 2), FilmIdCounter("b", 3))
  }

  it should "leave a mapped film where it is, however it has grown, and number new ones after the largest counter" in {
    val stored = FilmIdCounters.of(Seq(FilmIdCounter("small|2020", 1), FilmIdCounter("gone|2019", 7))).toOption.get
    stored.additionsFor(Seq(film("small|2020", 40), film("new", 1), film("newer", 3))) shouldBe
      Seq(FilmIdCounter("newer", 8), FilmIdCounter("new", 9))
    stored.covering(Seq(film("small|2020", 40), film("new", 1))).counterOf("small|2020") shouldBe Some(1)
  }

  "of" should "refuse a map that is not injective both ways" in {
    FilmIdCounters.of(Seq(FilmIdCounter("a", 1), FilmIdCounter("a", 2))).isLeft shouldBe true
    FilmIdCounters.of(Seq(FilmIdCounter("a", 1), FilmIdCounter("b", 1))).isLeft shouldBe true
    FilmIdCounters.of(Seq(FilmIdCounter("a", 0))).isLeft shouldBe true
    FilmIdCounters.of(Seq(FilmIdCounter("a", 1), FilmIdCounter("b", 3))).map(_.nextCounter) shouldBe Right(4L)
  }

  "FilmIdMapping" should "append only what the map lacks, and refuse to plan over an unreadable or broken store" in {
    val store   = new InMemoryFilmIdCounterStore
    val mapping = new FilmIdMapping(store)
    mapping.plan(Seq(film("a", 1))).map(_._2) shouldBe Right(Seq(FilmIdCounter("a", 1)))
    store.allChecked()._1 shouldBe empty                                           // a plan writes nothing
    mapping.append(Seq(film("a", 1), film("b", 2))) shouldBe Right(2)
    mapping.append(Seq(film("a", 9), film("b", 2))) shouldBe Right(0)
    mapping.load().map(_.counterOf("a")) shouldBe Right(Some(2L))
    new FilmIdMapping(new FilmIdCounterStore {
      def allChecked() = (Seq.empty, false)
      def insert(entries: Seq[FilmIdCounter]) = fail("must not write after a failed read")
    }).append(Seq(film("a", 1))).isLeft shouldBe true
  }
}
