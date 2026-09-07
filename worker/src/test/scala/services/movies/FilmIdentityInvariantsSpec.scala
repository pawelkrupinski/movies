package services.movies

import models._
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime

/** What must hold after ANY sequence of writes and re-keys, whatever their order:
 *  every side-collection row belongs to a live document, every key is held by exactly
 *  one document, and the corpus index still agrees with the rows. The fold, the retitle
 *  and the settle each moved documents around in their own way before the stable film
 *  id; this pins the invariant they all now share. */
class FilmIdentityInvariantsSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  sealed trait Op
  final case class Put(title: String, year: Option[Int], tmdbId: Option[Int], cinema: Cinema) extends Op
  final case class ResolveYear(title: String, year: Int, tmdbId: Int) extends Op
  final case class Settle() extends Op

  private val titles  = Gen.oneOf("Alpha", "Beta", "Gamma")
  private val years   = Gen.oneOf(Some(2025), Some(2026), None)
  private val tmdbIds = Gen.oneOf(Some(1), Some(2), None)
  private val cinemas = Gen.oneOf[Cinema](KinoMuza, Helios, Multikino)
  private val op: Gen[Op] = Gen.frequency(
    6 -> (for (t <- titles; y <- years; id <- tmdbIds; c <- cinemas) yield Put(t, y, id, c)),
    2 -> (for (t <- titles; y <- Gen.oneOf(2025, 2026); id <- Gen.oneOf(1, 2)) yield ResolveYear(t, y, id)),
    1 -> Gen.const(Settle()))
  private val ops: Gen[List[Op]] = Gen.choose(1, 12).flatMap(n => Gen.listOfN(n, op))

  private def slot(cinema: Cinema, title: String, year: Option[Int]) =
    Map[Source, SourceData]((cinema: Source) -> SourceData(title = Some(title), releaseYear = year,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 12, 20, 0), None))))

  "any sequence of puts, resolutions and settles" should "leave every side row on a live document and every key on one document" in {
    forAll(ops, minSuccessful(80)) { sequence =>
      val screenings = new InMemoryScreeningsRepository
      val slots      = new InMemorySlotsRepository
      val repository = new InMemoryMovieRepository(screenings = Some(screenings), slots = Some(slots))
      val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
      sequence.foreach {
        case Put(t, y, id, c) =>
          val key = CacheKey(t, y, titleNormalizer)
          val existing = cache.get(key).map(_.data).getOrElse(Map.empty)
          cache.put(key, MovieRecord(tmdbId = id, data = existing ++ slot(c, t, y) ++
            id.map(i => (Tmdb: Source) -> SourceData(title = Some(t), releaseYear = y.orElse(Some(2026)))).toMap))
        case ResolveYear(t, y, id) =>
          val yearless = CacheKey(t, None, titleNormalizer)
          if (cache.get(yearless).isDefined)
            cache.settleResolved(yearless, cache.get(yearless).get.copy(tmdbId = Some(id),
              data = cache.get(yearless).get.data + ((Tmdb: Source) -> SourceData(title = Some(t), releaseYear = Some(y)))))
        case Settle() => cache.canonicalizeBySanitize()
      }

      val rows    = repository.findAll()
      val liveIds = rows.map(_.id.value).toSet
      withClue(s"ops=$sequence\nrows=${rows.map(r => (r.id.value, r.key(titleNormalizer)))}\n") {
        (screenings.findAll().keySet -- liveIds) shouldBe empty
        (slots.findAll().keySet -- liveIds)      shouldBe empty
        rows.groupBy(_.key(titleNormalizer)).collect { case (k, rs) if rs.sizeIs > 1 => k } shouldBe empty
        cache.indexSnapshot shouldBe cache.rowsRebuiltIndexSnapshot
      }
    }
  }
}
