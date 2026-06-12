package services.movies

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `StoredMovieRecord.idFor` is the one `_id` formula the repo, the change
 * stream, and the /debug live view all key rows on. The load-bearing property:
 * case- and diacritic-only variants of the same title fold to ONE id, so a
 * merge collapses them to a single row (and the live view's per-id DOM lookup
 * finds it) instead of leaving duplicates.
 */
class StoredMovieRecordIdSpec extends AnyFlatSpec with Matchers {

  "idFor" should "be sanitize(title)|year" in {
    StoredMovieRecord.idFor("Belle", Some(2021)) shouldBe "belle|2021"
    StoredMovieRecord.idFor("Belle", None)       shouldBe "belle|"
  }

  it should "fold case + diacritic variants of the same title to one id" in {
    val a = StoredMovieRecord.idFor("Tom i Jerry: Przygoda w muzeum", Some(2024))
    val b = StoredMovieRecord.idFor("tom i jerry: przygoda w muzeum", Some(2024))
    a shouldBe b
  }

  it should "agree with fromStorage's round-trip (the id rebuilds to the same id)" in {
    val id      = StoredMovieRecord.idFor("Diuna", Some(2021))
    val rebuilt = StoredMovieRecord.fromStorage(id, MovieRecord())
    StoredMovieRecord.idOf(rebuilt) shouldBe id
  }
}
