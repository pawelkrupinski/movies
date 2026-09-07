package services.movies

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{CinemaCityWroclavia, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `StoredMovieRecord.keyFor` is the one lookup-key formula the repository, the change
 * stream, and the /debug live view all key rows on. The load-bearing property:
 * case- and diacritic-only variants of the same title fold to ONE id, so a
 * merge collapses them to a single row (and the live view's per-id DOM lookup
 * finds it) instead of leaving duplicates.
 */
class StoredMovieRecordIdSpec extends AnyFlatSpec with Matchers {

  "keyFor" should "be sanitize(title)|year" in {
    StoredMovieRecord.keyFor("Belle", Some(2021), titleNormalizer) shouldBe "belle|2021"
    StoredMovieRecord.keyFor("Belle", None, titleNormalizer)       shouldBe "belle|"
  }

  it should "fold case + diacritic variants of the same title to one id" in {
    val a = StoredMovieRecord.keyFor("Tom i Jerry: Przygoda w muzeum", Some(2024), titleNormalizer)
    val b = StoredMovieRecord.keyFor("tom i jerry: przygoda w muzeum", Some(2024), titleNormalizer)
    a shouldBe b
  }

  it should "agree with fromStorage's round-trip (the id rebuilds to the same id)" in {
    val id      = StoredMovieRecord.keyFor("Diuna", Some(2021), titleNormalizer)
    val rebuilt = StoredMovieRecord.fromStorage(id, MovieRecord(), titleNormalizer)
    rebuilt.id.value shouldBe id
  }

  it should "key each stored row on its own _id, so two docs never share a DOM id" in {
    // Prod /debug showed two "Zabriskie Point" rows that both opened the SAME
    // details panel: one Mongo doc bakes the year into its title, so its `_id`
    // is `zabriskiepoint1970|1970`; a second clean doc (`zabriskiepoint|1970`)
    // has a cinema reporting the title WITH the year, so RE-DERIVING an id from
    // its display title (`sanitize("Zabriskie Point (1970)")|1970`) collapses it
    // onto the first. Both rows then render the same `data-id` and the live
    // view's first-match lookup opens whichever row comes first. The id must be
    // the persisted `_id`, which is unique, not the round-trip re-derivation.
    val baked = StoredMovieRecord.fromStorage("zabriskiepoint1970|1970", MovieRecord(), titleNormalizer)
    val clean = StoredMovieRecord.fromStorage(
      "zabriskiepoint|1970",
      MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some("Zabriskie Point (1970)")))), titleNormalizer)

    baked.id.value shouldBe "zabriskiepoint1970|1970"
    clean.id.value shouldBe "zabriskiepoint|1970"
    baked.id.value should not be clean.id.value
  }
}
