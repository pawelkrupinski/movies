package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `screenings` and `movie_slots` are the same shape of side collection — one row per
 * `(filmId, slotKey)`, addressed through the shared [[SlotKeyed]]. Their `*Checked` reads
 * therefore have to mean the same thing, because callers branch on that boolean and a
 * caller cannot ask which repository it happens to be holding.
 *
 * They diverged on the one state neither is a read of: no collection wired at all.
 * `MongoScreeningsRepository` reported it as a FAILED read, `MongoSlotsRepository` as a
 * complete one — and "failed" is wrong, because nothing was attempted. It matters as soon
 * as a caller defers on failure: against a Mongo-less stack the screenings half would defer
 * forever while the slots half proceeded, on identical inputs.
 */
class SideRepositoryParitySpec extends AnyFlatSpec with Matchers {

  private val screenings = new MongoScreeningsRepository(None)
  private val slots      = new MongoSlotsRepository(None)

  "an unwired side repository" should "report a per-film read as COMPLETE, not failed" in {
    withClue("screenings: ")(screenings.findForFilmChecked("film|2026")._2 shouldBe true)
    withClue("slots: ")(slots.findForFilmChecked("film|2026")._2           shouldBe true)
  }

  it should "agree with its sibling on both halves of the answer" in {
    screenings.findForFilmChecked("film|2026")._1 shouldBe empty
    slots.findForFilmChecked("film|2026")._1      shouldBe empty
    screenings.findForFilmChecked("film|2026")._2 shouldBe slots.findForFilmChecked("film|2026")._2
  }

  it should "report a multi-film read as COMPLETE too" in {
    withClue("screenings: ")(screenings.findForFilmsChecked(Set("a|", "b|"))._2 shouldBe true)
    withClue("slots: ")(slots.findForFilmsChecked(Set("a|", "b|"))._2           shouldBe true)
  }
}
