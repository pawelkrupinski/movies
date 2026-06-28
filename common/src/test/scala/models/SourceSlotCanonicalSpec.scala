package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `Source.dropSupersededCinemaSlots` removes a legacy bare-`Cinema` slot once a
 * per-title `CinemaShowing` slot for the SAME cinema exists — the duplication a
 * pre-split (commit 847f555f) row develops once it's re-scraped under the new
 * per-title key. A lone bare slot (no per-title sibling) and the external
 * enrichment sources are left untouched.
 */
class SourceSlotCanonicalSpec extends AnyFlatSpec with Matchers {

  private val mkShowing = CinemaShowing.keyFor(Multikino, "Dzień objawienia")

  "dropSupersededCinemaSlots" should "drop a bare Cinema slot superseded by a per-title slot of the same cinema" in {
    val data = Map[Source, String](Multikino -> "bare", mkShowing -> "per-title", Tmdb -> "tmdb")
    Source.dropSupersededCinemaSlots(data) shouldBe Map[Source, String](mkShowing -> "per-title", Tmdb -> "tmdb")
  }

  it should "keep a lone bare Cinema slot with no per-title sibling" in {
    val data = Map[Source, String](Helios -> "bare", Tmdb -> "tmdb")
    Source.dropSupersededCinemaSlots(data) shouldBe data
  }

  it should "only drop the bare slot of the cinema that has a per-title sibling" in {
    // Multikino has a per-title slot (drop its bare); Helios has only a bare slot (keep).
    val data = Map[Source, String](Multikino -> "bare-mk", mkShowing -> "per-title", Helios -> "bare-helios")
    Source.dropSupersededCinemaSlots(data) shouldBe Map[Source, String](mkShowing -> "per-title", Helios -> "bare-helios")
  }

  it should "leave a map with no per-title slots unchanged" in {
    val data = Map[Source, String](Multikino -> "bare", Helios -> "bare", Tmdb -> "tmdb")
    Source.dropSupersededCinemaSlots(data) shouldBe data
  }

  it should "never touch external enrichment sources" in {
    val data = Map[Source, String](mkShowing -> "per-title", Tmdb -> "tmdb", Imdb -> "imdb", Filmweb -> "filmweb")
    Source.dropSupersededCinemaSlots(data) shouldBe data
  }
}
