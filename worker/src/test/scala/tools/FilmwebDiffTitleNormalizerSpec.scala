package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Pins the cross-source title equivalences `FilmwebDiff` relies on: the same
 *  screening must normalise to the same key whether it comes from our scraper or
 *  Filmweb, so language/format variants and truncation stop double-counting a
 *  film as both `ours-only` and `fw-only`. */
class FilmwebDiffTitleNormalizerSpec extends AnyFlatSpec with Matchers {
  import FilmwebDiffTitleNormalizer.normalize

  "normalize" should "strip trailing language tags so language variants key alike" in {
    normalize("Peddi Hindi")   shouldBe "peddi"
    normalize("Peddi Telugu")  shouldBe "peddi"
    normalize("Peddi")         shouldBe "peddi"
    normalize("Peddi Hindi")   shouldBe normalize("Peddi")
    normalize("Peddi Telugu")  shouldBe normalize("Peddi")
  }

  it should "strip trailing version + format tags (napisy/dubbing/lektor/2D/3D/IMAX/4DX)" in {
    normalize("Avatar napisy")        shouldBe "avatar"
    normalize("Avatar dubbing")       shouldBe "avatar"
    normalize("Avatar lektor")        shouldBe "avatar"
    normalize("Avatar 3D")            shouldBe "avatar"
    normalize("Avatar 2D napisy")     shouldBe "avatar"
    normalize("Avatar IMAX")          shouldBe "avatar"
    normalize("Avatar 4DX")           shouldBe "avatar"
  }

  it should "not double-count a truncated title against its full form" in {
    // Filmweb truncates long titles with an ellipsis; our scraper keeps the full
    // title. Both must reduce to the shared prefix key.
    // Diacritics are preserved (both sides carry the same Polish title), so the
    // keys match on the shared prefix without needing diacritic folding.
    val full      = normalize("Skarpetek dwie, podróż na kraj kosmosu")
    val truncated = normalize("Skarpetek dwie, podróż na kraj…")
    val truncated2 = normalize("Skarpetek dwie, podróż na kraj...")
    truncated  shouldBe "skarpetek dwie podróż na kraj"
    truncated2 shouldBe truncated
    full       should startWith(truncated)
  }

  it should "keep the existing Arabic→Roman, lowercase and space-collapse behaviour" in {
    normalize("Avatar 3")            shouldBe "avatar iii"
    normalize("Avatar III")          shouldBe "avatar iii"
    normalize("Avatar 3")            shouldBe normalize("Avatar III")
    normalize("  Dune   Part   2 ")  shouldBe "dune part ii"
  }

  it should "drop parenthetical/bracket annotations (original title, year)" in {
    normalize("Diuna (Dune)")        shouldBe "diuna"
    normalize("Diuna [2024]")        shouldBe "diuna"
  }

  it should "never reduce a title to empty even if it looks like a bare tag" in {
    // A title that's nothing BUT a trailing tag keeps the single token (we never
    // strip down to empty) rather than vanishing.
    normalize("3D")   shouldBe "3d"
    normalize("IMAX") shouldBe "imax"
  }
}
