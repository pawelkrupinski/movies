package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditDistanceSpec extends AnyFlatSpec with Matchers {

  private val cases: Seq[(String, String, Int)] = Seq(
    ("", "", 0), ("abc", "", 3), ("", "abc", 3),
    ("kitten", "sitting", 3),
    ("guru", "gourou", 2),
    ("sokourov", "sokurow", 2),     // a substitution after a deletion — the greedy walk's blind spot
    ("tarkowski", "tarkovsky", 2),
    ("verhoeven", "verhoven", 1),
    ("nyby", "niby", 1),
    ("dalloway", "guru", 8)
  )

  "between" should "compute the Levenshtein distance" in {
    for ((a, b, d) <- cases) withClue(s"$a / $b: ") {
      EditDistance.between(a, b) shouldBe d
      EditDistance.between(b, a) shouldBe d
    }
  }

  "within" should "agree with the full distance at every bound" in {
    for ((a, b, d) <- cases; max <- 0 to 4) withClue(s"$a / $b within $max: ") {
      EditDistance.within(a, b, max) shouldBe (d <= max)
      EditDistance.within(b, a, max) shouldBe (d <= max)
    }
  }
}
