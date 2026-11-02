package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path
import scala.util.Try

/**
 * THE BARE-TAKE PROPERTY over REAL families (docs/investigations/2026-11-01-flip-unification.md §7.10; the owner,
 * 2026-11-03: "a take is a pure function of the current inputs, as if all data had been there from the start"): a BARE
 * listing's take -- one publishing no fact, cast, pin, feed link or shared catalogue id ([[BareListing]]) -- is the same at
 * every step of every arrival chain of its family ([[PopulationMonotonicity.bareMoves]]). Stronger than
 * [[PopulationMonotonicitySpec]]'s "never back to a value it left": an arrival moves a bare take not even once, so no
 * order of arrival can flip it. Every committed slice is walked, as there.
 *
 * Takes that still move are KNOWN rows (`identity-bare-moves-known.tsv`), and the list only shrinks: a known row whose
 * listing no longer moves fails until it is removed.
 */
class BareTakeSpec extends AnyFlatSpec with Matchers with tools.CountryScope {
  import PopulationMonotonicitySpec.{Known, countryOf, relative, sliceDirs}

  override protected def countriesOf(testName: String): Seq[String] =
    if (testName.contains("drift-slices/") || testName.contains("identity-slices/")) Seq(countryOf(Path.of(testName.split(' ').last)))
    else Nil

  private val known = Known.read(BareTakeSpec.KnownFile)

  sliceDirs.foreach { dir =>
    "A bare listing" should s"take the same on every arrival chain of its family: ${relative(dir)}" in {
      val name  = relative(dir)
      val moves = BareTakeSpec.moves(dir)
      val fresh = moves.filterNot(m => known.exists(k => k.slice == name && k.venue == m.key.venue && k.rawTitle == m.key.rawTitle))
      val fixed = known.filter(k => k.slice == name && !moves.exists(m => k.venue == m.key.venue && k.rawTitle == m.key.rawTitle))
      withClue("bare takes an arrival moved: ")(fresh.map(_.line) shouldBe empty)
      withClue("known rows whose bare take no longer moves -- remove them from identity-bare-moves-known.tsv: ")(
        fixed.map(k => s"${k.venue} | ${k.rawTitle} (${k.note})") shouldBe empty)
    }
  }
}

object BareTakeSpec {
  val KnownFile: Path = Path.of("test", "resources", "fixtures", "identity-bare-moves-known.tsv")

  /** The bare takes an arrival moves in the slice in `dir`. */
  def moves(dir: Path): Seq[PopulationMonotonicity.BareMove] = {
    val PopulationMonotonicitySpec.Loaded(all, lookups, normalizer, inputs, whole, core) = PopulationMonotonicitySpec.load(dir)
    PopulationMonotonicity.bareMoves(all, lookups, normalizer, inputs, whole, PopulationMonotonicitySpec.Chains, PopulationMonotonicitySpec.Steps, core)
  }

  /** `BareTakeSpec [slice dir...]`: each slice's moved bare takes as rows of the known file (every slice without arguments). */
  def main(args: Array[String]): Unit = {
    val dirs = if (args.isEmpty) PopulationMonotonicitySpec.sliceDirs else args.toSeq.map(Path.of(_))
    dirs.foreach { dir =>
      Try(moves(dir)).fold(e => println(s"# ${PopulationMonotonicitySpec.relative(dir)}: FAILED $e"), ms => {
        println(s"# ${PopulationMonotonicitySpec.relative(dir)}: ${ms.size} moved")
        ms.foreach(m => println(s"${PopulationMonotonicitySpec.relative(dir)}\t${m.key.venue}\t${m.key.rawTitle}\t${m.line}"))
      })
    }
  }
}
