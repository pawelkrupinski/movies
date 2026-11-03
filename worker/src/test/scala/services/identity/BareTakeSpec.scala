package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path
import scala.util.Try

/**
 * THE BARE-TAKE PROPERTY over REAL families (docs/investigations/2026-11-01-flip-unification.md §7.10; the owner,
 * 2026-11-03: "can there be sibling dependence without flips?"): a BARE listing's take -- one publishing no fact, cast,
 * pin, feed link or shared catalogue id ([[BareListing]]) -- is its own title's, or its ANCHORS' one film
 * ([[BareListing.anchored]]), so along every arrival chain of its family it goes at most none -> X -> none: never two
 * films, never back to a film it left ([[PopulationMonotonicity.bareWalk]]'s flips). An arrival may move it (an anchor
 * arriving, or a conflicting one); it may not flip it. Every committed slice is walked, as in [[PopulationMonotonicitySpec]].
 *
 * Takes that still flip are KNOWN rows (`identity-bare-moves-known.tsv`), and the list only shrinks: a known row whose
 * listing no longer flips fails until it is removed.
 */
class BareTakeSpec extends AnyFlatSpec with Matchers with tools.CountryScope {
  import PopulationMonotonicitySpec.{Known, countryOf, relative, sliceDirs}

  override protected def countriesOf(testName: String): Seq[String] =
    if (testName.contains("drift-slices/") || testName.contains("identity-slices/")) Seq(countryOf(Path.of(testName.split(' ').last)))
    else Nil

  private val known = Known.read(BareTakeSpec.KnownFile)

  "A chain's run of bare takes" should "flip only by taking two films, or a film again after leaving it" in {
    import PopulationMonotonicity.flips
    val none = TakeFlips.NoFilm
    flips(Seq(none, "1", "1", none)) shouldBe false
    flips(Seq("1", none)) shouldBe false
    flips(Seq(none, "1", none, "1")) shouldBe true
    flips(Seq("1", "2")) shouldBe true
    flips(Seq(none, "1", none, "2")) shouldBe true
  }

  sliceDirs.foreach { dir =>
    "A bare listing" should s"never flip along an arrival chain of its family: ${relative(dir)}" in {
      val name  = relative(dir)
      val flips = BareTakeSpec.walk(dir)._1.flips
      val fresh = flips.filterNot(f => known.exists(k => k.slice == name && k.venue == f.key.venue && k.rawTitle == f.key.rawTitle))
      val fixed = known.filter(k => k.slice == name && !flips.exists(f => k.venue == f.key.venue && k.rawTitle == f.key.rawTitle))
      withClue("bare takes an arrival flipped: ")(fresh.map(_.line) shouldBe empty)
      withClue("known rows whose bare take no longer flips -- remove them from identity-bare-moves-known.tsv: ")(
        fixed.map(k => s"${k.venue} | ${k.rawTitle} (${k.note})") shouldBe empty)
    }
  }
}

object BareTakeSpec {
  val KnownFile: Path = Path.of("test", "resources", "fixtures", "identity-bare-moves-known.tsv")

  /** The walk of the bare takes in the slice in `dir`, and the whole resolve's bare listings whose anchors disagreed. */
  def walk(dir: Path): (PopulationMonotonicity.BareWalk, Set[services.movies.ListingKey]) = {
    val PopulationMonotonicitySpec.Loaded(all, lookups, normalizer, inputs, whole, core) = PopulationMonotonicitySpec.load(dir)
    (PopulationMonotonicity.bareWalk(all, lookups, normalizer, inputs, whole, PopulationMonotonicitySpec.Chains, PopulationMonotonicitySpec.Steps, core),
      whole.anchorConflicts)
  }

  /** The bare takes an arrival moves in the slice in `dir`. */
  def moves(dir: Path): Seq[PopulationMonotonicity.BareMove] = walk(dir)._1.moves

  /** `BareTakeSpec [slice dir...]`: each slice's moved and flipped bare takes, and its anchor conflicts (every slice without
   *  arguments). */
  def main(args: Array[String]): Unit = {
    val dirs = if (args.isEmpty) PopulationMonotonicitySpec.sliceDirs else args.toSeq.map(Path.of(_))
    dirs.foreach { dir =>
      val name = PopulationMonotonicitySpec.relative(dir)
      Try(walk(dir)).fold(e => println(s"# $name: FAILED $e"), { case (w, conflicts) =>
        println(s"# $name: ${w.moves.size} moved, ${w.flips.size} flipped, ${conflicts.size} anchor conflict(s)")
        w.moves.foreach(m => println(s"MOVE\t$name\t${m.line}"))
        w.flips.foreach(f => println(s"FLIP\t$name\t${f.line}"))
        conflicts.foreach(k => println(s"CONFLICT\t$name\t${k.venue} | ${k.rawTitle}"))
      })
    }
  }
}
