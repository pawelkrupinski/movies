package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.IdentitySlice

import java.nio.file.Path
import java.time.{Clock, ZoneOffset}
import scala.util.Try

/**
 * THE NAMESAKE PROPERTIES over REAL families ([[NamesakeAmbiguity]]; the owner, 2026-11-04: a take is a pure function of
 * the current inputs, "incremental == whole"): every committed slice, as [[PopulationMonotonicitySpec]] walks them, along
 * its families' arrival chains ([[PopulationMonotonicity.namesakeWalk]]):
 *  (a) a listing namesake-ambiguous in the whole resolve takes the same at every step of every chain -- an arrival never
 *      moves it;
 *  (b) whether a listing is namesake-ambiguous is the same at every step as in the whole resolve -- a function of the
 *      listing and TMDB alone, never of the listings present.
 * No known rows: either property failing is a defect of the mechanism, not of a family.
 */
class NamesakeTakeSpec extends AnyFlatSpec with Matchers with tools.CountryScope {
  import PopulationMonotonicitySpec.{countryOf, relative, sliceDirs}

  override protected def countriesOf(testName: String): Seq[String] =
    if (testName.contains("drift-slices/") || testName.contains("identity-slices/")) Seq(countryOf(Path.of(testName.split(' ').last)))
    else Nil

  sliceDirs.foreach { dir =>
    "A namesake-ambiguous listing" should s"take the same on every arrival chain, and be one in every population: ${relative(dir)}" in {
      val walk = NamesakeTakeSpec.walk(dir)._1
      withClue("namesake-ambiguous takes an arrival moved: ")(walk.moves.map(_.line) shouldBe empty)
      withClue("listings a population classified otherwise than the whole resolve: ")(walk.reclassed.map(_.line) shouldBe empty)
    }
  }
}

object NamesakeTakeSpec {

  /** The walk of the slice in `dir`, and its whole resolve's namesake-ambiguous core listings. */
  def walk(dir: Path): (PopulationMonotonicity.NamesakeWalk, Seq[services.movies.ListingKey]) = {
    val slice     = IdentitySlice.readSlice(dir)
    val replaying = new tools.SliceHttp.Replaying(slice.http)
    val reads     = new IdentitySlice.Reads(slice.country, replaying, replaying.gaps, Clock.fixed(slice.recordedAt, ZoneOffset.UTC), slice.filed,
      tools.ProdState.none)
    val lookups   = scripts.IdentityDrift.memo(reads)
    val all       = slice.core ++ slice.context
    val inputs    = ResolveInputs.Unpinned
    val whole     = IdentityResolver.resolve(all, lookups, reads.normalizer, inputs)
    val core      = slice.core.map(_.key).toSet
    (PopulationMonotonicity.namesakeWalk(all, lookups, reads.normalizer, inputs, whole, PopulationMonotonicitySpec.Chains,
      PopulationMonotonicitySpec.Steps, core), whole.namesakeAmbiguous.filter(core).toSeq.sorted(using services.movies.ListingKey.ordering))
  }

  /** `NamesakeTakeSpec [slice dir...]`: each slice's namesake-ambiguous listings, moved takes and reclassed listings (every
   *  slice without arguments). */
  def main(args: Array[String]): Unit = {
    val dirs = if (args.isEmpty) sliceDirs else args.toSeq.map(Path.of(_))
    dirs.foreach { dir =>
      val name    = PopulationMonotonicitySpec.relative(dir)
      val started = System.nanoTime()
      Try(walk(dir)).fold(e => println(s"# $name: FAILED $e"), { case (w, ambiguous) =>
        println(f"# $name: ${ambiguous.size} ambiguous, ${w.moves.size} moved, ${w.reclassed.size} reclassed in ${(System.nanoTime() - started) / 1e9}%.1fs")
        ambiguous.foreach(k => println(s"AMBIGUOUS\t$name\t${k.venue} | ${k.rawTitle}"))
        w.moves.foreach(m => println(s"MOVE\t$name\t${m.line}"))
        w.reclassed.foreach(r => println(s"RECLASS\t$name\t${r.line}"))
      })
    }
  }

  private def sliceDirs = PopulationMonotonicitySpec.sliceDirs
}
