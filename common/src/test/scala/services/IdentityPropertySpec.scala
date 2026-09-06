package services

import org.scalacheck.{Gen, Shrink}
import org.scalactic.anyvals.PosInt
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Random

/**
 * Base for the `*PropertySpec`s that state the identity core's invariants —
 * merge, evidence, verdict, canonicalisation — as properties over generated
 * values ([[IdentityGenerators]]) rather than one prod row at a time.
 *
 * A few hundred cases per property keeps `common/test` fast; the generators draw
 * from small pools so collisions (same cinema, same title, same tmdbId) are the
 * common case rather than the rare one.
 *
 * Shrinking is OFF. Most properties compare a value against a permutation of
 * itself, and the two are generated together; shrinking either side alone
 * would report a "counterexample" that is not a permutation at all.
 */
trait IdentityPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt.ensuringValid(200))

  implicit def noShrink[A]: Shrink[A] = Shrink.shrinkAny

  /** `xs` beside a permutation of it, drawn from one seed so a failure prints both. */
  def withPermutation[A](gen: Gen[Seq[A]]): Gen[(Seq[A], Seq[A])] =
    for { xs <- gen; seed <- Gen.long } yield xs -> new Random(seed).shuffle(xs)

  /** A permutation of `xs` under `seed` — for shuffling several collections of one
   *  value together inside a single property. */
  def permute[A](seed: Long, xs: Seq[A]): Seq[A] = new Random(seed).shuffle(xs)
}
