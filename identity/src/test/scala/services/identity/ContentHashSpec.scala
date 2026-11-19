package services.identity

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.hashing.MurmurHash3

/** The slice digest a stored family keeps: equal content hashes equally however it was built, in
 *  every JVM, and any change to it — however deep — moves the hash. */
class ContentHashSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  private def film(title: String, directors: Seq[String]) =
    Candidate(7, IdentityMeasures.Film(title, year = Some(1968), directors = Some(directors), popularity = Some(3.5)))
  private def slice(candidates: Seq[(Int, Candidate)], answers: Seq[(CandidateQuery, Option[Seq[Int]])]) =
    CorpusContext.Slice(Map.empty, Map.empty, Set("lalka"), Set.empty, Set.empty, Map("lalka" -> "has"), candidates.toMap, answers.toMap)

  private val lalka   = film("Lalka", Seq("Wojciech Has"))
  private val queries = Seq(CandidateQuery.Title("Lalka") -> Some(Seq(7, 9)), CandidateQuery.Director("Wojciech Has") -> None)

  "a slice's digest" should "be equal for equal content, whatever order its maps and sets were built in" in {
    slice(Seq(7 -> lalka, 9 -> film("Lalka", Nil)), queries).digest shouldBe
      slice(Seq(9 -> film("Lalka", Nil), 7 -> lalka), queries.reverse).digest
  }

  it should "move with any change, however deep in a record" in {
    val base = slice(Seq(7 -> lalka), queries).digest
    slice(Seq(7 -> film("Lalka", Seq("Wojciech J. Has"))), queries).digest should not be base
    slice(Seq(7 -> lalka), queries.map { case (q, a) => q -> a.map(_.reverse) }).digest should not be base
    slice(Seq(7 -> lalka), queries.take(1)).digest should not be base
  }

  it should "be the same in every JVM and run: it is persisted beside each stored family" in {
    // Pinned: a change here strands every stored family's digest (a full re-resolve on deploy).
    ContentHash.of(slice(Seq(7 -> lalka), queries)) shouldBe ContentHash.of(slice(Seq(7 -> lalka), queries))
    ContentHash.of(("lalka", Some(1968), Seq(1.5, 2.0), Set("a", "b"))) shouldBe PinnedTuple
  }

  // Every listing carries the corpus-wide learned decorations, one shared instance; walking it for
  // each listing of each group of each slice made digests 9.5 s of a US take-up (UK 2.7 s). The
  // decorations are already the rules version's (IncrementalResolver.rulesVersion): a change to
  // them rebuilds every family, so a slice's digest has nothing to learn from them.
  it should "not read the learned title decorations a listing carries" in {
    val some  = TitleDecorations(Set(Seq("kino", "seniora")), Set(Seq("2d")))
    val other = TitleDecorations(Set(Seq("ladies", "night")), Set.empty)
    val listing = IdentityMeasures.Listing("Lalka", year = Some(2025))
    ContentHash.of(listing.copy(decorations = some)) shouldBe ContentHash.of(listing.copy(decorations = other))
    ContentHash.of(listing.copy(title = "Lalka 2")) should not be ContentHash.of(listing)
  }

  it should "hash the constants of the walk as they were first stored" in {
    ContentHash.of(null) shouldBe 0x9e3779b97f4a7c15L
    ContentHash.of(None) shouldBe 0x7L
    ContentHash.of(true) shouldBe 0x4L
    ContentHash.of(false) shouldBe 0x5L
    ContentHash.of(TitleDecorations(Set(Seq("2d")), Set.empty)) shouldBe 0xdL
  }

  it should "hash any value exactly as the walk first stored did (ContentHashSpec.Reference)" in {
    forAll(ContentHashSpec.values, minSuccessful(2000)) { value => ContentHash.of(value) shouldBe ContentHashSpec.Reference.of(value) }
  }

  private val PinnedTuple = 7427319714951912975L
}

object ContentHashSpec {

  /** The walk as it was when its digests were first stored, VERBATIM: a stored family's slice digest and a listing's model
   *  digest ([[FamilyIndex.modelDigest]]) are this function's output, so [[ContentHash.of]] must stay byte-identical to
   *  it however it is written. Changing it here is changing every stored digest -- a full re-resolve on deploy. */
  object Reference {
    def of(value: Any): Long = value match {
      case null                => 0x9e3779b97f4a7c15L
      case _: TitleDecorations => 0xdL
      case s: String           => (MurmurHash3.stringHash(s, 0x1b873593).toLong << 32) ^ (MurmurHash3.stringHash(s, 0x5bd1e995) & 0xffffffffL)
      case i: Int              => mix(i.toLong ^ 0x1L)
      case l: Long             => mix(l ^ 0x2L)
      case d: Double           => mix(java.lang.Double.doubleToLongBits(d) ^ 0x3L)
      case b: Boolean          => if (b) 0x4L else 0x5L
      case c: Char             => mix(c.toLong ^ 0x6L)
      case None                => 0x7L
      case Some(x)             => mix(of(x) + 0x8L)
      case m: scala.collection.Map[?, ?] =>
        mix(m.foldLeft(m.size.toLong * 0x9L) { case (acc, (k, v)) => acc + mix(of(k) * 31 + of(v)) })
      case s: scala.collection.Set[?] =>
        mix(s.foldLeft(s.size.toLong * 0xaL)((acc, e) => acc + mix(of(e))))
      case s: Iterable[?]      => s.foldLeft(0xbL)((acc, e) => mix(acc * 31 + of(e)))
      case p: Product          => p.productIterator.foldLeft(of(p.productPrefix))((acc, e) => mix(acc * 31 + of(e)))
      case other               => mix(other.hashCode.toLong ^ 0xcL)
    }

    private def mix(z0: Long): Long = {
      var z = z0
      z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
      z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
      z ^ (z >>> 31)
    }
  }

  private val leaves: Gen[Any] = Gen.oneOf[Any](
    Gen.const(null), Gen.const(None), Gen.alphaStr, Gen.chooseNum(Int.MinValue, Int.MaxValue), Gen.chooseNum(Long.MinValue, Long.MaxValue),
    Gen.double, Gen.oneOf(true, false), Gen.alphaChar, Gen.const(TitleDecorations(Set(Seq("2d")), Set.empty)),
    Gen.chooseNum(0, 9).map(java.math.BigInteger.valueOf(_)), Gen.alphaStr.map(CandidateQuery.Title(_)))

  /** Any value a digest walks: scalars, and options, maps, sets, sequences and products of them, nested. */
  def values: Gen[Any] = Gen.sized(size => valuesOf(math.min(size, 4)))
  private def valuesOf(depth: Int): Gen[Any] =
    if (depth <= 0) leaves
    else {
      val inner = Gen.lzy(valuesOf(depth - 1))
      Gen.frequency(
        3 -> leaves,
        1 -> inner.map(Some(_)),
        1 -> Gen.listOfN(3, inner),
        1 -> Gen.listOfN(3, inner).map(_.toVector),
        1 -> Gen.listOfN(3, inner).map(_.toSet),
        1 -> Gen.listOfN(3, Gen.zip(Gen.alphaStr, inner)).map(_.toMap),
        1 -> Gen.zip(inner, inner, inner))
    }
}
