package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingKey, SingleCountryNormalizer}

import scala.util.Random

/**
 * The resolver's properties on GENERATED corpora (`GeneratedIdentityCorpus`), each beside the
 * mutation that must break it — a property a mutant passes proves nothing.
 *
 *  - A1: the multiset of lookups is the same for every presentation of the set;
 *  - P1: the partition, films and confidences are the same for every presentation — 14 random
 *    permutations and 7 split arrivals (a part resolved alone first, then the whole);
 *  - P2: resolving the same set again changes nothing, and re-assigning ids moves no listing;
 *  - P3: no cluster holds a cannot-linked pair;
 *  - A3: resolving each family alone equals the global resolve, and an edge crossing a family
 *    fails the resolve rather than being dropped.
 */
class IdentityResolverPropertiesSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val weights    = IdentityCalibration.fromResource("services/identity/test-calibration.json").get
  private val Seeds      = 1L to 40L
  private val Arrivals   = 1L to 21L

  private def corpus(seed: Long) = GeneratedIdentityCorpus.generate(seed, normalizer)

  /** Every call the resolver makes to its lookups, as a multiset. */
  private final class Recording(inner: IdentityLookups) extends IdentityLookups {
    val calls = scala.collection.mutable.ArrayBuffer.empty[String]
    override def hasDetail(l: Listing): Boolean = inner.hasDetail(l)
    override def detail(l: Listing): Answer[Option[DetailFacts]] = { calls += s"detail ${l.venue} ${l.page}"; inner.detail(l) }
    override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = { calls += q.sortKey; inner.candidates(q) }
    override def film(id: Int): Answer[Option[IdentityMeasures.Film]] = { calls += s"film $id"; inner.film(id) }
    def multiset: Map[String, Int] = calls.groupMapReduce(identity)(_ => 1)(_ + _)
  }

  private def run(listings: Seq[Listing], lookups: IdentityLookups,
                  mutation: IdentityResolver.Mutation = IdentityResolver.Mutation.None): Resolution =
    IdentityResolver.resolveWith(listings, lookups, normalizer, weights, mutation)

  /** A presentation: a permutation, or on every third seed a SPLIT arrival. */
  private def presentations(listings: Seq[Listing]): Seq[(String, Seq[Listing], Seq[Listing])] = Arrivals.map { s =>
    val rnd      = new Random(s)
    val shuffled = rnd.shuffle(listings)
    if (s % 3 == 2) {
      val cut = if (shuffled.size < 2) 0 else 1 + rnd.nextInt(shuffled.size - 1)
      (s"split@$cut/$s", shuffled.take(cut), shuffled.drop(cut) ++ rnd.shuffle(shuffled.take(cut)))
    } else (s"perm/$s", Nil, shuffled)
  }

  private def signature(r: Resolution) =
    r.decisions.map(d => (d.listings, d.film, math.round(d.confidence * 1e9), d.basis)).toSet

  /** The first presentation whose outcome differs from the sorted one, per corpus. */
  private def orderViolation(mutation: IdentityResolver.Mutation, lookupsToo: Boolean): Option[String] =
    Seeds.iterator.flatMap { seed =>
      val c = corpus(seed)
      val refLookups = new Recording(c.lookups)
      val reference  = run(c.listings.sortBy(_.sortKey), refLookups, mutation)
      presentations(c.listings).iterator.flatMap { case (label, first, whole) =>
        if (first.nonEmpty) run(first, c.lookups, mutation)
        val rec = new Recording(c.lookups)
        val got = run(whole, rec, mutation)
        if (lookupsToo && rec.multiset != refLookups.multiset) Some(s"seed $seed $label: lookup multiset differs")
        else Option.when(!lookupsToo && signature(got) != signature(reference))(s"seed $seed $label: outcome differs")
      }.nextOption()
    }.nextOption()

  "A1" should "issue the same multiset of lookups for 21 presentations of 40 generated corpora" in {
    orderViolation(IdentityResolver.Mutation.None, lookupsToo = true) shouldBe None
  }

  it should "catch lookups skipped lazily in arrival order (mutation)" in {
    orderViolation(IdentityResolver.Mutation.LazyLookups, lookupsToo = true) should not be empty
  }

  "P1" should "decide the same clusters, films and confidences for 21 presentations of 40 generated corpora" in {
    orderViolation(IdentityResolver.Mutation.None, lookupsToo = false) shouldBe None
  }

  it should "catch first-wins conflict resolution (mutation)" in {
    orderViolation(IdentityResolver.Mutation.FirstWins, lookupsToo = false) should not be empty
  }

  "P2" should "be a fixpoint: a second resolve of the same set changes no decision and no id" in {
    Seeds.foreach { seed =>
      val c = corpus(seed)
      val (a, b) = (run(c.listings, c.lookups), run(c.listings.reverse, c.lookups))
      signature(a) shouldBe signature(b)
      val ids1 = IdAssigner.fresh(a.decisions.map(_.listings.toSet))
      val ids2 = IdAssigner.assign(ids1.ids, b.decisions.map(_.listings.toSet), ids1.nextFresh)
      IdAssigner.listingIdChanges(ids1, ids2) shouldBe 0
      ids2.ids shouldBe ids1.ids
    }
  }

  "P3" should "never put a cannot-linked pair in one cluster" in {
    Seeds.foreach { seed =>
      val c = corpus(seed)
      val r = run(c.listings, c.lookups)
      r.violations shouldBe 0
      val keyOf = c.listings.map(l => l.sortKey -> l.key).toMap
      r.edges.filterNot(_.must).foreach { e =>
        withClue(s"seed $seed ${e.reason}: ") {
          r.decisionOf(keyOf(e.a)).listings should not contain keyOf(e.b)
        }
      }
    }
  }

  "A3 families" should "resolve each family alone exactly as the whole set" in {
    Seeds.foreach { seed =>
      val c = corpus(seed)
      val global = run(c.listings, c.lookups)
      val scoped = c.listings.groupBy(l => global.familyOf(l.key)).values.flatMap(ls => run(ls, c.lookups).decisions)
      scoped.map(d => (d.listings.toSet, d.film)).toSet shouldBe global.decisions.map(d => (d.listings.toSet, d.film)).toSet
    }
  }

  it should "refuse to resolve when families are narrower than the edge rules (mutation)" in {
    val refused = Seeds.count { seed =>
      val c = corpus(seed)
      scala.util.Try(run(c.listings, c.lookups, IdentityResolver.Mutation.NarrowFamilies)).failed.toOption
        .exists(_.isInstanceOf[IdentityResolver.FamilyCrossing])
    }
    refused should be > 0
  }

  "Every decision" should "explain itself in printable text: no control character reaches a report or a log" in {
    // A NUL in an explanation made logs binary, and `grep` (ugrep -I) then skipped them silently —
    // a green shadow run read as one that exited mid-pass.
    Seeds.foreach { seed =>
      val c = corpus(seed)
      run(c.listings, c.lookups).decisions.flatMap(d => d.explanation :+ d.render).foreach { line =>
        withClue(line.replace('\u0000', '␀'))(line.exists(ch => Character.isISOControl(ch) && ch != '\n') shouldBe false)
      }
    }
  }

  "The resolver on generated data" should "keep every film's listings mostly together and never join two known films" in {
    var (pairs, together, wrongJoins) = (0L, 0L, 0L)
    Seeds.foreach { seed =>
      val c = corpus(seed)
      val r = run(c.listings, c.lookups)
      val keys = c.listings.map(_.key)
      for (a <- keys; b <- keys if ListingKey.ordering.lt(a, b)) {
        val same = r.decisionOf(a) eq r.decisionOf(b)
        if (c.truth(a) == c.truth(b)) { pairs += 1; if (same) together += 1 }
        else if (same) wrongJoins += 1
      }
    }
    info(s"same-film pairs kept together: $together of $pairs; different-film pairs joined: $wrongJoins")
    together.toDouble / pairs should be > 0.5
  }
}
