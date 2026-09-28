package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

import scala.util.Random

/** The corpus context kept current event by event equals the one derived whole from the listings
 *  held, on every key, after every event of a random sequence. */
class LiveCorpusSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val Seeds      = 1L to 24L

  /** Every key the whole context holds, read through both. */
  private def differences(live: LiveCorpus, held: Iterable[Listing], lookups: IdentityLookups): Seq[String] = {
    val whole = new CandidateGeneration(held.toSeq.sorted.distinctBy(_.key), lookups, normalizer, PinConstraints(Nil), TitleDecorations.None, lazyLookups = false)
    val reads = CorpusContext.Reads.of(whole.nodes, whole.recorded.values.map(_.film).toSeq, whole.recorded.keySet, whole.answers.keySet.toSet, normalizer.sanitize)
    val (a, b) = (live.slice(reads), whole.context.slice(reads))
    Seq(
      Option.when(a.candidates != b.candidates)(s"candidates ${(a.candidates.toSet diff b.candidates.toSet).take(2)} vs ${(b.candidates.toSet diff a.candidates.toSet).take(2)}"),
      Option.when(a.houses != b.houses)(s"houses ${a.houses} vs ${b.houses}"),
      Option.when(a.groups != b.groups)("title groups"),
      Option.when(a.reached != b.reached)(s"reached ${(a.reached.toSet diff b.reached.toSet).take(2)} vs ${(b.reached.toSet diff a.reached.toSet).take(2)}"),
      Option.when(a.wholeTitles != b.wholeTitles)("whole titles"),
      Option.when(a.answers != b.answers)(s"answers ${(a.answers.toSet diff b.answers.toSet).take(2)} vs ${(b.answers.toSet diff a.answers.toSet).take(2)}"),
      Option.when(a.digest != b.digest)("digest"),
      Option.when(live.houseRanking != whole.context.houseRanking)("house ranking"),
      Option.when(live.candidateCount != whole.recorded.size)(s"${live.candidateCount} candidates held, ${whole.recorded.size} recorded")
    ).flatten
  }

  private def divergence(seed: Long): Option[String] = {
    val corpus = GeneratedIdentityCorpus.generate(seed, normalizer, films = 12, listings = 48)
    divergence(s"seed $seed", corpus.listings, corpus.lookups, seed)
  }

  private def divergence(label: String, listings: Seq[Listing], inner: IdentityLookups, seed: Long): Option[String] = {
    val lookups = new FillingLookups(inner, new Random(seed * 31))
    val live    = new LiveCorpus(lookups, normalizer, PinConstraints(Nil), TitleDecorations.None)
    val random  = new RandomIdentityEvents(listings, lookups, seed)
    random.events.zipWithIndex.flatMap { case (event, step) =>
      event match {
        case IdentityEvent.Seen(listings) => live.seen(listings)
        case IdentityEvent.Gone(keys)     => live.gone(keys)
        case IdentityEvent.Answered(c)    => live.answered(c)
      }
      differences(live, random.held.values, lookups).headOption.map(d => s"$label, step $step (${event.label}): $d")
    }.nextOption()
  }

  "the live corpus context" should "equal the whole one on every key after every event" in {
    Seeds.flatMap(divergence) shouldBe empty
  }

  it should "equal it on the corpora whose families read each other's facts" in {
    (for { corpus <- CrossFamilyCorpora.all(normalizer); seed <- Seeds }
      yield divergence(s"${corpus.label} / $seed", corpus.listings, corpus.lookups, seed)).flatten shouldBe empty
  }
}
