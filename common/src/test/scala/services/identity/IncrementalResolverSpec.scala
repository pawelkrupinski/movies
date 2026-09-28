package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingKey, SingleCountryNormalizer}

import scala.collection.mutable
import scala.util.Random

/**
 * The incremental resolver against the reference: after EVERY event of a random sequence —
 * listings arriving in batches, listings leaving, gaps in the lookups closing as the fill answers
 * them — the model it keeps equals `IdentityResolver.resolve` of the listings it holds, over the
 * lookups as they are then. P1 (a resolve is a function of the set) is what makes that the whole
 * contract: the order events arrive in is not an input.
 */
class IncrementalResolverSpec extends AnyFlatSpec with Matchers {

  private val normalizer  = SingleCountryNormalizer.titleNormalizer
  private val calibration = IdentityCalibration.fromResource("services/identity/test-calibration.json").get
  private val Seeds       = 1L to 24L

  /** `inner` with some questions not answered yet: `answer` closes them, as a fill round does. */
  private final class Filling(inner: IdentityLookups, rnd: Random) extends IdentityLookups {
    private val openQueries = mutable.HashSet.empty[CandidateQuery]
    private val openFilms   = mutable.HashSet.empty[Int]
    private val seenQueries = mutable.HashSet.empty[CandidateQuery]
    private val seenFilms   = mutable.HashSet.empty[Int]
    // A question is a gap the first time it is seen with probability 0.3, and stays one until answered.
    private def gapQuery(q: CandidateQuery) = { if (seenQueries.add(q) && rnd.nextDouble() < 0.3) openQueries += q; openQueries(q) }
    private def gapFilm(id: Int)            = { if (seenFilms.add(id) && rnd.nextDouble() < 0.3) openFilms += id; openFilms(id) }
    def hasDetail(l: Listing): Boolean = inner.hasDetail(l)
    def detail(l: Listing): Answer[Option[DetailFacts]] = inner.detail(l)
    def candidates(q: CandidateQuery): Answer[Seq[Hit]] = if (gapQuery(q)) Answer.Unknown else inner.candidates(q)
    def film(id: Int): Answer[Option[IdentityMeasures.Film]] = if (gapFilm(id)) Answer.Unknown else inner.film(id)
    /** Answer about half of the open questions; what changed. */
    def answer(): AnswersChanged = {
      val queries = openQueries.toSeq.sorted.filter(_ => rnd.nextBoolean()).toSet
      val films   = openFilms.toSeq.sorted.filter(_ => rnd.nextBoolean()).toSet
      openQueries --= queries; openFilms --= films
      AnswersChanged(queries, films)
    }
  }

  private def signature(decisions: Seq[ResolverDecision], familyOf: Map[ListingKey, Int]) =
    (decisions.map(d => (d.listings, d.film, math.round(d.confidence * 1e9), d.basis)).toSet,
     familyOf.groupMap(_._2)(_._1).values.map(_.toSet).toSet)
  private def signature(r: Resolution): (Set[(Set[ListingKey], Option[Int], Long, ResolverDecision.Basis)], Set[Set[ListingKey]]) =
    signature(r.decisions, r.familyOf)

  /** The first event after which the model differs from a resolve of what it holds. */
  private def divergence(seed: Long, mutation: IncrementalResolver.Mutation = IncrementalResolver.Mutation.None): Option[String] = {
    val corpus  = GeneratedIdentityCorpus.generate(seed, normalizer, films = 12, listings = 48)
    val rnd     = new Random(seed)
    val lookups = new Filling(corpus.lookups, new Random(seed * 31))
    val model   = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, mutation = mutation)
    val held    = mutable.LinkedHashMap.empty[ListingKey, Listing]
    val pending = mutable.Queue.from(rnd.shuffle(corpus.listings))
    Iterator.range(0, 40).flatMap { step =>
      val label = rnd.nextInt(10) match {
        case 0 | 1 | 2 | 3 if pending.nonEmpty =>
          val batch = Seq.fill(1 + rnd.nextInt(6))(()).flatMap(_ => Option.when(pending.nonEmpty)(pending.dequeue()))
          batch.foreach(l => held(l.key) = l); model.listingsSeen(batch); s"seen ${batch.size}"
        case 4 | 5 if held.nonEmpty =>
          val gone = rnd.shuffle(held.keys.toSeq).take(1 + rnd.nextInt(3))
          gone.foreach(held.remove); pending ++= gone.flatMap(key => corpus.listings.find(_.key == key))
          model.listingsGone(gone); s"gone ${gone.size}"
        case _ =>
          val answered = lookups.answer(); model.answersChanged(answered)
          s"answered ${answered.queries.size}+${answered.films.size}"
      }
      val expected = IdentityResolver.resolveWith(held.values, lookups, normalizer, calibration, IdentityResolver.Mutation.None)
      Option.when(signature(model.decisions, model.familyOf) != signature(expected))(s"seed $seed, step $step ($label)")
    }.nextOption()
  }

  "the incremental resolver" should "equal a resolve of what it holds after every event of a random sequence" in {
    Seeds.flatMap(divergence(_)) shouldBe empty
  }

  it should "be caught by the sequence when it never pulls in a family it now shares a key with (the teeth)" in {
    Seeds.forall(seed => divergence(seed, IncrementalResolver.Mutation.NoExpansion).isEmpty) shouldBe false
  }

  /** Each family of a resolve, resolved alone — with the whole corpus's context, or with its own —
   *  and the first family whose decisions differ from the whole resolve's. */
  private def familyAlone(listings: Seq[Listing], lookups: IdentityLookups, withContext: Boolean): Option[String] = {
    val whole   = IdentityResolver.resolveWith(listings, lookups, normalizer, calibration, IdentityResolver.Mutation.None)
    val context = IdentityResolver.contextOf(listings, lookups, normalizer)
    val byKey   = listings.map(l => l.key -> l).toMap
    whole.familyOf.groupMap(_._2)(_._1).values.iterator.flatMap { keys =>
      val alone = IdentityResolver.resolveWith(keys.map(byKey), lookups, normalizer, calibration, IdentityResolver.Mutation.None,
        corpus = Option.when(withContext)(context))
      val mine = keys.toSet
      Option.when(signature(alone)._1 != signature(whole)._1.filter(_._1.forall(mine)))(keys.map(_.toString).mkString(", "))
    }.nextOption()
  }

  /** Families whose decisions hang on what OTHER families hold — the corpus-wide facts
   *  (`CorpusContext`) an incremental model must keep current. Taken from the house incidents
   *  (`IdentityResolverCasesSpec`): a banner's house is learned from its other works' records. */
  private val crossFamily: Seq[(String, Seq[Listing], IdentityLookups)] = {
    import FilmTable.{F, listing}
    import models.{Helios, KinoApollo, Multikino, Rialto}
    val rboFilms = Seq(F(1702782, "Royal Ballet & Opera 2026/27: Swan Lake", 2027, "", 0, 3),
      F(1702778, "Royal Ballet & Opera 2026/27: Alice's Adventures in Wonderland", 2027, "", 0, 3),
      F(1703631, "The Metropolitan Opera 2026/27: Manon", 2027, "", 0, 5),
      F(1703622, "The Metropolitan Opera 2026/27: Macbeth", 2026, "", 0, 5))
    def rbo(work: String) = Seq(Helios, KinoApollo).map(listing(_, s"RBO Cinema Season 2026-27: $work"))
    def met(work: String) = Seq(Multikino, Rialto).map(listing(_, s"Met Opera 2026-27: $work"))
    Seq(
      ("a banner's house learned from its other works (RBO's Manon)",
        rbo("Swan Lake") ++ rbo("Alice's Adventures in Wonderland") ++ rbo("Manon") ++ met("Manon") ++ met("Macbeth"),
        new FilmTable(rboFilms, normalizer)))
  }

  /** The case's families arriving one at a time, in every order (and leaving again): the first
   *  event after which the model differs from a resolve of what it holds. */
  private def arrivals(listings: Seq[Listing], lookups: IdentityLookups, mutation: IncrementalResolver.Mutation): Option[String] = {
    val whole  = IdentityResolver.resolveWith(listings, lookups, normalizer, calibration, IdentityResolver.Mutation.None)
    val byKey  = listings.map(l => l.key -> l).toMap
    val groups = whole.familyOf.groupMap(_._2)(_._1).values.map(_.toSeq.sorted(using ListingKey.ordering).map(byKey)).toSeq
    groups.indices.permutations.iterator.flatMap { order =>
      val model = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, mutation = mutation)
      val held  = mutable.LinkedHashMap.empty[ListingKey, Listing]
      def check(label: String) = {
        val expected = IdentityResolver.resolveWith(held.values, lookups, normalizer, calibration, IdentityResolver.Mutation.None)
        Option.when(signature(model.decisions, model.familyOf) != signature(expected))(s"order ${order.mkString}: $label")
      }
      order.iterator.flatMap { i => groups(i).foreach(l => held(l.key) = l); model.listingsSeen(groups(i)); check(s"+$i") } ++
        order.reverse.iterator.flatMap { i => held --= groups(i).map(_.key); model.listingsGone(groups(i).map(_.key)); check(s"-$i") }
    }.nextOption()
  }

  it should "stay equal as a cross-family case's families arrive and leave in every order" in {
    crossFamily.flatMap { case (label, listings, lookups) =>
      arrivals(listings, lookups, IncrementalResolver.Mutation.None).map(s"$label: " + _) } shouldBe empty
  }

  it should "be caught on a cross-family case when it ignores the corpus context (the teeth)" in {
    crossFamily.filter { case (_, listings, lookups) => arrivals(listings, lookups, IncrementalResolver.Mutation.IgnoreContext).isEmpty }
      .map(_._1) shouldBe empty
  }

  "the incremental resolver's work" should "re-resolve only the families an event can move" in {
    import FilmTable.{F, listing}
    import models.{Helios, Multikino}
    val films = Seq(F(1, "Lalka", 1968, "Wojciech Has", 159), F(2, "Matilda", 1996, "Danny DeVito", 98),
      F(3, "Faust", 1926, "F.W. Murnau", 116))
    val model = new IncrementalResolver(new FilmTable(films, normalizer), normalizer, calibration, decorations = TitleDecorations.None)
    model.listingsSeen(Seq(listing(Multikino, "Lalka", Some(1968)), listing(Helios, "Lalka"), listing(Multikino, "Matilda", Some(1996))))
    val afterTwo = model.familiesResolved
    afterTwo shouldBe 2
    model.answersChanged(AnswersChanged(Set(CandidateQuery.Title("Nothing anyone asked")), Set(999)))
    model.familiesResolved shouldBe afterTwo                    // nothing held asked it: nothing moves
    model.listingsSeen(Seq(listing(Helios, "Faust", Some(1926))))
    model.familiesResolved shouldBe afterTwo + 1                // a film of its own: its family alone
    model.listingsGone(Seq(listing(Helios, "Lalka").key))
    model.familiesResolved shouldBe afterTwo + 2                // Lalka's family, and only it
  }

  "a family" should "decide alone, with the corpus's context, exactly as the whole resolve does" in {
    val generated = Seeds.map(seed => GeneratedIdentityCorpus.generate(seed, normalizer, films = 12, listings = 48))
      .map(c => (s"generated", c.listings, c.lookups))
    (generated ++ crossFamily).flatMap { case (label, listings, lookups) =>
      familyAlone(listings, lookups, withContext = true).map(s"$label: " + _) } shouldBe empty
  }

  it should "decide otherwise alone WITHOUT the context, on each cross-family case (the property's teeth)" in {
    crossFamily.filter { case (_, listings, lookups) => familyAlone(listings, lookups, withContext = false).isEmpty }.map(_._1) shouldBe empty
  }
}
