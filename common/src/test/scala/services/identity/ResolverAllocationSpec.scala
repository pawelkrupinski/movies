package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.identity.IdentityMeasures.{Film, Listing, ListingFilm, ListingListing}

/**
 * The resolver's hottest helpers answer without allocating per call what they used to: on worker-pl the identity model's
 * thread allocated 727 of the worker's 1,084 MB/min (JFR 2026-10-05), most of it boxes, options, tuples and iterators
 * built per measure and thrown away — enough, in a 125 MB eden, to drag its working set into the old generation. Each
 * check also holds the answer to what the allocating form gave.
 */
class ResolverAllocationSpec extends AnyFlatSpec with Matchers {
  private val model = IdentityCalibration.resolver

  /** Bytes `body` allocates per call on this thread — its answer read as a primitive, never boxed — over its first
   *  calls, before the optimising compiler can scalar-replace what it allocates: in the resolver, called through deep
   *  megamorphic paths, it never does (the allocations stood in production's JFR). Once to load what it reads lazily. */
  private def bytesPerCall(body: java.util.function.DoubleSupplier): Double = {
    var sink = 0.0
    sink += body.getAsDouble
    val (_, bytes) = tools.ThreadAllocation.of { var j = 0; while (j < 2000) { sink += body.getAsDouble; j += 1 } }
    if (sink.isNaN) fail("unreachable")
    bytes / 2000.0
  }

  private val listing = Listing("Samson i Dalila", year = Some(2026), directors = Seq("Darko Tresnjak"), runtime = Some(180))
  private val film    = Film("Samson et Dalila", year = Some(2026), directors = Some(Seq("Darko Tresnjak")), runtime = Some(178))
  private val measures = IdentityMeasures.listingFilm(listing, film, searchRank = Some(1), rivals = 2, corroboratingVenues = 3)
  private val pairMeasures = IdentityMeasures.listingListing(listing, listing.copy(title = "Samson i Dalila (MET Opera)"), sameVenue = false,
    sharedChainId = None)

  "a calibrated score" should "be summed without a box, option or tuple per signal, to the same log-odds" in {
    Seq(ListingFilm -> measures, ListingListing -> pairMeasures).foreach { case (scope, m) =>
      model.logOdds(scope, m) shouldBe (model.scopes(scope).prior + model.contributions(scope, m).map(_._2).sum)
      model.contributionSum(scope, m) shouldBe model.contributions(scope, m).map(_._2).sum
      withClue(s"$scope: ")(bytesPerCall(() => model.logOdds(scope, m)) should be < 8.0)
      withClue(s"$scope: ")(bytesPerCall(() => model.contributionSum(scope, m)) should be < 8.0)
    }
  }

  "the constraint solver's join check" should "ask its cannot-links without an option per call, to the same answer" in {
    val cannot = scala.collection.mutable.HashMap(1 -> scala.collection.mutable.Set(2, 3), 2 -> scala.collection.mutable.Set(1))
    val film   = scala.collection.mutable.HashMap(4 -> 10, 5 -> 11, 6 -> 10)
    ConstraintSolver.forbids(cannot, film, 1, 2) shouldBe true
    ConstraintSolver.forbids(cannot, film, 1, 4) shouldBe false
    ConstraintSolver.forbids(cannot, film, 4, 5) shouldBe true
    ConstraintSolver.forbids(cannot, film, 4, 6) shouldBe false
    ConstraintSolver.forbids(cannot, film, 7, 8) shouldBe false
    bytesPerCall(() => if (ConstraintSolver.forbids(cannot, film, 1, 4)) 1.0 else 0.0) should be < 8.0
  }

  "a title token run" should "be judged without an iterator per call, as startsWith and endsWith judge it" in {
    import services.movies.TitleContainment.{isTokenRun, tokens}
    val cases = Seq("Diuna" -> "Diuna część druga", "Diuna" -> "Kino na leżakach: Diuna", "Diuna" -> "Diuna", "Obcy" -> "Ósmy pasażer Nostromo",
      "Dzień" -> "Najdłuższy dzień w roku", "Lalka" -> "Lalka 2D PL", "Lalka 2D" -> "Lalka 2D PL", "" -> "Lalka")
    for ((a, b) <- cases; (x, y) <- Seq(tokens(a) -> tokens(b), tokens(b) -> tokens(a), tokens(a).toVector -> tokens(b).toVector,
                                       tokens(a).toVector -> tokens(b))) {
      withClue(s"$x / $y: ")(isTokenRun(x, y) shouldBe (x.nonEmpty && y.lengthIs > x.length && (y.startsWith(x) || y.endsWith(x))))
    }
    val (base, whole) = (tokens("Diuna"), tokens("Kino na leżakach: Diuna"))
    bytesPerCall(() => if (isTokenRun(base, whole)) 1.0 else 0.0) should be < 8.0
  }

  "a candidate's own evidence" should "be summed without a tuple or box per signal, to the same sums" in {
    val weights = new EvidenceWeights(model)
    val own     = model.contributions(ListingFilm, measures).collect { case (name, weight) if !weights.Priors(name) => weight }.sum
    weights.ownContributions(measures) shouldBe own
    val veto    = model.contributions(ListingFilm, measures).collect { case (name, weight) if !weights.Priors(name) && weight >= 0 => weight }.sum
    model.contributionSumWhere(ListingFilm, measures, (name, weight) => !weights.Priors(name) && weight >= 0) shouldBe veto
    model.contributionSumWhere(ListingFilm, measures, (name, _) => name == "nothing") shouldBe 0.0
    val lent    = model.contributions(ListingFilm, measures).map { case (name, w) => if (weights.Priors(name)) math.max(0.0, w) else w }.sum
    model.contributionSumWhere(ListingFilm, measures, (_, _) => true, (name, w) => if (weights.Priors(name)) math.max(0.0, w) else w) shouldBe lent
    bytesPerCall(() => weights.ownContributions(measures)) should be < 32.0
    bytesPerCall(() => weights.factsProbability(measures)) should be < 96.0
  }

  "each listing's film id" should "be one hash table built to size, the same ids, with nothing copied as it grows" in {
    import services.movies.ListingKey
    val ids = (1L to 500L).map(id => id -> (1 to 40).map(i => ListingKey.Native(s"Venue $i", s"https://v/$id/$i", s"Film $id"): ListingKey).toSet)
    val reference = ids.flatMap { case (id, ls) => ls.map(_ -> id) }.toMap
    val built     = IdAssigner.Assignment(ids, 501L).idOfListing
    built shouldBe reference
    reference shouldBe built
    built.get(ListingKey.Native("nowhere", "x", "y")) shouldBe None
    built.count { case (l, id) => reference.get(l).exists(_ != id) } shouldBe 0
    val (_, bytes) = tools.ThreadAllocation.of(IdAssigner.Assignment(ids, 501L).idOfListing.size)
    withClue(s"$bytes bytes for 20,000 listings: ")(bytes should be < 1600000L)
  }

  "an accent fold of a title already ASCII" should "be the title itself, asked without a stream per call" in {
    import tools.TextNormalization.deburr
    deburr("The Brutalist") should be theSameInstanceAs "The Brutalist"
    deburr("Łódź Żółć") shouldBe "lodz Zolc"
    deburr("Amélie") shouldBe "Amelie"
    deburr("") shouldBe ""
    deburr("\u007f") shouldBe "\u007f"
    deburr("\u0080x") shouldBe "\u0080x"
    val ascii = "Kino na leżakach".filter(_ < 0x80)
    bytesPerCall(() => deburr(ascii).length.toDouble) should be < 8.0
  }

  "a listing's search groups against a film" should "key each title once, not per pair, to the same groups" in {
    val many  = film.copy(alternativeTitles = (1 to 30).map(i => s"Samson und Dalila Fassung $i") :+ "Samson i Dalila")
    val asked = listing.copy(searchTitles = Seq("Samson i Dalila", "Samson et Dalila", "Opera"))
    def reference(l: Listing, f: Film) = {
      val titles = (f.title +: (f.originalTitle.toSeq ++ f.alternativeTitles)).map(IdentityMeasures.key).toSet
      l.searchTitles.map(IdentityMeasures.key).filter(titles).distinct
    }
    Seq(asked -> many, asked -> film, listing -> many).foreach { case (l, f) => IdentityMeasures.searchGroups(l, f) shouldBe reference(l, f) }
    bytesPerCall(() => IdentityMeasures.searchGroups(asked, many).size.toDouble) should be < 512.0   // was 11,344
  }
}
