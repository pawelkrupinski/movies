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

  "a listing's measures against a film" should "be the map they were, held in slots, without a hash map and tuples per pair" in {
    import scala.collection.immutable.HashMap
    val pairs = Seq(listing -> film, listing.copy(year = None, directors = Nil) -> film.copy(directors = None, popularity = Some(12.0)),
      Listing("Diuna: Część druga", year = Some(2024)) -> Film("Dune: Part Two", originalTitle = Some("Dune: Part Two"), year = Some(2024)),
      Listing("Gone With The Wind (2026)") -> Film("Gone with the Wind", year = Some(1939), directors = Some(Seq("Victor Fleming"))))
    pairs.foreach { case (l, f) =>
      val m   = IdentityMeasures.listingFilm(l, f, searchRank = Some(2), rivals = 1, corroboratingVenues = 4)
      val ref = HashMap.from(m.iterator)
      m shouldBe ref
      ref shouldBe m
      m.hashCode shouldBe ref.hashCode
      m.keySet shouldBe ref.keySet
      ref.keysIterator.foreach(k => (m.get(k), m.getOrElse(k, null), m(k), m.contains(k)) shouldBe ((ref.get(k), ref(k), ref(k), true)))
      m.get("nothing") shouldBe None
      m.getOrElse("nothing", null) shouldBe null
      (m + ("director" -> IdentityMeasures.Category("same_person"))) shouldBe (ref + ("director" -> IdentityMeasures.Category("same_person")))
      (m + ("other" -> IdentityMeasures.Category("x"))) shouldBe (ref + ("other" -> IdentityMeasures.Category("x")))
      (m ++ IdentityMeasures.PublishedYear.map(_ -> IdentityMeasures.MissingListing)) shouldBe (ref ++ IdentityMeasures.PublishedYear.map(_ -> IdentityMeasures.MissingListing))
      m.removed("rivals") shouldBe ref.removed("rivals")
      m.filterNot(_._1 == "year.delta") shouldBe ref.filterNot(_._1 == "year.delta")
      IdentityMeasures.comparedFacts(ListingFilm, m) shouldBe IdentityMeasures.comparedFacts(ListingFilm, ref)
      model.probability(ListingFilm, m) shouldBe model.probability(ListingFilm, ref)
    }
    val title = IdentityMeasures.titleRelation(listing, film)
    bytesPerCall(() => IdentityMeasures.listingFilmTitled(listing, film, Some(1), 2, 3, title).size.toDouble) should be < 2000.0   // was 3,600: the hash map, its nodes and an entry tuple each
  }

  "a film's director credited by its house's name" should "be read from tokens worked out once per listing and film" in {
    val met     = Listing("The Metropolitan Opera: Così fan tutte", year = Some(2026), directors = Seq("The Metropolitan Opera", "Phelim McDermott"))
    val opera   = Film("The Metropolitan Opera: Così fan tutte", year = Some(2026), directors = Some(Seq("Phelim McDermott")))
    val alone   = met.copy(directors = Seq("The Metropolitan Opera"))
    IdentityMeasures.listingFilm(met, opera, Some(1), 0, 0)("director") shouldBe IdentityMeasures.Category("same_person")
    IdentityMeasures.listingFilm(alone, opera, Some(1), 0, 0)("director") shouldBe IdentityMeasures.MissingListing
    val title = IdentityMeasures.titleRelation(met, opera)
    bytesPerCall(() => IdentityMeasures.listingFilmTitled(met, opera, Some(1), 0, 0, title).size.toDouble) should be < 6500.0   // was 8,469: every name and title tokenised per pair
  }

  "a credit in Latin letters weighed against one in another script" should "be read as it is, not transliterated" in {
    val bi    = Film("Kaili Blues", year = Some(2015), directors = Some(Seq("毕赣")))
    val names = Seq("Bi Gan", "Mikhail Ivanov", "Pedro Almodóvar", "Ivanov, Petrov", "Иван Петров", "")
    names.foreach { name =>
      val credits = new IdentityMeasures.Credits(Seq(name))
      credits.inLatin.names shouldBe credits.names.map(n => com.ibm.icu.text.Transliterator.getInstance("Any-Latin; Latin-ASCII").transliterate(n))
    }
    IdentityMeasures.listingFilm(Listing("Kaili Blues", directors = Seq("Bi Gan")), bi, Some(1), 0, 0)("director") shouldBe
      IdentityMeasures.Category("same_person")
    bytesPerCall(() => new IdentityMeasures.Credits(Seq("Mikhail Ivanov")).inLatin.names.size.toDouble) should be < 512.0
  }

  "two titles one typo apart" should "be told without a vector of their differing words per pair, to the same answer" in {
    import services.movies.TitleContainment.tokens
    def reference(a: Seq[String], b: Seq[String]): Boolean = a.size >= 2 && a.size == b.size && a != b && {
      val differing = a.indices.filter(i => a(i) != b(i))
      differing.sizeIs == 1 && IdentityMeasures.oneTypoApart(Seq("x", a(differing.head)), Seq("x", b(differing.head)))
    }
    val titles = Seq("Mission Impossible", "Mision Impossible", "Mission Impossibile", "Scary Movie 3", "Scary Movie 4", "Hunt Club", "Hurt Club",
      "Lalka", "Lalkar", "Mission Impossible II", "Mission Impossible III", "Kaili Blues", "Kaila Blues", "Another Round", "Anothre Rounds")
    for (a <- titles; b <- titles) withClue(s"$a / $b: ")(IdentityMeasures.oneTypoApart(tokens(a), tokens(b)) shouldBe reference(tokens(a), tokens(b)))
    IdentityMeasures.oneTypoApart(tokens("Kaili Blues"), tokens("Kaila Blues")) shouldBe true
    IdentityMeasures.oneTypoApart(tokens("Scary Movie 3"), tokens("Scary Movie 4")) shouldBe false
    IdentityMeasures.oneTypoApart(tokens("Another Round"), tokens("Anothre Rounds")) shouldBe false
    val (a, b) = (tokens("The Lord of the Rings Return of the King"), tokens("The Lord of the Rings Return of the Kings"))
    bytesPerCall(() => if (IdentityMeasures.oneTypoApart(a, b)) 1.0 else 0.0) should be < 64.0
  }

  // Every projection reads the model's listings (~100k on worker-us) for the keys it holds and to adopt its objects; as a
  // list, a 24-byte cell each per projection, ~2.4 MB held for the projection's length and promoted (heap dump
  // 2026-10-05: IdentityProjection.Resolved.modelled was a top holder of dead list cells in the old generation).
  "the model's listings" should "be handed over as an array, the same listings" in {
    val corpus  = GeneratedIdentityCorpus.generate(3L, services.movies.SingleCountryNormalizer.titleNormalizer, films = 12, listings = 48)
    val model   = new IncrementalResolver(corpus.lookups, services.movies.SingleCountryNormalizer.titleNormalizer, IdentityCalibration.resolver)
    model.listingsSeen(corpus.listings)
    val listings = model.listings
    listings shouldBe a[scala.collection.immutable.ArraySeq[?]]
    listings.map(_.key).toSet shouldBe corpus.listings.map(_.key).toSet
  }

  "a rival fitting a listing's facts better" should "be judged by sums in place, to the same answer" in {
    val weights = new EvidenceWeights(model)
    val lynch   = Listing("Mulholland Drive", year = Some(2001), directors = Seq("David Lynch"), runtime = Some(147))
    def scored(film: Film) = { val m = IdentityMeasures.listingFilm(lynch, film, Some(1), 1, 0)
      Scored(Candidate(film.hashCode, film), model.probability(ListingFilm, m), m, denial = None, lynch, Some(1)) }
    val top    = scored(Film("Mulholland Drive", year = Some(2001), directors = None, runtime = None))
    val rivals = Seq(scored(Film("Mulholland Drive", year = Some(2001), directors = Some(Seq("David Lynch")), runtime = Some(146))),
      scored(Film("Mulholland Dr.", year = Some(1999), directors = Some(Seq("Someone Else")))), top)
    def reference(rival: Scored): Boolean =
      if (rival.titleNamesIt) weights.own(rival) > weights.own(top)
      else {
        val unanswered = top.measures.collect { case (name, IdentityMeasures.MissingFilm) => name }.toSet
        def answered(s: Scored) = weights.ownContributions(s.measures.filterNot { case (name, _) => unanswered(name) })
        answered(rival) > answered(top)
      }
    rivals.foreach(r => weights.fitsBetter(r, top) shouldBe reference(r))
    val rival = rivals(1)
    bytesPerCall(() => if (weights.fitsBetter(rival, top)) 1.0 else 0.0) should be < 1000.0   // was 10,412
  }
}
