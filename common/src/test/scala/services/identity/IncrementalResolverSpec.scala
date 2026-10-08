package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.LoneElement
import services.movies.{ListingKey, SingleCountryNormalizer}

import tools.IndependentCases

import scala.collection.mutable
import scala.util.Random

/**
 * The incremental resolver against the reference: after EVERY event of a random sequence —
 * listings arriving in batches, listings leaving, gaps in the lookups closing as the fill answers
 * them — the model it keeps equals `IdentityResolver.resolve` of the listings it holds, over the
 * lookups as they are then. P1 (a resolve is a function of the set) is what makes that the whole
 * contract: the order events arrive in is not an input.
 */
class IncrementalResolverSpec extends AnyFlatSpec with Matchers with LoneElement {

  private val normalizer  = SingleCountryNormalizer.titleNormalizer
  private val calibration = IdentityCalibration.fromResource("services/identity/test-calibration.json").get
  private val Seeds       = 1L to 24L


  /** The first event after which the model differs from a resolve of what it holds. */
  private def divergence(seed: Long, mutation: IncrementalResolver.Mutation = IncrementalResolver.Mutation.None,
                         regionBatch: Int = IncrementalResolver.RegionBatch): Option[String] = {
    val corpus  = GeneratedIdentityCorpus.generate(seed, normalizer, films = 12, listings = 48)
    val lookups = new FillingLookups(corpus.lookups, new Random(seed * 31))
    val model   = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None,
      regionBatch = regionBatch, mutation = mutation)
    val random  = new RandomIdentityEvents(corpus.listings, lookups, seed)
    random.events.zipWithIndex.flatMap { case (event, step) =>
      event match {
        case IdentityEvent.Seen(listings) => model.listingsSeen(listings)
        case IdentityEvent.Gone(keys)     => model.listingsGone(keys)
        case IdentityEvent.Answered(c)    => model.answersChanged(c)
      }
      val expected = IdentityResolver.resolveWith(random.held.values, lookups, normalizer, calibration, IdentityResolver.Mutation.None)
      Option.when(ResolutionSignature.of(model) != ResolutionSignature.of(expected))(s"seed $seed, step $step (${event.label})")
    }.nextOption()
  }

  "the incremental resolver" should "equal a resolve of what it holds after every event of a random sequence" in {
    IndependentCases.flatMap(Seeds)(divergence(_)) shouldBe empty
  }

  it should "equal it too when every update resolves in batches of a few listings" in {
    IndependentCases.flatMap(Seeds)(divergence(_, regionBatch = 3)) shouldBe empty
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
      Option.when(ResolutionSignature.of(alone)._1 != ResolutionSignature.of(whole)._1.filter(_._1.forall(mine)))(keys.map(_.toString).mkString(", "))
    }.nextOption()
  }

  private val crossFamily: Seq[(String, Seq[Listing], IdentityLookups)] =
    CrossFamilyCorpora.all(normalizer).filter(_.decides).map(corpus => (corpus.label, corpus.listings, corpus.lookups))

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
        Option.when(ResolutionSignature.of(model) != ResolutionSignature.of(expected))(s"order ${order.mkString}: $label")
      }
      order.iterator.flatMap { i => groups(i).foreach(l => held(l.key) = l); model.listingsSeen(groups(i)); check(s"+$i") } ++
        order.reverse.iterator.flatMap { i => held --= groups(i).map(_.key); model.listingsGone(groups(i).map(_.key)); check(s"-$i") }
    }.nextOption()
  }

  it should "stay equal as a cross-family case's families arrive and leave in every order" in {
    IndependentCases.flatMap(crossFamily) { case (label, listings, lookups) =>
      arrivals(listings, lookups, IncrementalResolver.Mutation.None).map(s"$label: " + _) } shouldBe empty
  }

  it should "be caught on a cross-family case when it ignores the corpus context (the teeth)" in {
    crossFamily.filter { case (_, listings, lookups) => arrivals(listings, lookups, IncrementalResolver.Mutation.IgnoreContext).isEmpty }
      .map(_._1) shouldBe empty
  }

  /** The events of a random sequence merged into batches — consecutive events whose listings do not
   *  clash — the model equal to a resolve of what it holds after each batch. */
  private def batchedDivergence(seed: Long): Option[String] = {
    val corpus  = GeneratedIdentityCorpus.generate(seed, normalizer, films = 12, listings = 48)
    val lookups = new FillingLookups(corpus.lookups, new Random(seed * 31))
    val model   = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None)
    val random  = new RandomIdentityEvents(corpus.listings, lookups, seed)
    val events  = random.events.map(event => event -> random.held.toMap).toSeq
    val batches = events.foldLeft(Vector.empty[Vector[(IdentityEvent, Map[ListingKey, Listing])]]) { case (acc, next) =>
      def keys(event: IdentityEvent): Set[ListingKey] = event match {
        case IdentityEvent.Seen(ls) => ls.map(_.key).toSet
        case IdentityEvent.Gone(ks) => ks.toSet
        case _                      => Set.empty
      }
      acc.lastOption match {
        case Some(open) if open.size < 3 && !open.exists(e => (keys(e._1) intersect keys(next._1)).nonEmpty) => acc.init :+ (open :+ next)
        case _ => acc :+ Vector(next)
      }
    }
    batches.zipWithIndex.iterator.flatMap { case (batch, index) =>
      val seen     = batch.collect { case (IdentityEvent.Seen(ls), _) => ls }.flatten
      val gone     = batch.collect { case (IdentityEvent.Gone(ks), _) => ks }.flatten
      val answered = batch.collect { case (IdentityEvent.Answered(c), _) => c }
      model.batch(seen, gone, AnswersChanged(answered.flatMap(_.queries).toSet, answered.flatMap(_.films).toSet))
      val expected = IdentityResolver.resolveWith(batch.last._2.values, lookups, normalizer, calibration, IdentityResolver.Mutation.None)
      Option.when(ResolutionSignature.of(model) != ResolutionSignature.of(expected))(s"seed $seed, batch $index (${batch.map(_._1.label).mkString(", ")})")
    }.nextOption()
  }

  it should "equal a resolve of what it holds after every batch of events" in {
    IndependentCases.flatMap(Seeds)(batchedDivergence) shouldBe empty
  }

  it should "equal a resolve when it takes a corpus whole, component by component, and after events that follow" in {
    val cases = Seeds.map(seed => (s"seed $seed", GeneratedIdentityCorpus.generate(seed, normalizer, films = 12, listings = 48)))
      .map { case (label, c) => (label, c.listings, c.lookups) } ++ crossFamily
    IndependentCases.flatMap(cases) { case (label, listings, lookups) =>
      val model = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None)
      def differs(expected: Iterable[Listing]) =
        ResolutionSignature.of(model) != ResolutionSignature.of(IdentityResolver.resolveWith(expected, lookups, normalizer, calibration, IdentityResolver.Mutation.None))
      val (half, rest) = listings.splitAt(listings.size / 2)
      model.seed(listings)
      val seeded = Option.when(differs(listings))(s"$label: seeded")
      model.listingsGone(half.map(_.key))
      val left   = Option.when(differs(rest))(s"$label: after half left")
      model.listingsSeen(half)
      val back   = Option.when(differs(listings))(s"$label: after half came back")
      Seq(seeded, left, back).flatten
    } shouldBe empty
  }

  it should "decide as a whole resolve when a full build splits the corpus into small batches" in {
    val cases = Seeds.map(seed => (s"seed $seed", GeneratedIdentityCorpus.generate(seed, normalizer, films = 12, listings = 48)))
      .map { case (label, c) => (label, c.listings, c.lookups) } ++ crossFamily
    IndependentCases.flatMap(cases) { case (label, listings, lookups) =>
      Seq(1, 3, 7).flatMap { batch =>
        val model = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, regionBatch = batch)
        model.seed(listings)
        Option.when(ResolutionSignature.of(model) != ResolutionSignature.of(
          IdentityResolver.resolveWith(listings, lookups, normalizer, calibration, IdentityResolver.Mutation.None)))(s"$label, batch $batch")
      }
    } shouldBe empty
  }

  it should "take a corpus whole resolving each of its families about once" in {
    val corpus = GeneratedIdentityCorpus.generate(7L, normalizer, films = 12, listings = 48)
    val model  = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None)
    model.seed(corpus.listings)
    val whole  = IdentityResolver.resolveWith(corpus.listings, corpus.lookups, normalizer, calibration, IdentityResolver.Mutation.None)
    ResolutionSignature.of(model) shouldBe ResolutionSignature.of(whole)
    model.familiesResolved should be <= (whole.families * 2)
  }

  "the incremental resolver's resolution" should "carry the whole resolve's decisions and decided films, and leave its families to the model" in {
    val corpus = GeneratedIdentityCorpus.generate(3L, normalizer, films = 12, listings = 48)
    val model  = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None)
    model.seed(corpus.listings)
    val whole  = IdentityResolver.resolveWith(corpus.listings, corpus.lookups, normalizer, calibration, IdentityResolver.Mutation.None)
    ResolutionSignature.of(model.resolution.decisions, model.familyOf) shouldBe ResolutionSignature.of(whole)
    model.resolution.films shouldBe whole.films
    // Built for every projection, which reads decisions alone: a map of every listing's family is not built for it.
    model.resolution.familyOf shouldBe empty
  }

  "the incremental resolver's gaps" should "name what the lookups cannot answer yet, and only what the source never answers once the fill ran dry" in {
    val corpus  = GeneratedIdentityCorpus.generate(4L, normalizer, films = 12, listings = 48)
    val lookups = new FillingLookups(corpus.lookups, new Random(4L))
    val model   = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None)
    model.seed(corpus.listings)
    model.gaps.isEmpty shouldBe false
    Iterator.continually(lookups.answer()).takeWhile(!_.isEmpty).foreach(model.answersChanged)
    // The generated source leaves some questions unanswered for good (its GAPS): only those remain.
    model.gaps.queries.filter(query => corpus.lookups.candidates(query).isKnown) shouldBe empty
    model.gaps.films.filter(id => corpus.lookups.film(id).isKnown) shouldBe empty
    ResolutionSignature.of(model) shouldBe
      ResolutionSignature.of(IdentityResolver.resolveWith(corpus.listings, lookups, normalizer, calibration, IdentityResolver.Mutation.None))
  }

  "a detail page answered anew" should "move its listing as the evidence it adds decides" in {
    import FilmTable.{F, listing}
    import models.{Helios, Multikino}
    val films   = Seq(F(1, "Lalka", 1968, "Wojciech Has", 159), F(2, "Lalka", 2025, "Maciej Kawalski", 112))
    val table   = new FilmTable(films, normalizer)
    var credits = Map.empty[ListingKey, Seq[String]]
    val lookups = new IdentityLookups {
      def hasDetail(l: Listing): Boolean = true
      def detail(l: Listing): Answer[Option[DetailFacts]] = Answer.Known(credits.get(l.key).map(ds => DetailFacts(None, ds, None, None)))
      def candidates(q: CandidateQuery): Answer[Seq[Hit]] = table.candidates(q)
      def film(id: Int): Answer[Option[IdentityMeasures.Film]] = table.film(id)
    }
    val bare  = listing(Helios, "Lalka")
    val dated = listing(Multikino, "Lalka", Some(1968), Some("Wojciech Has"))
    val model = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None)
    model.listingsSeen(Seq(bare, dated))
    def whole = IdentityResolver.resolveWith(Seq(bare, dated), lookups, normalizer, calibration, IdentityResolver.Mutation.None)
    ResolutionSignature.of(model) shouldBe ResolutionSignature.of(whole)
    credits = Map(bare.key -> Seq("Maciej Kawalski"))
    model.answersChanged(AnswersChanged(Set.empty, Set.empty, Set(bare.key)))
    ResolutionSignature.of(model) shouldBe ResolutionSignature.of(whole)
    model.decisions.find(_.listings(bare.key)).flatMap(_.film) shouldBe Some(2)
  }

  // A venue re-scraping the listing unchanged in the same drain as its page's answer: the listing is
  // in `seen` (and dropped there, unchanged) and in the answered details (and dropped there, seen), so
  // it was re-read by neither — Identity model convergence's arrival-order check, where Kino Pod
  // Baranami's 'Caravaggio' kept Jarman's 1986 film in one order and the 2025 documentary in the other.
  it should "move its listing though its venue re-published it unchanged in the same batch" in {
    import FilmTable.{F, listing}
    import models.{Helios, Multikino}
    val films   = Seq(F(1, "Lalka", 1968, "Wojciech Has", 159), F(2, "Lalka", 2025, "Maciej Kawalski", 112))
    val table   = new FilmTable(films, normalizer)
    var credits = Map.empty[ListingKey, Seq[String]]
    val lookups = new IdentityLookups {
      def hasDetail(l: Listing): Boolean = true
      def detail(l: Listing): Answer[Option[DetailFacts]] = Answer.Known(credits.get(l.key).map(ds => DetailFacts(None, ds, None, None)))
      def candidates(q: CandidateQuery): Answer[Seq[Hit]] = table.candidates(q)
      def film(id: Int): Answer[Option[IdentityMeasures.Film]] = table.film(id)
    }
    val bare  = listing(Helios, "Lalka")
    val dated = listing(Multikino, "Lalka", Some(1968), Some("Wojciech Has"))
    val model = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None)
    model.listingsSeen(Seq(bare, dated))
    credits = Map(bare.key -> Seq("Maciej Kawalski"))
    model.batch(Seq(bare), Nil, AnswersChanged(Set.empty, Set.empty, Set(bare.key)))
    model.decisions.find(_.listings(bare.key)).flatMap(_.film) shouldBe Some(2)
  }

  "a season production another venue's search reaches later" should "re-learn the banner's house and move the broadcasts it names" in {
    // Kino 1410's Met broadcasts arrive first and learn Royal Ballet & Opera; the venue whose search
    // reaches the Met's record arrives after, and must move them as the whole resolve does.
    import MetSeasonHouseCase.*
    val seasonLookups = lookups(normalizer)
    val model = new IncrementalResolver(seasonLookups, normalizer, calibration, decorations = TitleDecorations.None)
    model.listingsSeen(Seq(cosi, carmen))
    model.decisions.find(_.listings(carmen.key)).flatMap(_.film) shouldBe Some(RboCarmen)
    model.listingsSeen(Seq(other))
    ResolutionSignature.of(model) shouldBe ResolutionSignature.of(
      IdentityResolver.resolveWith(Seq(cosi, carmen, other), seasonLookups, normalizer, calibration, IdentityResolver.Mutation.None))
    model.decisions.find(_.listings(carmen.key)).flatMap(_.film) should not be Some(RboCarmen)
  }

  "a family's size" should "name its busiest nodes by their published titles, readable in a log line" in {
    val size = IncrementalResolver.FamilySize(3, 2, 4, Seq("Pressure\u0000Pressure\u0000\u0000Anthony Maras\u0000100" -> 2, "Pressure" -> 1))
    size.render shouldBe "3 listings / 2 nodes / 4 keys (Pressure×2, Pressure×1)"
  }

  "an answer filed before its event arrives" should "not fail a re-resolve that reads it, and move the family once its event does" in {
    // Production: the fill files answers while the model drains, so a region's resolve can read a
    // newer answer than the corpus context was told of — here a director walk reaching a film no
    // other question reaches. The answer's own event follows on the next drain.
    import FilmTable.{F, listing}
    import models.{Helios, Multikino}
    val films   = Seq(F(1, "Lalka", 1968, "Wojciech Has", 159), F(2, "Lalka", 2025, "Maciej Kawalski", 112),
                      F(3, "Sanatorium pod Klepsydrą", 1973, "Wojciech Has", 124))
    val table   = new FilmTable(films, normalizer)
    var walked  = false
    val lookups = new IdentityLookups {
      def hasDetail(l: Listing): Boolean = false
      def detail(l: Listing): Answer[Option[DetailFacts]] = Answer.Known(None)
      def candidates(q: CandidateQuery): Answer[Seq[Hit]] = q match {
        case _: CandidateQuery.Director if !walked => Answer.Unknown
        case _                                     => table.candidates(q)
      }
      def film(id: Int): Answer[Option[IdentityMeasures.Film]] = table.film(id)
    }
    val dated = listing(Multikino, "Lalka", Some(1968), Some("Wojciech Has"))
    val bare  = listing(Helios, "Lalka")
    val model = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None)
    model.listingsSeen(Seq(dated))
    walked = true                                                          // filed; its event not yet drained
    noException should be thrownBy model.listingsSeen(Seq(bare))
    model.answersChanged(AnswersChanged(model.gaps.queries, Set.empty))
    ResolutionSignature.of(model) shouldBe ResolutionSignature.of(
      IdentityResolver.resolveWith(Seq(dated, bare), lookups, normalizer, calibration, IdentityResolver.Mutation.None))
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

  "a listing re-published with only another poster, days or names" should "be held in its place, nothing resolved again" in {
    import FilmTable.{F, listing}
    import models.Multikino
    val model = new IncrementalResolver(new FilmTable(Seq(F(1, "Lalka", 1968, "Wojciech Has", 159)), normalizer), normalizer, calibration,
      decorations = TitleDecorations.None)
    val first = listing(Multikino, "Lalka", Some(1968)).copy(poster = Some("https://kino.example/lalka-a.jpg"))
    model.listingsSeen(Seq(first))
    val (resolved, decided) = (model.familiesResolved, model.decisions)
    val recut = listing(Multikino, "Lalka", Some(1968)).copy(poster = Some("https://kino.example/lalka-b.jpg"),
      names = VenueNames.ofHashes(Seq(42)))
    model.listingsSeen(Seq(recut))
    model.familiesResolved shouldBe resolved                    // equal to what it holds: no family resolved again
    model.decisions.zip(decided).forall { case (a, b) => a eq b } shouldBe true
    val held = model.listings.loneElement
    (held.poster, held.names) shouldBe ((recut.poster, recut.names))
    held.key should be theSameInstanceAs first.key              // under the key object its families name
  }

  "a family" should "decide alone, with the corpus's context, exactly as the whole resolve does" in {
    val generated = Seeds.map(seed => GeneratedIdentityCorpus.generate(seed, normalizer, films = 12, listings = 48))
      .map(c => (s"generated", c.listings, c.lookups))
    IndependentCases.flatMap(generated ++ crossFamily) { case (label, listings, lookups) =>
      familyAlone(listings, lookups, withContext = true).map(s"$label: " + _) } shouldBe empty
  }

  it should "decide otherwise alone WITHOUT the context, on each cross-family case (the property's teeth)" in {
    crossFamily.filter { case (_, listings, lookups) => familyAlone(listings, lookups, withContext = false).isEmpty }.map(_._1) shouldBe empty
  }
}
