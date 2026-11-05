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
        case IdentityEvent.Seen(listings) => model.batch(seen = listings)
        case IdentityEvent.Gone(keys)     => model.batch(gone = keys)
        case IdentityEvent.Answered(c)    => model.batch(answered = c)
      }
      val expected = IdentityResolver.resolveWith(random.held.values, lookups, normalizer, calibration, IdentityResolver.Mutation.None)
      Option.when(ResolutionSignature.of(model) != ResolutionSignature.of(expected))(s"seed $seed, step $step (${event.label})")
    }.nextOption()
  }

  "the incremental resolver" should "equal a resolve of what it holds after every event of a random sequence" in {
    IndependentCases.flatMap(Seeds)(divergence(_)) shouldBe empty
  }

  /** What a stage after the model reads of each decision beside its take (docs/design/identity-no-drift.md): the film it
   *  leans to, its best candidate, its questions unanswered and the trace it keeps ([[DecisionTrace.kept]]). */
  private def handed(decisions: Seq[ResolverDecision]) =
    decisions.map(d => (d.members.toSet, d.leaning, d.candidate, d.unanswered, DecisionTrace.kept(d), d.trace == DecisionTrace.kept(d))).toSet

  /** The first event after which the model hands the stage other decisions than a resolve of what it holds would. */
  private def handedDivergence(seed: Long): Option[String] = {
    val corpus  = GeneratedIdentityCorpus.generate(seed, normalizer, films = 12, listings = 48)
    val lookups = new FillingLookups(corpus.lookups, new Random(seed * 31))
    val store   = new InMemoryIdentityModelStore
    var model   = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store)
    val random  = new RandomIdentityEvents(corpus.listings, lookups, seed)
    random.events.zipWithIndex.flatMap { case (event, step) =>
      event match {
        case IdentityEvent.Seen(listings) => model.batch(seen = listings)
        case IdentityEvent.Gone(keys)     => model.batch(gone = keys)
        case IdentityEvent.Answered(c)    => model.batch(answered = c)
      }
      // a restart now and then: what the store keeps of a decision is what the stage reads of it
      if (step % 7 == 6) { model = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, store = store); model.restore(random.held.values.toSeq) }
      val expected = IdentityResolver.resolveWith(random.held.values, lookups, normalizer, calibration, IdentityResolver.Mutation.None)
      val (got, want) = (handed(model.decisions), handed(expected.decisions).map(h => h.copy(_6 = true)))
      Option.when(got != want)(s"seed $seed, step $step (${event.label}): ${(got -- want).take(1)} vs ${(want -- got).take(1)}")
    }.nextOption()
  }

  it should "hand a stage after it what a resolve of what it holds hands it — leaning, candidate, unanswered, kept trace — restarted too" in {
    IndependentCases.flatMap(Seeds)(handedDivergence) shouldBe empty
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
      order.iterator.flatMap { i => groups(i).foreach(l => held(l.key) = l); model.batch(seen = groups(i)); check(s"+$i") } ++
        order.reverse.iterator.flatMap { i => held --= groups(i).map(_.key); model.batch(gone = groups(i).map(_.key)); check(s"-$i") }
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
      model.batch(seen = listings)
      val seeded = Option.when(differs(listings))(s"$label: seeded")
      model.batch(gone = half.map(_.key))
      val left   = Option.when(differs(rest))(s"$label: after half left")
      model.batch(seen = half)
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
        model.batch(seen = listings)
        Option.when(ResolutionSignature.of(model) != ResolutionSignature.of(
          IdentityResolver.resolveWith(listings, lookups, normalizer, calibration, IdentityResolver.Mutation.None)))(s"$label, batch $batch")
      }
    } shouldBe empty
  }

  // `sizes` counted the families over the default region (500 listings), whatever region the model resolves by.
  // The drift replay resolves an update's regions side by side (`regionThreads`): each region reads only the context, the
  // lookups and the listings held, and the round's results are folded in batch order, so every decision is the same.
  it should "decide the same when an update's regions are resolved side by side, event after event" in {
    val cases = Seeds.map(seed => (s"seed $seed", GeneratedIdentityCorpus.generate(seed, normalizer, films = 12, listings = 48)))
      .map { case (label, c) => (label, c.listings, c.lookups) } ++ crossFamily
    IndependentCases.flatMap(cases) { case (label, listings, lookups) =>
      val alone  = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, regionBatch = 3)
      val beside = new IncrementalResolver(lookups, normalizer, calibration, decorations = TitleDecorations.None, regionBatch = 3,
        regionThreads = 4)
      val rnd    = new Random(listings.size.toLong)
      val halves = rnd.shuffle(listings).splitAt(listings.size / 2)
      val events = Seq[IncrementalResolver => Unit](_.batch(seen = halves._1), _.batch(seen = halves._2),
        _.batch(gone = halves._1.take(5).map(_.key)), _.batch(seen = halves._1.take(5)))
      events.zipWithIndex.flatMap { case (event, step) =>
        event(alone); event(beside)
        Option.when(ResolutionSignature.of(beside) != ResolutionSignature.of(alone) || beside.familyOf != alone.familyOf)(s"$label, event $step")
      }
    } shouldBe empty
  }

  // The worker's take-up (`restore`, the boot's first projection) resolves its regions side by side
  // (`takeUpThreads`): from an empty store, and from one whose families half stand, it decides what one thread does.
  it should "take up a corpus the same when the take-up's regions are resolved side by side" in {
    IndependentCases.flatMap(Seeds) { seed =>
      val corpus  = GeneratedIdentityCorpus.generate(seed, normalizer, films = 12, listings = 48)
      val stored  = new InMemoryIdentityModelStore
      new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, store = stored, regionBatch = 3)
        .batch(seen = corpus.listings.take(corpus.listings.size / 2))
      Seq("empty store" -> (() => new InMemoryIdentityModelStore), "half stood" -> (() => {
        val copy = new InMemoryIdentityModelStore
        copy.replace(Set.empty, stored.families()); stored.rulesVersion.foreach(copy.recordRulesVersion); copy
      })).flatMap { case (label, store) =>
        def takeUp(threads: Int) = {
          val model = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None,
            store = store(), regionBatch = 3, takeUpThreads = threads)
          model.restore(corpus.listings)
          model
        }
        val (alone, beside) = (takeUp(1), takeUp(4))
        Option.when(ResolutionSignature.of(beside) != ResolutionSignature.of(alone) || beside.familyOf != alone.familyOf)(s"seed $seed, $label")
      }
    } shouldBe empty
  }

  it should "count as large the families over its own region, as the model bounds one" in {
    val corpus = GeneratedIdentityCorpus.generate(7L, normalizer, films = 12, listings = 48)
    val model  = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None, regionBatch = 1)
    model.batch(corpus.listings, Nil, AnswersChanged.Empty)
    val sizes  = model.sizes
    sizes.largestListings should be > 1
    sizes.large should be > 0
    sizes.render should startWith (s"${sizes.large} families over 1 listings;")
  }

  it should "take a corpus whole resolving each of its families about once" in {
    val corpus = GeneratedIdentityCorpus.generate(7L, normalizer, films = 12, listings = 48)
    val model  = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None)
    model.batch(seen = corpus.listings)
    val whole  = IdentityResolver.resolveWith(corpus.listings, corpus.lookups, normalizer, calibration, IdentityResolver.Mutation.None)
    ResolutionSignature.of(model) shouldBe ResolutionSignature.of(whole)
    model.familiesResolved should be <= (whole.families * 2)
  }

  "the incremental resolver's resolution" should "carry the whole resolve's decisions and decided films, and leave its families to the model" in {
    val corpus = GeneratedIdentityCorpus.generate(3L, normalizer, films = 12, listings = 48)
    val model  = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None)
    model.batch(seen = corpus.listings)
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
    model.batch(seen = corpus.listings)
    model.gaps.isEmpty shouldBe false
    Iterator.continually(lookups.answer()).takeWhile(!_.isEmpty).foreach(changed => model.batch(answered = changed))
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
    model.batch(seen = Seq(bare, dated))
    def whole = IdentityResolver.resolveWith(Seq(bare, dated), lookups, normalizer, calibration, IdentityResolver.Mutation.None)
    ResolutionSignature.of(model) shouldBe ResolutionSignature.of(whole)
    credits = Map(bare.key -> Seq("Maciej Kawalski"))
    model.batch(answered = AnswersChanged(Set.empty, Set.empty, Set(bare.key)))
    ResolutionSignature.of(model) shouldBe ResolutionSignature.of(whole)
    model.decisions.find(_.listings(bare.key)).flatMap(_.film) shouldBe Some(2)
  }

  "same-title billings crediting different directors" should "keep apart while their films' records are not answered yet" in {
    // DE "Rose" (prod 10-18..10-22): Filmstarts' venues credit Markus Schleinzer (1176962), Metropol Düsseldorf's kinoheld
    // page Marie-Camille Babayan (1712584), KiKuBi's Aurélie Saada (846864); Cineamo's venues bill the bare title, linked
    // to 1176962. Each director's filmography holds a "Rose" of their own, none the others'. While the three records were
    // unanswered (a deploy's new questions, a new listing's), nothing denied any of them, the title joined every billing
    // and one film took them all — Metropol and KiKuBi on Schleinzer's film — and the next fill moved them back.
    import FilmTable.F
    import models.{Cinema, CinemaMovie, Movie, Showtime}
    val germany = services.movies.TitleNormalizer.forCountry(models.Country.Germany)
    val venues  = Cinema.all.filter(c => models.City.forCinema(c).exists(_.country == models.Country.Germany)).take(7)
    def billed(cinema: Cinema, year: Option[Int], directors: Seq[String], page: Option[String] = None, ids: Map[String, String] = Map.empty) =
      Listing.of(cinema, CinemaMovie(Movie("Rose", releaseYear = year), cinema, None, page, None, Nil, directors,
        Seq(Showtime(java.time.LocalDateTime.of(2026, 10, 25, 20, 0), None)), externalIds = ids), germany)
    val films = Seq(
      F(1176962, "Rose", 2026, "Markus Schleinzer", 94, 6.0, releaseCountries = Some("ATDE"), releases = Some("AT2026-DE2026-")),
      F(1712584, "Rose", 2026, "Marie-Camille Babayan", 0, 1.5, releaseCountries = Some("FR"), releases = Some("FR2026-"), searched = false, suggested = false),
      F(846864, "Rose", 2021, "Aurélie Saada", 102, 1.5, releaseCountries = Some("FR"), releases = Some("FR2021-"), searched = false, suggested = false))
    val schleinzer = venues.take(2).map(billed(_, Some(2026), Seq("Markus Schleinzer"), ids = Map("webedia" -> "319893")))
    val bare       = venues.slice(2, 5).map(billed(_, Some(2026), Nil, ids = Map("cineamo" -> "1176962m")))
    val babayan    = billed(venues(5), None, Seq("Marie-Camille Babayan"), page = Some("https://www.kinoheld.de/film/rose-6ad8438a4098f"))
    val saada      = billed(venues(6), None, Seq("Aurélie Saada"), page = Some("https://www.kinoheld.de/film/rose-69f2dcdbbbd26"))
    val table      = new FilmTable(films, germany)
    var unanswered = films.map(_.id).toSet
    val lookups = new IdentityLookups {
      def hasDetail(l: Listing): Boolean = false
      def detail(l: Listing): Answer[Option[DetailFacts]] = table.detail(l)
      def candidates(q: CandidateQuery): Answer[Seq[Hit]] = table.candidates(q)
      def film(id: Int): Answer[Option[IdentityMeasures.Film]] = if (unanswered(id)) Answer.Unknown else table.film(id)
    }
    val model = new IncrementalResolver(lookups, germany, IdentityCalibration.resolver, decorations = TitleDecorations.None)
    def filmOf(l: Listing) = model.decisions.find(_.listings(l.key)).flatMap(_.film)
    def takes = Map("schleinzer" -> schleinzer.map(filmOf).distinct, "bare" -> bare.map(filmOf).distinct,
      "babayan" -> Seq(filmOf(babayan)), "saada" -> Seq(filmOf(saada)))
    model.batch(seen = schleinzer ++ bare ++ Seq(babayan, saada))
    // no record answered: a billing takes its own director's film or none, never another credited director's
    withClue(takes) {
      takes("babayan").flatten.toSet should (be(empty) or be(Set(1712584)))
      takes("saada").flatten.toSet should (be(empty) or be(Set(846864)))
      takes("schleinzer").flatten.toSet should (be(empty) or be(Set(1176962)))
    }
    unanswered = Set.empty
    model.batch(answered = AnswersChanged(Set.empty, films.map(_.id).toSet))
    // every record answered: each its own film, the bare billings free to join one — Cineamo's linked film
    withClue(takes)(takes shouldBe Map("schleinzer" -> Seq(Some(1176962)), "bare" -> Seq(Some(1176962)),
      "babayan" -> Seq(Some(1712584)), "saada" -> Seq(Some(846864))))
    ResolutionSignature.of(model) shouldBe
      ResolutionSignature.of(IdentityResolver.resolveWith(schleinzer ++ bare ++ Seq(babayan, saada), lookups, germany, IdentityCalibration.resolver,
        IdentityResolver.Mutation.None))
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
    model.batch(seen = Seq(bare, dated))
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
    model.batch(seen = Seq(cosi, carmen))
    model.decisions.find(_.listings(carmen.key)).flatMap(_.film) shouldBe Some(RboCarmen)
    model.batch(seen = Seq(other))
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
    model.batch(seen = Seq(dated))
    walked = true                                                          // filed; its event not yet drained
    noException should be thrownBy model.batch(seen = Seq(bare))
    model.batch(answered = AnswersChanged(model.gaps.queries, Set.empty))
    ResolutionSignature.of(model) shouldBe ResolutionSignature.of(
      IdentityResolver.resolveWith(Seq(dated, bare), lookups, normalizer, calibration, IdentityResolver.Mutation.None))
  }

  "the incremental resolver's work" should "re-resolve only the families an event can move" in {
    import FilmTable.{F, listing}
    import models.{Helios, Multikino}
    val films = Seq(F(1, "Lalka", 1968, "Wojciech Has", 159), F(2, "Matilda", 1996, "Danny DeVito", 98),
      F(3, "Faust", 1926, "F.W. Murnau", 116))
    val model = new IncrementalResolver(new FilmTable(films, normalizer), normalizer, calibration, decorations = TitleDecorations.None)
    model.batch(seen = Seq(listing(Multikino, "Lalka", Some(1968)), listing(Helios, "Lalka"), listing(Multikino, "Matilda", Some(1996))))
    val afterTwo = model.familiesResolved
    afterTwo shouldBe 2
    model.batch(answered = AnswersChanged(Set(CandidateQuery.Title("Nothing anyone asked")), Set(999)))
    model.familiesResolved shouldBe afterTwo                    // nothing held asked it: nothing moves
    model.batch(seen = Seq(listing(Helios, "Faust", Some(1926))))
    model.familiesResolved shouldBe afterTwo + 1                // a film of its own: its family alone
    model.batch(gone = Seq(listing(Helios, "Lalka").key))
    model.familiesResolved shouldBe afterTwo + 2                // Lalka's family, and only it
  }

  "a listing re-published with only another poster, days or names" should "be held in its place, nothing resolved again" in {
    import FilmTable.{F, listing}
    import models.Multikino
    val model = new IncrementalResolver(new FilmTable(Seq(F(1, "Lalka", 1968, "Wojciech Has", 159)), normalizer), normalizer, calibration,
      decorations = TitleDecorations.None)
    val first = listing(Multikino, "Lalka", Some(1968)).copy(poster = Some("https://kino.example/lalka-a.jpg"))
    model.batch(seen = Seq(first))
    val (resolved, decided) = (model.familiesResolved, model.decisions)
    val recut = listing(Multikino, "Lalka", Some(1968)).copy(poster = Some("https://kino.example/lalka-b.jpg"),
      names = VenueNames.ofHashes(Seq(42)))
    model.batch(seen = Seq(recut))
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
