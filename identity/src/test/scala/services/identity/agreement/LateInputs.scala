package services.identity.agreement

import services.identity.{AnswersChanged, Answer, CandidateQuery, CatalogueAnswers, CatalogueHit, CatalogueId, CatalogueQuestion, DetailFacts, Hit,
  IdentityCalibration, IdentityLookups, IdentityMeasures, IdentityResolver, IncrementalResolver, InMemoryIdentityModelStore, Listing, Pin,
  PinConstraints, PosterAnswers, PosterHash, ProseWords, Question, Resolution, TitleDecorations, UnifiedRules}
import services.movies.{ListingKey, SingleCountryNormalizer}

import scala.collection.mutable

/**
 * The per-input-kind half of the no-drift harness (docs/design/identity-no-drift.md, "The harness"): one input, or one
 * kind of them, arrives LATE — after the model and the agreement stage decided without it — and the incremental path
 * must then take exactly what a whole resolve over every input takes. [[IncrementalWholeEquivalenceSpec]] drives random
 * histories over all inputs at once; this drives one [[LateInputs.Kind]] at a time, so a red names the kind whose late
 * arrival the incremental path does not re-read.
 *
 * Until it arrives, a late input reads as one of three [[LateInputs.Absence]]s: not filed yet (`Unknown`, never asked),
 * asked and failed (`Unknown`, attempted: the stage settles without it), or not existing yet (TMDB holding no such record
 * or search hit, a family no entry, a page no facts: a `Known` empty answer). Its arrival reaches the model as the
 * worker's fill tells it ([[AnswersChanged]] of what the model asked), and moves the stage's version as the worker's
 * family store moves it (`FamilyAnswerStore.agreementKeyOf`): a family's, a poster's or a catalogue answer always; an
 * IMDb id's find or a TMDB record (its cast and descriptions are the record's) while the stage waits on it or a kept
 * verdict read it ([[AgreementStage.reads]]); a TMDB search while a kept verdict read one; a venue page of the model's
 * never. A pin arriving re-resolves the model under the new pins (its rules version names them); a listing joins or
 * leaves through the model's batch, as a scrape does.
 */
object LateInputs {
  val normalizer: services.movies.TitleNormalizer = SingleCountryNormalizer.titleNormalizer
  private val calibration = IdentityCalibration.fromResource("services/identity/test-calibration.json").get

  /** The kinds of input the identity system reads. */
  enum Kind {
    case FamilyAnswer, TmdbFind, Record, Search, People, Poster, Catalogue, VenuePage, Synopsis, BroadcastDay, Pin, Listing
  }

  /** How a late input reads until it arrives. */
  enum Absence {
    /** Not filed yet, never asked: `Unknown`. */
    case Unfiled
    /** Asked and failed: `Unknown`, and attempted — the stage decides without it (the settle rule). */
    case Attempted
    /** Not existing yet: a `Known` empty answer — TMDB holding no such record or hit, a family no entry, a page no facts. */
    case Missing
  }

  /** One input the identity system reads, as the harness gates it. */
  enum Input(val kind: Kind) {
    /** A TMDB search: a person's ([[CandidateQuery.Director]]) is of [[Kind.People]]. */
    case Search(query: CandidateQuery) extends Input(query match {
      case _: CandidateQuery.Director => Kind.People
      case _                          => Kind.Search
    })
    /** A film new in TMDB: its search hits and its record. */
    case NewFilm(tmdbId: Int) extends Input(Kind.Search)
    /** A film's TMDB record. */
    case Record(tmdbId: Int) extends Input(Kind.Record)
    /** A film's top-billed cast as TMDB credits it. */
    case Cast(tmdbId: Int) extends Input(Kind.People)
    /** A film's TMDB descriptions. */
    case Overview(tmdbId: Int) extends Input(Kind.Synopsis)
    /** The day TMDB dates a film's release: a broadcast's air date. */
    case Day(tmdbId: Int) extends Input(Kind.BroadcastDay)
    /** A listing's venue detail page. */
    case Page(listing: ListingKey) extends Input(Kind.VenuePage)
    /** An agreement question: a family's (a person search's is of [[Kind.People]]), a find, a poster, a catalogue page or id. */
    case Agreement(question: Question) extends Input(question match {
      case Question.Family(_, asked) if asked.startsWith("director|") => Kind.People
      case Question.Family(_, _)                                      => Kind.FamilyAnswer
      case Question.Find(_)                                           => Kind.TmdbFind
      case Question.Poster(_)                                         => Kind.Poster
      case Question.Catalogue(_)                                      => Kind.Catalogue
      case _                                                          => Kind.Record
    })
  }

  /**
   * One case: every input as it finally stands — the listings, TMDB (searches, records, cast, descriptions, release days,
   * venue pages), the families, the IMDb finds, the posters, the catalogue, the pins — and which of them arrive late:
   * `late` (with `absence` until then), `pins`, the listings `joining` and those `leaving` (held at the start, gone at the
   * end). `listings` is the final set, `joining` among them.
   */
  final case class Scenario(name: String, kind: Kind, listings: Seq[Listing], tmdb: IdentityLookups,
                            families: Map[VoterFamily, FamilyAnswers] = Map.empty,
                            finds: String => Answer[Option[Int]] = _ => Answer.Known(None),
                            posters: PosterAnswers = PosterAnswers.Silent, catalogue: CatalogueAnswers = CatalogueAnswers.Silent,
                            rules: UnifiedRules = UnifiedRules.resolver, identities: Seq[VoterFamily] = Seq(VoterFamily.Wiki),
                            late: Input => Boolean = _ => false, absence: Absence = Absence.Unfiled,
                            pins: Seq[Pin] = Nil, joining: Seq[Listing] = Nil, leaving: Seq[Listing] = Nil) {
    def withAbsence(a: Absence): Scenario = copy(absence = a, name = s"$name [$a]")
  }

  /** When the incremental path restarts over its stores: never, after deciding without the input, or after it arrived. */
  enum Restart { case Never, Before, After }

  /** Each listing's cluster (its members) and take: grouping and takes, exactly ([[IncrementalWholeEquivalenceSpec.takes]]). */
  def takes(resolution: Resolution) = IncrementalWholeEquivalenceSpec.takes(resolution)

  /**
   * One run over a scenario: the gates (`arrived` flips them all at once), the model and stage with their stores, and the
   * questions read while withheld — what an arrival announces.
   */
  final class Run(s: Scenario) {
    var arrived = false
    private def gated(input: Input): Option[Absence] = Option.when(!arrived && s.late(input))(s.absence)
    private val askedWhileWithheld = mutable.LinkedHashSet.empty[Input]
    private val askedQueries       = mutable.Set.empty[CandidateQuery]
    private val askedFilms         = mutable.Set.empty[Int]
    private val askedPages         = mutable.Set.empty[ListingKey]

    /** `answer`, or — the input withheld — `Unknown` until filed, `missing` while it does not exist yet. */
    private def gate[A](input: Input, missing: => A)(answer: => Answer[A]): Answer[A] = gated(input) match {
      case None                  => answer
      case Some(Absence.Missing) => askedWhileWithheld += input; Answer.Known(missing)
      case Some(_)               => askedWhileWithheld += input; Answer.Unknown
    }
    private def newFilmGone(id: Int): Boolean = gated(Input.NewFilm(id)).isDefined && { askedWhileWithheld += Input.NewFilm(id); true }

    val lookups: IdentityLookups = new IdentityLookups {
      def hasDetail(listing: Listing): Boolean = s.tmdb.hasDetail(listing)
      override def numbersFilmsByTmdbId: Boolean = s.tmdb.numbersFilmsByTmdbId
      def detail(listing: Listing): Answer[Option[DetailFacts]] = gate(Input.Page(listing.key), Option.empty[DetailFacts])(s.tmdb.detail(listing))
      def candidates(query: CandidateQuery): Answer[Seq[Hit]] =
        gate(Input.Search(query), Seq.empty[Hit])(s.tmdb.candidates(query)).map(_.filterNot(hit => newFilmGone(hit.tmdbId)))
      // a record filed before its release day is known holds only the year it was released in
      def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] =
        if (newFilmGone(tmdbId)) Answer.Known(None)
        else gate(Input.Record(tmdbId), Option.empty[IdentityMeasures.Film])(s.tmdb.film(tmdbId))
          .map(_.map(record => if (gated(Input.Day(tmdbId)).isDefined) record.copy(released = None) else record))
      override def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]] =
        if (newFilmGone(tmdbId)) Answer.Known(None)
        else gate(Input.Day(tmdbId), Option.empty[java.time.LocalDate])(film(tmdbId).map(_.flatMap(_.released)))
      override def cast(tmdbId: Int): Answer[Option[Seq[String]]] = gate(Input.Cast(tmdbId), Option.empty[Seq[String]])(s.tmdb.cast(tmdbId))
      override def overview(tmdbId: Int): Answer[Option[ProseWords]] = gate(Input.Overview(tmdbId), Option.empty[ProseWords])(s.tmdb.overview(tmdbId))
    }
    /** The model's lookups, noting what it asked: what an arrival announces to it, as the fill does. */
    private val modelLookups: IdentityLookups = new services.identity.ForwardingIdentityLookups(lookups) {
      override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = { askedQueries += query; super.candidates(query) }
      override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = { askedFilms += tmdbId; super.film(tmdbId) }
      override def detail(listing: Listing): Answer[Option[DetailFacts]] = { askedPages += listing.key; super.detail(listing) }
    }

    private def family(of: VoterFamily, inner: FamilyAnswers): FamilyAnswers = new FamilyAnswers {
      val family: VoterFamily = of
      private def q(question: String) = Input.Agreement(Question.Family(of, question))
      def titled(text: String): Answer[Seq[SourceHit]]     = gate(q(s"title|$text"), Seq.empty[SourceHit])(inner.titled(text))
      def directedBy(name: String): Answer[Seq[SourceHit]] = gate(q(s"director|$name"), Seq.empty[SourceHit])(inner.directedBy(name))
      def record(id: String): Answer[Option[SourceRecord]] = gate(q(s"record|$id"), Option.empty[SourceRecord])(inner.record(id))
      override def showing(venue: String): Answer[Seq[Showing]] = gate(q(s"showing|$venue"), Seq.empty[Showing])(inner.showing(venue))
      override def fresh(question: String): Boolean = inner.fresh(question)
    }
    val families: Map[VoterFamily, FamilyAnswers] = s.families.map { case (of, inner) => of -> family(of, inner) }
    val tmdbOf: String => Answer[Option[Int]] = imdb => gate(Input.Agreement(Question.Find(imdb)), Option.empty[Int])(s.finds(imdb))
    val posters: PosterAnswers = new PosterAnswers {
      private def q(question: AgreementStage.PosterQuestion) = Input.Agreement(Question.Poster(question))
      def venue(url: String): Answer[Option[PosterHash]] = gate(q(AgreementStage.PosterQuestion.Venue(url)), Option.empty[PosterHash])(s.posters.venue(url))
      def film(tmdbId: Int): Answer[Seq[PosterHash]]     = gate(q(AgreementStage.PosterQuestion.Film(tmdbId)), Seq.empty[PosterHash])(s.posters.film(tmdbId))
      override def unread(question: AgreementStage.PosterQuestion): Boolean = s.posters.unread(question)
      override def fresh(question: AgreementStage.PosterQuestion): Boolean  = s.posters.fresh(question)
    }
    val catalogue: CatalogueAnswers = new CatalogueAnswers {
      private def q(question: CatalogueQuestion) = Input.Agreement(Question.Catalogue(question))
      def linked(page: String): Answer[Seq[CatalogueId]]    = gate(q(CatalogueQuestion.Page(page)), Seq.empty[CatalogueId])(s.catalogue.linked(page))
      def mapped(id: CatalogueId): Answer[Seq[CatalogueHit]] = gate(q(CatalogueQuestion.Id(id)), Seq.empty[CatalogueHit])(s.catalogue.mapped(id))
    }
    /** The question memory: a question withheld as attempted was; one withheld as unfiled was not; any other was asked and
     *  its answer, whatever it is, is what there is — an `Unknown` the case never answers is a durable non-answer. */
    val tried: Question => Boolean = question => {
      val as = question match {
        case Question.Candidate(query) => Input.Search(query)
        case Question.FilmRecord(id)   => Input.Record(id)
        case Question.VenuePage(key)   => Input.Page(key)
        case Question.Record(id)       => Input.Day(id)
        case other                     => Input.Agreement(other)
      }
      gated(as).forall(_ == Absence.Attempted)
    }

    /** What the stage's version counts — the filings `FamilyAnswerStore.agreementKeyOf` lets through: a family's, a
     *  poster's or a catalogue answer always; a find or a TMDB record (its cast and descriptions are the record's) while
     *  the stage waits on it or a kept verdict read it; a TMDB search while a kept verdict read one — and which. */
    var version = 0L
    private val filings = mutable.ArrayBuffer.empty[(Long, String)]
    private def file(id: String): Unit = { version += 1; filings += version -> id }
    private val changes: AnswerChanges = since => Some(filings.iterator.collect { case (at, id) if at > since => id }.toSet)
    private val modelStore = new InMemoryIdentityModelStore
    private val verdicts   = new InMemoryAgreementVerdicts
    private val keys       = new InMemoryAgreementKeys
    private def pinsNow    = if (arrived) s.pins else Nil
    private def newModel() = new IncrementalResolver(modelLookups, normalizer, calibration, pins = PinConstraints(pinsNow),
      decorations = TitleDecorations.None, store = modelStore, rules = s"r|${pinsNow.map(_.id).mkString(",")}")
    private def newStage(stored: AgreementVerdicts, keyStore: AgreementKeys) =
      new AgreementStage(families, lookups, normalizer, IdentityCalibration.resolver, tmdbOf, stored, clock = _root_.tools.SpecClock.Pinned,
        changes = changes, posters = posters, tmdb = Some(lookups), identities = s.identities, rules = s.rules, catalogue = catalogue,
        correctionsPerApply = Int.MaxValue, correctionPostersAtOnce = Int.MaxValue, attempted = tried, keyStore = keyStore)

    private val held  = mutable.LinkedHashMap.from((s.listings.filterNot(s.joining.contains) ++ s.leaving).map(l => l.key -> l))
    private var model = newModel()
    private var stage = newStage(verdicts, keys)
    private var fresh = true
    private def listingOf(key: ListingKey) = held.get(key)

    /** `on` applied to `resolution` until it has no backlog and waits on nothing not attempted, or 40 applies. */
    private def settle(on: AgreementStage, resolution: => Resolution, first: Boolean): Resolution = {
      var republished: Option[Iterable[ListingKey]] = if (first) None else Some(Nil)
      Iterator.continually { val r = on.apply(resolution, listingOf, version, republished); republished = Some(Nil); r }.zipWithIndex
        .find { case (_, i) => i > 40 || (!on.backlog && on.wanted.forall(tried)) }.get._1
    }
    private def projected(): Resolution = { val r = settle(stage, model.resolution, fresh); fresh = false; r }

    private def restart(): Unit = {
      model = newModel()
      model.restore(held.values.toSeq.sorted)
      stage = newStage(verdicts, keys)
      fresh = true
    }

    /** Everything arrives: the gates open, the model told what it asked of them, the stage's version moved by the
     *  filings the worker's family store counts ([[file]]), the pins taken up, the listings joining and leaving. */
    private def arrive(): Unit = {
      val (waitedOn, read) = (stage.wanted, stage.reads)
      arrived = true
      def record(id: Int) = Option.when(waitedOn(Question.Record(id)) || read(AgreementStage.recordReadId(id)))(AgreementStage.recordReadId(id))
      val tmdbFilings = askedWhileWithheld.toSeq.flatMap {
        case Input.Agreement(q @ Question.Find(_))  => q.id.filter(id => waitedOn(q) || read(id))
        case Input.Agreement(Question.Record(id))   => record(id)
        case Input.Day(id)                          => record(id)
        case Input.Record(id)                       => record(id)
        case Input.Cast(id)                         => record(id)
        case Input.Overview(id)                     => record(id)
        case Input.NewFilm(id)                      => record(id) ++ Option.when(read(AgreementStage.QueryFiled))(AgreementStage.QueryFiled)
        case Input.Search(_)                        => Option.when(read(AgreementStage.QueryFiled))(AgreementStage.QueryFiled)
        case Input.Agreement(q)                     => q.id
        case Input.Page(_)                          => None
      }
      tmdbFilings.distinct.foreach(file)
      val newFilms = askedWhileWithheld.collect { case Input.NewFilm(id) => id }
      model.batch(answered = AnswersChanged(
        askedQueries.filter(q => s.late(Input.Search(q)) || newFilms.nonEmpty).toSet,
        askedFilms.filter(id => s.late(Input.Record(id)) || newFilms(id)).toSet,
        askedPages.filter(key => s.late(Input.Page(key))).toSet))
      if (s.pins.nonEmpty) { model = newModel(); model.restore(held.values.toSeq.sorted) }
      s.joining.foreach(l => held(l.key) = l)
      s.leaving.foreach(l => held.remove(l.key))
      model.batch(seen = s.joining, gone = s.leaving.map(_.key))
    }

    /** The incremental path: decided without the late inputs, restarted where asked, then everything arrives. */
    def incremental(restartAt: Restart): Resolution = {
      model.batch(seen = held.values.toSeq)
      projected()
      if (restartAt == Restart.Before) { restart(); projected() }
      arrive()
      projected()
      if (restartAt == Restart.After) restart()
      projected()
    }

    /** A whole resolve over the inputs as they stand — every late one arrived, or (`withLate` false) none of them: the
     *  model's resolve of the listings and a fresh stage settled over it. */
    def whole(withLate: Boolean = true): Resolution = {
      arrived = withLate
      val listings = if (withLate) s.listings else (s.listings.filterNot(s.joining.contains) ++ s.leaving)
      held.clear(); held ++= listings.map(l => l.key -> l)
      val resolution = IdentityResolver.resolveWith(listings, lookups, normalizer, calibration, IdentityResolver.Mutation.None,
        pins = PinConstraints(pinsNow), decorations = TitleDecorations.None)
      settle(newStage(new InMemoryAgreementVerdicts, new InMemoryAgreementKeys), resolution, first = true)
    }
  }

  /** The first difference between the incremental path and the whole resolve, named. */
  def divergence(s: Scenario, restartAt: Restart): Option[String] =
    IncrementalWholeEquivalenceSpec.difference(new Run(s).incremental(restartAt), new Run(s).whole())
      .map(d => s"${s.name}, restart ${restartAt}: $d")

  /** Does the late input move the whole resolve at all — the case has teeth? */
  def matters(s: Scenario): Boolean = takes(new Run(s).whole(withLate = false)) != takes(new Run(s).whole())
}
