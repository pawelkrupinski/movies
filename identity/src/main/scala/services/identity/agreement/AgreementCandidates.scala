package services.identity.agreement

import scala.collection.mutable
import services.identity.IdentityLookups
import services.identity.measures.{Evidence, FallbackIds, Listing, ListingShape, PosterEvidence}
import services.identity.measures.MeasuredListing
import services.identity.vocabulary.{Answer, CandidateQuery, Dependency, Memo, PosterAnswers, PosterHash, PosterQuestion, Question, VoterFamily}

/** The TMDB films a cluster's own evidence reaches, as the stage's takes and corrections read them: its candidates, its
 *  titles' searches and picks, the venue posters' distances to them, and the records and productions the broadcast
 *  join reads — each read through `evidence` ([[AgreementEvidence]]), the cluster's candidates kept while what they read stands
 *  (`changes`: the answers filed since a version). */
private[agreement] final class AgreementCandidates(evidence: AgreementEvidence, changes: AnswerChanges) {
  import evidence.{looked, venues, settled}

  /** Each cluster's TMDB candidates, none denied, at its listings' digest: what its posters are compared against — a cache
   *  of the TMDB answers the read took ([[Memo]]): kept while none of them was filed since. */
  private[agreement] val candidateFilms = new Memo[String, AgreementCandidates.CandidateFilms](_.reads, _ => false)

  /** How many reads of TMDB's answers met one not answered yet — a search, a record — since the stage was made: a
   *  correction that met one decided on what it could read, and is read again rather than kept ([[correctionOf]]). The
   *  stage applies on one thread at a time (`apply` is synchronized). */
  private[agreement] var blindReads = 0L
  /** The listing as the model measures it, its venue's detail page read: what the broadcast join reads. */
  private[agreement] def measured(listing: Listing): MeasuredListing = Evidence.of(listing, venues.detail(listing).answered.flatten).measured
  /** Is a record of the billed work dated by its year alone, its day still to be read before the broadcast join decides
   *  ([[waitsOn]])? What the stage's take and the fill's `broadcast.take` signal both wait on. */
  private[agreement] def undatedOf(lookups: IdentityLookups): Int => Boolean =
    film => lookups.releaseDay(film).isAwaited && waitsOn(film)

  /** The candidates of a relay whose listings link a listings site's catalogue entry as their page, read as the venues
   *  bill them — the entry's facts left out: Flicks links an encore to an old season's page, whose year denies the
   *  broadcast's record before its day is read (UK "MetOpera: Samson et Dalila", 8 December 2026, linked to the
   *  2018–19 page). None for a cluster whose listings bill no stage work or link no such page, nor while a search the
   *  read asks has no answer. */
  private[agreement] def uncatalogued(listings: Seq[Listing], lookups: IdentityLookups): Seq[Int] =
    if (!listings.exists(ListingShape.stagesAWork) || !listings.exists(_.page.exists(services.identity.measures.SourceReliability.listingsSiteEntry))) Nil
    else {
      searchedFilms(listings, new AgreementEvidence.CatalogueBlind(lookups)).getOrElse(Nil)
    }

  /** The house productions the families hold of the works the listings bill, as their searches by the listings' own
   *  titles find them — each search's first [[FamilyLookups.Records]] hits whose title bills a house beside a work, their
   *  records read — for the broadcast join's credit of a production ([[Broadcast]]); `Unknown`, each gap noted in
   *  `asked`, while one is not answered — and not attempted: one attempted is read as no production ([[settled]]). Read only for a relay whose banner spells another house than a record it fits.
   *  `receipt`, a verdict's where it reads them, notes each. */
  private[agreement] def productionsOf(listings: Seq[Listing], measured: Listing => services.identity.measures.MeasuredListing,
                            asked: mutable.Set[Question], receipt: Option[mutable.Map[Dependency, Long]] = None): Answer[Seq[services.identity.measures.Film]] = {
    val queries = listings.flatMap(listing => services.identity.measures.IdentityMeasures.searchQueries(measured(listing))).distinct
    var unknown = false
    def noted[A](family: VoterFamily, question: String, answer: Answer[A]): Option[A] = {
      // a verdict's fill reads them: in its receipt, answered or not
      receipt.foreach(_(Dependency.Answer(Question.Family(family, question))) = Dependency.digest(answer))
      val gap = Question.Family(family, question)
      if (answer == Answer.Unknown) asked += gap
      if (settled(gap, answer).isAwaited) unknown = true
      answer.answered
    }
    val found = evidence.familiesInOrder.flatMap { case (family, answers) =>
      queries.filterNot(text => family.latinTitlesOnly && !tools.LatinScript.hasLetter(text)).flatMap { text =>
        noted(family, s"title|$text", answers.titled(text)).toSeq.flatMap(_.take(FamilyLookups.Records))
          .filter(hit => Broadcast.billsAProduction(hit.title)).map(family -> _.id)
      }.distinct.flatMap { case (_, hit) => noted(family, s"record|$hit", answers.record(hit)).flatten.map(_.film) }
    }
    if (unknown) Answer.Unknown else Answer.Known(found)
  }

  /** Does the take still wait on a record's day: its read again not attempted yet — the question memory's say (an answer
   *  filed is marked as one), never whether this stage asked it (docs/design/identity-no-drift.md)? No clock: the settle
   *  rule's (docs/design/identity-questions.md, step 4). A record TMDB still dates by its year once read again is taken as
   *  dating none: one read, never a loop. */
  private[agreement] def waitsOn(film: Int): Boolean = !evidence.attempted(Question.Record(film))

  /** `answer`, `hashes`' to the poster `question`, the question noted in `asked` while it is `Unknown` or stale: a stale
   *  answer is read, and asked again; one attempted and not answered is read as `absent` ([[settled]], then
   *  [[Answer.failedAs]]). */
  private[agreement] def posterRead[A](question: PosterQuestion, answer: Answer[A], absent: => A, hashes: PosterAnswers,
                            asked: mutable.Set[Question]): Answer[A] = {
    if (answer == Answer.Unknown || !hashes.fresh(question)) asked += Question.Poster(question)
    settled(Question.Poster(question), answer).failedAs(absent)
  }

  /** The nearest each of the cluster's candidates (and `also`, the film the families agree on) comes to each of its
   *  venue posters — none where no listing shows one; `Unknown`, each poster not hashed yet noted in `asked`, while one
   *  is not. A candidate numbering another edition than a listing showing a poster is not compared
   *  ([[PosterEvidence.editionsApart]]). */
  private[agreement] def posterDistances(id: String, digest: Long, listings: Seq[Listing], also: Option[Int],
                              asked: mutable.Set[Question],
                              candidates: (String, Long, Seq[Listing]) => Seq[Int] = candidatesOf,
                              hashes: PosterAnswers): Answer[Seq[Map[Int, Option[Int]]]] = {
    val urls = PosterEvidence.urls(listings)
    if (urls.isEmpty || !evidence.hasTmdb) Answer.Known(Nil)
    else {
      val venue = urls.map(url => url -> posterRead(PosterQuestion.Venue(url), hashes.venue(url), None, hashes, asked))
      if (venue.exists(_._2 == Answer.Unknown)) Answer.Unknown
      else {
        val shown: Seq[PosterHash] = venue.flatMap(_._2.answered.flatten)
        if (shown.isEmpty) Answer.Known(Nil)
        else filmDistances(id, digest, listings, also, shown, asked, candidates, hashes)
      }
    }
  }

  /** The nearest each candidate of the cluster (and `also`) comes to each venue poster `shown` — a candidate numbering
   *  another edition than a listing showing a poster left out; `Unknown`, each poster not hashed yet noted in `asked`,
   *  while one is not. */
  private def filmDistances(id: String, digest: Long, listings: Seq[Listing], also: Option[Int], shown: Seq[PosterHash],
                            asked: mutable.Set[Question], candidates: (String, Long, Seq[Listing]) => Seq[Int],
                            hashes: PosterAnswers): Answer[Seq[Map[Int, Option[Int]]]] = {
    val showing = listings.filter(listing => PosterEvidence.shows(listing) && listing.poster.isDefined)
    def apart(film: Int) = looked.flatMap(_.film(film).answered.flatten).exists(record => showing.exists(PosterEvidence.editionsApart(_, record)))
    val films  = (candidates(id, digest, listings).filterNot(apart) ++ also.filterNot(FallbackIds.isFallback)).distinct
    val filmHashes = films.map(film => film -> posterRead(PosterQuestion.Film(film), hashes.film(film), Nil, hashes, asked))
    if (filmHashes.exists(_._2 == Answer.Unknown)) Answer.Unknown
    else Answer.Known(shown.map(poster => filmHashes.map { case (film, answer) => film -> PosterEvidence.nearest(Seq(poster), answer.answered.getOrElse(Nil)) }.toMap))
  }

  /** The TMDB films the listings' own evidence reaches, none denied and no fallback id — None while a search they ask
   *  has no answer. */
  private[agreement] def searchedFilms(listings: Seq[Listing], lookups: IdentityLookups): Option[Seq[Int]] = {
    val noting = new AgreementEvidence.UnknownNoting(lookups)
    val found  = evidence.nodesOf(listings, noting)
      .flatMap(_.candidates.filterNot(_.denied).map(_.tmdbId)).filterNot(FallbackIds.isFallback).distinct.sorted
    Option.unless(noting.unknown)(found)
  }

  /** The TMDB films the cluster's own evidence reaches, none denied — a cache of its listings and of every TMDB answer
   *  the read took (its receipt): kept while its listings' digest stands and none of those answers filed since moved (a
   *  filing re-read and compared by value, [[Memo.clean]]); none,
   *  read again once something is filed (another `version`), while a TMDB question of theirs has no answer. What it read
   *  goes into the receipt being made, if any, kept or read afresh. */
  private[agreement] def candidatesOf(id: String, digest: Long, listings: Seq[Listing]): Seq[Int] = {
    val applying = evidence.version
    val kept = candidateFilms.get(id).filter(kept => kept.digest == digest && kept.blindAt.forall(_ == applying) &&
      candidateFilms.clean(id, digest, applying, changes.changedSince, again = (read, recorded) => evidence.digestNow(read, AgreementEvidence.listingIn(listings)).contains(recorded)))
    val now  = kept.getOrElse {
      var blind = false
      val receipt = mutable.Map.empty[Dependency, Long]
      val films = evidence.receipting(receipt)(looked.fold(Seq.empty[Int]) { lookups =>
        val found = searchedFilms(listings, lookups)
        blind = found.isEmpty
        found.getOrElse(Nil)
      })
      // read blind — a search with no answer — it is kept only while nothing is filed: the digest does not move when TMDB
      // answers, so kept for good, "none" stood for good; kept for no pass, every pass searched live TMDB again
      val fresh = AgreementCandidates.CandidateFilms(digest, films, Option.when(blind)(applying), receipt.toMap)
      candidateFilms.put(id, fresh); candidateFilms.check(id, digest, applying)
      fresh
    }
    if (now.blindAt.isDefined) blindReads += 1
    // kept, its reads are this apply's too, and the receipt's being made
    if (kept.isDefined) now.reads.keysIterator.foreach {
      case Dependency.Lookup(_, _, Question.FilmRecord(film)) => evidence.tmdbReads.record(film)
      case _                                                  => ()
    }
    if (kept.isDefined && now.reads.keysIterator.exists { case Dependency.Lookup(_, _, Question.Candidate(_)) => true; case _ => false }) evidence.tmdbReads.queried()
    evidence.noted(now.reads)
    now.films
  }

  /** The TMDB films the listings' own title searches find ([[services.identity.measures.IdentityMeasures.searchQueries]], of TMDB
   *  and IMDb's titled lists) — the candidates a poster may name against a model take, read off the answers the model
   *  already holds without scoring them again: a resolver pass per take cost a cold projection 10 GB of allocation over
   *  the five corpora (2026-10-06). Read, never kept; none while one of the searches has no answer — or, `answeredOnly`,
   *  the films the answered ones find (the synopsis evidence's namesakes, [[synopsisAgainst]]). */
  private[agreement] def titledFilms(listings: Seq[Listing], answeredOnly: Boolean = false): Seq[Int] =
    looked.fold(Seq.empty[Int]) { lookups =>
      val queries = listings.flatMap(listing => services.identity.measures.IdentityMeasures.searchQueries(Evidence.of(listing, None).measured)).distinct
        .flatMap(text => Seq(CandidateQuery.Title(text), CandidateQuery.ImdbTitled(text)))
      val found = queries.map(lookups.candidates)
      if (found.contains(Answer.Unknown)) blindReads += 1
      if (found.contains(Answer.Unknown) && !answeredOnly) Nil
      else found.flatMap(_.answered.getOrElse(Nil)).map(_.tmdbId).filterNot(FallbackIds.isFallback).distinct.sorted
    }

  /** The films the listings' own titles pick — as published, as the venue's rules clean them, as they are searched — each
   *  title search's most popular hit, a tie picking none: TMDB's evidence of the title alone, which no feed's facts
   *  enter ([[Catalogue.take]]). An answer not held picks nothing. */
  private[agreement] def titlePicks(listings: Seq[Listing]): Set[Int] =
    looked.fold(Set.empty[Int]) { lookups =>
      listings.flatMap(listing => Seq(listing.title, listing.cleanTitle) ++ listing.searchTitle).map(_.trim).filter(_.nonEmpty).distinct.flatMap { text =>
        lookups.candidates(CandidateQuery.Title(text)).answered.flatMap { hits =>
          val named = hits.filterNot(hit => FallbackIds.isFallback(hit.tmdbId))
          named.maxByOption(_.popularity).filter(best => !named.exists(hit => hit.tmdbId != best.tmdbId && hit.popularity == best.popularity)).map(_.tmdbId)
        }
      }.toSet
    }
}

private[agreement] object AgreementCandidates {
  /** A cluster's candidates read at its listings' `digest` — `blindAt` the version they were read at while a TMDB search had
   *  no answer, to be read again at another — with what the read took (its receipt). */
  private[agreement] final case class CandidateFilms(digest: Long, films: Seq[Int], blindAt: Option[Long], reads: Map[Dependency, Long])
}
