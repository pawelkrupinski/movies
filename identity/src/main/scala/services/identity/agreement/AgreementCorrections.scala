package services.identity.agreement

import services.identity.{Answer, Dependency, Evidence,
  IdentityMeasures, Listing, ListingShape, PosterEvidence, Question, ResolverDecision}
import services.movies.ListingKey

import scala.collection.mutable

/** The stage's reading of a film the MODEL took against the evidence that can contradict it ([[Correction]]): the
 *  stored correction while it stands, else the venue posters, the venues' programmes and the families read anew.
 *  Part of [[AgreementStage]], which mixes it in: it reads and writes the stage's own state. */
private[agreement] trait AgreementCorrections { this: AgreementStage =>
  /** What a take's stored correction stands on, read without decoding an answer or building a string: the film, its
   *  listings' keys and each one's poster and screening days — a take whose listings move is read afresh. */
  private[agreement] def correctionDigest(decision: ResolverDecision, listingOf: ListingKey => Option[Listing]): Long = {
    var members = 0
    var facts   = 0
    decision.members.foreach { key =>
      members += key.hashCode
      listingOf(key).foreach(listing => facts += listing.poster.hashCode * 31 + listing.screenings.hashCode)
    }
    (decision.film.hashCode.toLong << 32) ^ (members.toLong << 16) ^ (facts.toLong & 0xffffffffL)
  }

  /** A film the model took by its own rules — what [[Correction]] reads the evidence against. */
  private[agreement] def correctable(decision: ResolverDecision): Boolean =
    decision.film.isDefined && decision.members.nonEmpty &&
      (decision.basis == ResolverDecision.Basis.OwnMatch || decision.basis == ResolverDecision.Basis.PooledMatch)

  /** The model's take, withdrawn or switched where the evidence against it says so ([[Correction.decide]]): the stored
   *  correction while its listings and rules stand and none of the answers it read was filed again since it was checked
   *  — at boot, every stored one stands, as a stored verdict does — else read anew, while the apply's `fresh` budget
   *  lasts ([[AgreementStage.CorrectionsPerApply]]; a take beyond it stands as the model took it until a later apply).
   *  What it waits on is noted in `pass` to ask: the families' questions, the IMDb ids, the posters. */
  private[agreement] def corrected(decision: ResolverDecision, listingOf: ListingKey => Option[Listing], key: String, pass: AgreementStage.Pass): ResolverDecision = {
    val version = pass.version
    val digest = correctionDigest(decision, listingOf)
    // what the correction waited on was read at one shape of the take: another's is no wait of this one
    if (correctionWaits.get(key).exists(_.listings != digest)) correctionWaits -= key
    val kept = heldFor(key, digest, pass).filter(_.rules == decidedUnder).flatMap(v => v.correction.filter { c =>
      val waits = correctionWaits.get(key)
      // one waiting on an answer is read again unless what it asks is known — after a restart it is not — and once all
      // it waits on was attempted (the settle rule: decided on what it has); a TMDB find it waits on is filed among the
      // answers' changes as the families' answers are, and read again below like them
      // stands when none of its reads was filed since it was checked ([[Memo.clean]]) — a TMDB lookup's filing read again
      // and compared, as a verdict's is: every TMDB search is filed under one id (QueryFiled), so any search filed anywhere
      // reached every take that read one, and each was read afresh — 700 PL takes an apply, the budget never caught up (the
      // drift replay timing out, run 1641). A family's answer, a poster or a question asked that was filed decides it again.
      !(c.waiting && (waits.isEmpty || waits.exists(_.gaps.forall(tried)))) && {
        lazy val listings = sortedListings(decision, listingOf)
        held.clean(key, digest, version, changes.changedSince, again = {
          case (read: Dependency.Lookup, answered) => digestNow(read, listingIn(listings)).contains(answered)
          case _                                   => false
        })
      }
    })
    val now = kept.orElse(Option.when(pass.fresh()) {
      // the take's listings sorted and digested only to be read afresh: a stored correction is checked by `digest` alone
      val listings = sortedListings(decision, listingOf)
      // its receipt: every TMDB record and search, family answer and poster it read and every question it asked, so a
      // filing of one reads it again ([[Noting]])
      val receipt = mutable.Map.empty[Dependency, Long]
      def read() = { receipt.clear(); receipting(receipt)(correctionOf(decision, AgreementStage.Digested(key, listings, digest), pass.reading, version)) }
      val (correction, waits) = {
        val first  = read()
        val asking = first._2.fold(Set.empty[Question])(_.filter { case question: Question.Family => !tried(question); case _ => false })
        if (asking.isEmpty || !newCorrection(digest, version)) first
        else {
          // a take of a newly met shape — a listing joined it, a scrape reshaped it: the families its correction waits on
          // asked now, inline, as a new cluster's are, so its first take reads their answers — else it was served as the
          // model took it until they came in, and corrected after (PL "Ktoś całkiem obcy", 2026-10-25: 21 s on 2007's film)
          askedNow += digest
          pass.shapes += digest
          askNow(asking, AgreementStage.AskNowWithin)
          read()
        }
      }
      val verdict = StoredVerdict(key, digest, receipt.toMap, None, decidedUnder, correction = Some(correction))
      if (!held.get(key).contains(verdict)) { pass.moved += verdict; file(verdict) }
      held.check(key, digest, version)
      waits.fold(correctionWaits -= key)(w => correctionWaits(key) = AgreementStage.Waiting(digest, w))
      correction
    }).orElse(
      // not read afresh this apply — its budget spent, a deploy's new rules having made every stored verdict stale — it
      // stands as last decided over the same listings, never as the model took it: else the boot's whole projection put
      // the model's take back and a later apply corrected it again, a flip per deploy (UK Chichester's "The Ties That Bind
      // Us", 18 of them). Left unchecked, it is read afresh once an apply's budget reaches it.
      heldFor(key, digest, pass).flatMap(_.correction))
    correctionWaits.get(key).foreach(_.gaps.foreach {
      case Question.Poster(poster) => pass.correctionPosters += poster
      case question                => pass.asked += question
    })
    now.flatMap(_.outcome).fold(decision) { outcome =>
      val rule  = Some(Correction.ruleOf(outcome))
      val taken = outcome.film.fold(taking(decision)(ResolverDecision.Basis.Withdrawn, outcome.line, film = None, rule = rule))(film =>
        taking(decision)(ResolverDecision.Basis.Corrected, outcome.line, film = Some(film), rule = rule))
      takenAsBefore(decision, taken)
    }
  }

  /** `taken`, what the model's `decision` is taken as now — handed back as the object it was last taken as while the two
   *  are equal ([[takenAs]]). */
  private[agreement] def takenAsBefore(decision: ResolverDecision, taken: ResolverDecision): ResolverDecision =
    Option(takenAs.get(decision)).filter(_ == taken).getOrElse { takenAs.put(decision, taken); taken }

  /** The evidence against the model's take read now: the venue posters and — once one names another film — the
   *  venues' programmes and the families ([[Correction]]). A part still waiting on an answer decides nothing yet, but a
   *  part known may: a programme's contradiction withdraws the take while the families wait. */
  private[agreement] def correctionOf(decision: ResolverDecision, digested: AgreementStage.Digested,
                           reading: AgreementStage.Read, version: Long): (StoredCorrection, Option[Set[Question]]) = {
    val listings = digested.listings
    val model    = decision.film.get
    val reads    = mutable.Map.empty[Dependency, Long]
    val asked    = mutable.Set.empty[Question]
    // a search or a record TMDB has not answered yet decides nothing: the correction is read again, not kept as final —
    // kept, a take decided while the model's own record or its candidates' searches were gaps stood uncorrected until its
    // listings moved or a deploy read it again (UK Chichester's "The Ties That Bind Us", identity-slice --drift on 573)
    val blindBefore = blindReads
    def record(id: Int) = venuesLooked.film(id).toOption.flatten.orElse(looked.flatMap(_.film(id).toOption.flatten))
    def named(id: Int) = {
      val found = record(id)
      if (found.isEmpty && looked.fold(venuesLooked.film(id))(_.film(id)) == Answer.Unknown) blindReads += 1
      found
    }
    def title(id: Int) = record(id).fold(s"tmdb $id")(_.titled)
    // a relay screening years after its record's broadcast, months after a newer record of its title: no production
    // the take can be, whatever else is read
    def superseded(film: IdentityMeasures.Film) =
      Broadcast.superseded(listings, listing => Evidence.of(listing, None).measured, film, titledFilms(listings).filter(_ != model).flatMap(id => named(id).map(id -> _)))
        .map(why => Correction.Outcome(None, s"withdrawn ${title(model)} — relay: $why"))
    // a listing billing a stage work on the day one record of it was broadcast is that record ([[Broadcast.take]]): a
    // model take of a screen adaptation yields to it, as an agreed one does (PL Kino Amok's bare "Manon" on the Met's
    // day) — a house's production taken is the model's reading of the relay, never corrected here
    def relayed(film: IdentityMeasures.Film) = Option.when(listings.exists(ListingShape.stagesAWork) && !Broadcast.billsAProduction(film.title)) {
      val known = (model +: candidatesOf(digested.id, digested.digest, listings)).distinct.flatMap(id => named(id).map(id -> _))
      // a correction reads no film database's productions; a record of the billed work stored with its year alone is read
      // again before the day decides, as the stage's own take reads it (UK Theatr Gwaun Fishguard's bare "Romeo and
      // Juliet" on the RBO relay's day stood as Zeffirelli's film while the relay's record held only 2027)
      looked.flatMap(lookups => Broadcast.take(listings, measured, known)(undatedOf(lookups)).left.map(asked ++= _.map(Question.Record(_))).toOption.flatten)
    }.flatten.filter(_.film != model).map(taken => Correction.Outcome(Some(taken.film), s"corrected from ${title(model)} — ${taken.line}"))
    val outcome = named(model).flatMap(film => superseded(film).orElse(relayed(film)).orElse {
      val modelRecord = SourceRecord.ofTmdb(model, film)
      // (a) a venue poster matching another candidate, not the taken film
      val posters = correctionPosters(digested, model, reads, asked, reading)
      val vetoed  = posters.toOption.flatten
      // a venue poster given up on (its fetch kept failing), or attempted and not hashed, is no evidence: the take is read
      // as one a poster questions, and corrected only where the rest decides it without the poster
      val unread  = posters != Answer.Unknown && vetoed.isEmpty && PosterEvidence.urls(listings).exists { url =>
        val poster = AgreementStage.PosterQuestion.Venue(url)
        reading.posters.unread(poster) || reading.posters.venue(url) == Answer.Unknown && attempted(Question.Poster(poster))
      }
      val questioned = vetoed.isDefined || unread
      // (e) the venues' own programmes on `listedOn`'s site, asked only of a take a poster questions: a few venues' a day
      val listed = reading.programmes.filter(_ => questioned).fold[Answer[Option[SourceRecord]]](Answer.Known(None))(answers =>
        VenueListings.listed(listings, new AgreementStage.Recording(answers, asked, reads)))
      val programme = listed.toOption.flatten.filter(record => !Agreement.sameFilm(record, modelRecord) && Correction.contradicts(record.film, film))
      // (b) the families, asked only of a take another kind of evidence questions
      // the TMDB films the take's evidence reaches besides it: what a family's or the programme's record is matched to
      lazy val reached = (vetoed.map(_._1).toSeq ++ candidatesOf(digested.id, digested.digest, listings)).distinct.filter(_ != model)
      val records = (id: Int) => named(id).map(SourceRecord.ofTmdb(id, _))
      val against = if (programme.isEmpty && !questioned) None
        // beside the programmes, the family whose site lists them is no second kind of evidence: its take is not counted
        else familiesAgainst(listings, model, modelRecord, reached, records, programme.flatMap(_ => listedOn), reads, asked).toOption.flatten
      val evidence = programme.toSeq.map { record =>
        // the TMDB film the programme's record is: one the other evidence or the cluster's own candidates reach
        val same = (reached ++ against.map(_._1)).distinct.filter(id => records(id).exists(Agreement.sameFilm(record, _)))
        Correction.Against(Correction.Filmweb, same.headOption.filter(_ => same.sizeIs == 1),
          s"the venues' Filmweb programmes list ${record.film.titled} on their days")
      } ++ vetoed.toSeq.map { case (other, bits) =>
        Correction.Against(Correction.Poster, Some(other), s"a venue poster matches ${title(other)} ($bits bits), not the take")
      } ++ against.toSeq.map { case (other, says) => Correction.Against(Correction.Families, Some(other), says.replace(s"tmdb $other", title(other))) }
      val sharesDirector = (other: Int) => named(other).flatMap(o => Correction.shareDirector(o.directors.getOrElse(Nil), film.directors.getOrElse(Nil)))
      Correction.decide(title(model), evidence, sharesDirector)
    })
    // asked on every pass, the attempted ones too; waiting only on what was not attempted yet (the settle rule)
    val waits = Option.when(asked.nonEmpty)(asked.toSet)
    // in the receipt being made: what it read, and each question asked, read again when its answer is filed
    if (receipt != null) { receipt ++= reads; asked.foreach(question => receipt(Dependency.Asked(question)) = 0L) }
    (StoredCorrection(outcome.flatMap(_.film), outcome.map(_.line), asked.exists(question => !tried(question)) || blindReads != blindBefore), waits)
  }

  /** The candidate a venue poster matches against the model's take ([[PosterEvidence.veto]]) — read first against the
   *  take's own posters: a venue poster within [[PosterEvidence.VetoBits]] of one of them vetoes nothing, so the other
   *  candidates' posters are hashed only for a take none of them comes near. `Unknown` while the take's own posters, or
   *  every venue poster, are not hashed — or a venue poster is not and those hashed veto nothing: a veto they make stands. */
  private[agreement] def correctionPosters(digested: AgreementStage.Digested, model: Int, reads: mutable.Map[Dependency, Long],
                                asked: mutable.Set[Question], reading: AgreementStage.Read): Answer[Option[(Int, Int)]] = {
    val urls = PosterEvidence.urls(digested.listings)
    if (urls.isEmpty || tmdb.isEmpty) Answer.Known(None)
    else {
      def read[A](question: AgreementStage.PosterQuestion, answer: Answer[A], absent: A): Answer[A] = {
        reads(Dependency.Asked(Question.Poster(question))) = 0L
        posterRead(question, answer, absent, reading.posters, asked)
      }
      val venue = urls.map(url => url -> read(AgreementStage.PosterQuestion.Venue(url), reading.posters.venue(url), None))
      val own   = read(AgreementStage.PosterQuestion.Film(model), reading.posters.film(model), Nil)
      // a venue poster not hashed yet names nothing, but those hashed may: a poster's veto is its own, and a later one only
      // names a nearer film — the correction is read again once it is in (the poster asked: [[StoredCorrection.waiting]])
      // — else a listing joining a corrected take, its poster not hashed yet, took the whole cluster back to the model's
      // film until it was (PL "Ktoś całkiem obcy", 2026-10-25)
      val pending = venue.exists(_._2 == Answer.Unknown)
      if (own == Answer.Unknown || venue.forall(_._2 == Answer.Unknown)) Answer.Unknown
      else {
        val held = own.toOption.getOrElse(Nil)
        // the venue posters none of the take's own comes near: only they can name another film against it
        val far  = venue.collect { case (url, Answer.Known(Some(poster))) if !PosterEvidence.nearest(Seq(poster), held).exists(_ <= PosterEvidence.VetoBits) => url }.toSet
        if (far.isEmpty) if (pending) Answer.Unknown else Answer.Known(None)
        else {
          // the films one listing showing each such poster is searched by: a cluster billed at hundreds of venues is not
          // searched again whole for the few posters that might veto its take
          val showing = digested.listings.filter(listing => PosterEvidence.shows(listing) && listing.poster.exists(far)).distinctBy(_.poster)
          val films   = titledFilms(showing)
          films.foreach(film => reads(Dependency.Asked(Question.Poster(AgreementStage.PosterQuestion.Film(film)))) = 0L)
          posterDistances(digested.id, digested.digest, showing, Some(model), asked, (_, _, _) => films, reading.posters)
            .map(found => PosterEvidence.veto(Some(model), found)) match {
            case Answer.Known(None) if pending => Answer.Unknown
            case read                          => read
          }
        }
      }
    }
  }

  /** The film more of the families take than take the model's, by its TMDB id — a pick's TMDB or IMDb id, else the one
   *  film of `reached` its record is by its facts — with what they say, `uncounted`'s take aside; `Unknown` while a
   *  family's question, or TMDB's find of a taken IMDb id, is not answered yet — and not attempted: a family attempted and
   *  not answered is left out as no verdict, as a settled cluster leaves it (the settle rule). */
  private[agreement] def familiesAgainst(listings: Seq[Listing], model: Int, modelRecord: SourceRecord, reached: => Seq[Int], records: Int => Option[SourceRecord],
                              uncounted: Option[VoterFamily], reads: mutable.Map[Dependency, Long], asked: mutable.Set[Question]): Answer[Option[(Int, String)]] = {
    val gaps     = mutable.Set.empty[Question]
    val verdicts = verdictsOf(listings, gaps, reads)
    asked ++= gaps
    if (verdicts.contains(Answer.Unknown) && !gaps.forall(tried)) Answer.Unknown
    else {
      val mapped = verdicts.flatMap(_.toOption).flatMap(_.pick).filterNot(pick => uncounted.contains(pick.family)).map { pick =>
        val id = tmdbFilmOf(pick.record.crossIds.get("tmdb").flatMap(_.toIntOption), pick.record.crossIds.get("imdb"), asked)
        // a record linking no TMDB film (Filmweb's) is the one film the take's evidence reaches that its facts are
        pick -> (id match {
          case Answer.Known(None) => Answer.Known(reached.filter(film => records(film).exists(Agreement.sameFilm(pick.record, _))) match {
            case Seq(one) => Some(one)
            case _        => None
          })
          case found => found
        })
      }
      if (mapped.exists(_._2 == Answer.Unknown)) Answer.Unknown
      else {
        val forModel = mapped.collect { case (pick, id) if id.toOption.flatten.contains(model) || Agreement.sameFilm(pick.record, modelRecord) => pick.family }
        val others   = mapped.collect { case (pick, Answer.Known(Some(id))) if id != model && !forModel.contains(pick.family) => id -> pick.family }
          .groupMap(_._1)(_._2).toSeq.sortBy { case (id, takers) => (-takers.size, id) }
        Answer.Known(others.headOption.filter { case (_, takers) => takers.size > forModel.size && !others.drop(1).exists(_._2.size == takers.size) }
          .map { case (id, takers) =>
            id -> (s"${takers.map(_.label).sorted.mkString(", ")} take tmdb $id" +
              (if (forModel.isEmpty) ", none the take" else s", ${forModel.map(_.label).sorted.mkString(", ")} the take"))
          })
      }
    }
  }
}
