package services.identity.agreement

import scala.collection.mutable

import services.identity.measures.PosterEvidence
import services.identity.vocabulary.{Answer, Dependency, PosterHash, PosterQuestion, Question}

/** The POSTER evidence against a film the MODEL took ([[CorrectionReading]], [[Correction.Poster]]): a venue poster
 *  matching another candidate and not the taken film — read through `evidence` ([[AgreementEvidence]]) and the cluster's
 *  `candidates` (the posters' distances to its films) — and whether the posters question the take at all: only a take
 *  they question is asked of the venues' programmes and the families. */
private[agreement] final class PosterCorrection(evidence: AgreementEvidence, candidates: AgreementCandidates) {
  import candidates.{posterDistances, posterRead, titledFilms}

  /** The candidate a venue poster matches against the take `model` with its distance in bits, if one does, and whether
   *  the posters question the take: one vetoes it, or one was given up on (its fetch kept failing) or attempted and not
   *  hashed — no evidence, so the take is read as one a poster questions, and corrected only where the rest decides it
   *  without the poster. */
  def against(digested: AgreementEvidence.Cluster, model: Int, reads: mutable.Map[Dependency, Long], asked: mutable.Set[Question],
              reading: AgreementPass.Read): (Option[(Int, Int)], Boolean) = {
    val posters = correctionPosters(digested, model, reads, asked, reading)
    val vetoed  = posters.answered.flatten
    val unread  = posters != Answer.Unknown && vetoed.isEmpty && PosterEvidence.urls(digested.listings).exists { url =>
      val poster = PosterQuestion.Venue(url)
      reading.posters.unread(poster) || evidence.settled(Question.Poster(poster), reading.posters.venue(url)) == Answer.Failed
    }
    (vetoed, vetoed.isDefined || unread)
  }

  /** The candidate a venue poster matches against the model's take ([[PosterEvidence.veto]]) — read first against the
   *  take's own posters: a venue poster within [[PosterEvidence.VetoBits]] of one of them vetoes nothing, so the other
   *  candidates' posters are hashed only for a take none of them comes near. `Unknown` while the take's own posters, or
   *  every venue poster, are not hashed — or a venue poster is not and those hashed veto nothing: a veto they make stands. */
  private def correctionPosters(digested: AgreementEvidence.Cluster, model: Int, reads: mutable.Map[Dependency, Long],
                                asked: mutable.Set[Question], reading: AgreementPass.Read): Answer[Option[(Int, Int)]] = {
    val urls = PosterEvidence.urls(digested.listings)
    if (urls.isEmpty || !evidence.hasTmdb) Answer.Known(None)
    else {
      val venue = urls.map(url => url -> posterNoted(PosterQuestion.Venue(url), reading.posters.venue(url), None, reads, asked, reading))
      val own   = posterNoted(PosterQuestion.Film(model), reading.posters.film(model), Nil, reads, asked, reading)
      // a venue poster not hashed yet names nothing, but those hashed may: a poster's veto is its own, and a later one only
      // names a nearer film — the correction is read again once it is in (the poster asked: [[StoredCorrection.waiting]])
      // — else a listing joining a corrected take, its poster not hashed yet, took the whole cluster back to the model's
      // film until it was (PL "Ktoś całkiem obcy", 2026-10-25)
      val pending = venue.exists(_._2 == Answer.Unknown)
      if (own == Answer.Unknown || venue.forall(_._2 == Answer.Unknown)) Answer.Unknown
      else farVeto(digested, model, venue, own.answered.getOrElse(Nil), pending, reads, asked, reading)
    }
  }

  /** `answer` to the poster `question`, as [[AgreementCandidates.posterRead]] reads it, the question noted in the receipt
   *  `reads` as asked. */
  private def posterNoted[A](question: PosterQuestion, answer: Answer[A], absent: A, reads: mutable.Map[Dependency, Long],
                             asked: mutable.Set[Question], reading: AgreementPass.Read): Answer[A] = {
    reads(Dependency.Asked(Question.Poster(question))) = 0L
    posterRead(question, answer, absent, reading.posters, asked)
  }

  /** The candidate the venue posters none of the take's own (`held`) comes near match instead ([[PosterEvidence.veto]]) —
   *  `Unknown` while it names none and a venue poster is not hashed yet (`pending`). */
  private def farVeto(digested: AgreementEvidence.Cluster, model: Int, venue: Seq[(String, Answer[Option[PosterHash]])], held: Seq[PosterHash],
                      pending: Boolean, reads: mutable.Map[Dependency, Long], asked: mutable.Set[Question],
                      reading: AgreementPass.Read): Answer[Option[(Int, Int)]] = {
    // the venue posters none of the take's own comes near: only they can name another film against it
    val far  = venue.collect { case (url, Answer.Known(Some(poster))) if !PosterEvidence.nearest(Seq(poster), held).exists(_ <= PosterEvidence.VetoBits) => url }.toSet
    if (far.isEmpty) if (pending) Answer.Unknown else Answer.Known(None)
    else {
      // the films one listing showing each such poster is searched by: a cluster billed at hundreds of venues is not
      // searched again whole for the few posters that might veto its take
      val showing = digested.listings.filter(listing => PosterEvidence.shows(listing) && listing.poster.exists(far)).distinctBy(_.poster)
      val films   = titledFilms(showing)
      films.foreach(film => reads(Dependency.Asked(Question.Poster(PosterQuestion.Film(film)))) = 0L)
      posterDistances(digested.id, digested.digest, showing, Some(model), asked, (_, _, _) => films, reading.posters)
        .map(found => PosterEvidence.veto(Some(model), found)) match {
        case Answer.Known(None) if pending => Answer.Unknown
        case read                          => read
      }
    }
  }
}
