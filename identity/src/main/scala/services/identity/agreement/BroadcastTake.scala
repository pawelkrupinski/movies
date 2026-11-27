package services.identity.agreement

import scala.collection.mutable

import services.identity.measures.{Listing, ResolverDecision}
import services.identity.vocabulary.Question

/** The BROADCAST take of a cluster the families agreed on no film for ([[AgreementTakes.BroadcastTake]]): the one record
 *  of the billed stage work broadcast on one of its screening days ([[Broadcast]]), of the TMDB films the cluster's
 *  `candidates` reach, read through `evidence` ([[AgreementEvidence]]). */
private[agreement] final class BroadcastTake(evidence: AgreementEvidence, candidates: AgreementCandidates) {
  import candidates.{broadcastOf, candidatesOf, undatedOf, uncatalogued}

  /** The decision with the one record the cluster's screening days name taken ([[Broadcast]]): of the TMDB films its own
   *  evidence reaches, none denied — none while one of their records is not known yet, nor while a record of the billed
   *  work states no day only because the store filed it with its year alone: that record is noted in `asked`, to be
   *  read again. */
  def broadcast(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing], asked: mutable.Set[Question]): Option[ResolverDecision] =
    evidence.looked.filter(_ => Broadcast.onScreeningDays(listings)).flatMap { lookups =>
      val records = (candidatesOf(id, digest, listings) ++ uncatalogued(listings, lookups)).distinct.map(film => film -> lookups.film(film))
      // a record given up on is decided without (fix 4b); one not answered yet holds the join
      Option.when(!records.exists(_._2.isAwaited))(records.flatMap { case (film, record) => record.answered.flatten.map(film -> _) })
        .flatMap { known =>
          // a record of the billed work holding only its year is read again before the day decides: it might be the one
          broadcastOf(listings, known, asked, undatedOf(lookups))
            .left.map(asked ++= _.map(Question.Record(_))).toOption.flatten
        }
        .map(taken => AgreementTakes.taking(decision)(ResolverDecision.Basis.Broadcast, taken.line, film = Some(taken.film)))
    }
}
