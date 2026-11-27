package services.identity.agreement

import scala.collection.mutable

import services.identity.measures.{Listing, ResolverDecision}
import services.identity.vocabulary.{Answer, Question, VoterFamily}

/** The FILL take of a cluster the families agreed on no film for ([[AgreementTakes.FillTake]]): the film the first
 *  selected unified fill rule takes ([[filledOf]], filed with the families' verdict), taken unless a venue poster vetoes
 *  it or the venue's synopsis refuses it (`venueText`, [[VenueText.synopsisAgainst]]) — read through `evidence`
 *  ([[AgreementEvidence]]) and the cluster's `candidates`, by the selected `rules`. */
private[agreement] final class FillTake(evidence: AgreementEvidence, candidates: AgreementCandidates,
                                        rules: services.identity.agreement.UnifiedRules, venueText: VenueText) {
  import candidates.{measured, productionsOf}
  import AgreementTakes.{taking, Take}

  /** The film the first selected FILL rule takes for a cluster the families agreed on nothing for
   *  ([[services.identity.agreement.UnifiedRules.filled]] over [[services.identity.agreement.UnifiedEvidence.contenders]]: the cluster's own
   *  TMDB candidates, none denied, and the films the families took or lean to) — `Unknown` while a TMDB answer the
   *  cluster's candidates need is a gap. The venue posters' veto is read when it is taken ([[filledTake]]). Everything it
   *  reads is noted in `receipt`, the verdict's. */
  def filledOf(decision: ResolverDecision, listings: Seq[Listing], verdicts: Seq[FamilyVerdict], stated: Seq[Listing],
               thisYear: Int, asked: mutable.Set[Question], receipt: mutable.Map[services.identity.vocabulary.Dependency, Long]): Answer[Option[StoredFill]] =
    if (rules.fill.isEmpty || !evidence.hasTmdb) Answer.Known(None)
    else evidence.tmdbNodesOf(listings).fold[Answer[Option[StoredFill]]](Answer.Unknown) { nodes =>
      // the `broadcast.take` signal read as the stage's own take reads the join: the same productions, the same wait
      val cluster = services.identity.agreement.UnifiedEvidence.ClusterEvidence(listings, decision, nodes, verdicts, Nil, _ => None, thisYear, stated,
        Some(measured), venueText.names(listings), evidence.castOf,
        productions = () => productionsOf(listings, asked, Some(receipt)), undated = { film =>
          val attempt = services.identity.vocabulary.Dependency.Attempted(Question.Record(film))
          evidence.readNow(attempt).foreach(receipt(attempt) = _)
          candidates.undated(film)
        })
      Answer.Known(rules.filled(services.identity.agreement.UnifiedEvidence.contenders(cluster)).map { case (film, rule) =>
        StoredFill(rule, film.tmdb, film.imdb, rules.explain(film, rule)) })
    }

  /** The decision of the cluster `id` (its `listings` digesting to `digest`) with the film a fill rule took on its
   *  families' `verdict` — pending while TMDB was not asked about its IMDb id yet or the venue posters are not all hashed,
   *  and not taken when a venue poster names another candidate ([[services.identity.measures.PosterEvidence.veto]]) or
   *  the venue's synopsis another's description. */
  def filledTake(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing], verdict: StoredVerdict,
           finding: mutable.Set[Question], distances: AgreementTakes.Distances): Option[ResolverDecision] =
    verdict.filled.flatMap { fill =>
      // a find given up on (Failed) names no TMDB film: the fill stands on its IMDb id (fix 4: decided on, never awaited)
      evidence.tmdbFilmOf(fill.tmdb, fill.imdb, finding).failedAs(None) match {
        case Answer.Unknown => None
        case Answer.Failed  => None // failedAs leaves none
        case Answer.Known(tmdbFilm) => Take.screened(tmdbFilm, distances, film => venueText.synopsisAgainst(id, digest, listings, film))(()) match {
          case Take.Taken(_) =>
            val rule = Some(s"fill:${fill.rule}")
            tmdbFilm.map(id => taking(decision)(ResolverDecision.Basis.Filled, fill.line, film = Some(id), rule = rule))
              .orElse(fill.imdb.map(id => taking(decision)(ResolverDecision.Basis.Filled, fill.line, rule = rule,
                fallback = Some(ResolverDecision.Fallback(VoterFamily.Imdb.database, id, 1.0)))))
          case _ => None
        }
      }
    }
}
