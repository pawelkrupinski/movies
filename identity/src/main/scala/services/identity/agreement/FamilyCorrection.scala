package services.identity.agreement

import scala.collection.mutable

import services.identity.measures.Listing
import services.identity.vocabulary.{Answer, Dependency, Question, VoterFamily}

/** The FAMILIES' evidence against a film the MODEL took ([[CorrectionReading]], [[Correction.Families]]), asked only of
 *  a take another kind of evidence questions — and what it decides with the posters' veto and the venues' programmes
 *  ([[Correction.decide]]). Read through `evidence` ([[AgreementEvidence]]) and the cluster's `candidates`, the films
 *  named by the correction's `records`; the family whose site `listedOn` lists the programmes is no second kind of
 *  evidence beside them. */
private[agreement] final class FamilyCorrection(evidence: AgreementEvidence, candidates: AgreementCandidates, records: CorrectionRecords,
                                                listedOn: Option[VoterFamily]) {
  import evidence.tried

  /** What the evidence against the take `model` decides ([[Correction.decide]]): (a) the posters' veto (`vetoed`: the
   *  candidate a venue poster matches, with its bits), (e) the record the venues' programmes list (`programme`) and (b)
   *  the families — asked only when the posters `questioned` the take or a programme contradicts it. */
  def decided(digested: AgreementEvidence.Cluster, model: Int, vetoed: Option[(Int, Int)], questioned: Boolean,
              programme: Option[SourceRecord], reads: mutable.Map[Dependency, Long], asked: mutable.Set[Question]): Option[Correction.Outcome] =
    records.source(model).flatMap { modelRecord =>
      val listings = digested.listings
      // the TMDB films the take's evidence reaches besides it: what a family's or the programme's record is matched to
      lazy val reached = (vetoed.map(_._1).toSeq ++ candidates.candidatesOf(digested.id, digested.digest, listings)).distinct.filter(_ != model)
      val against = if (programme.isEmpty && !questioned) None
        // beside the programmes, the family whose site lists them is no second kind of evidence: its take is not counted
        else familiesAgainst(listings, model, modelRecord, reached, programme.flatMap(_ => listedOn), reads, asked).answered.flatten
      val contradictions = programme.toSeq.map { record =>
        // the TMDB film the programme's record is: one the other evidence or the cluster's own candidates reach
        val same = (reached ++ against.map(_._1)).distinct.filter(id => records.source(id).exists(SameFilm.sameFilm(record, _)))
        Correction.Against(Correction.Filmweb, same.headOption.filter(_ => same.sizeIs == 1),
          s"the venues' Filmweb programmes list ${record.film.titled} on their days")
      } ++ vetoed.toSeq.map { case (other, bits) =>
        Correction.Against(Correction.Poster, Some(other), s"a venue poster matches ${records.title(other)} ($bits bits), not the take")
      } ++ against.toSeq.map { case (other, says) => Correction.Against(Correction.Families, Some(other), says.replace(s"tmdb $other", records.title(other))) }
      Correction.decide(records.title(model), contradictions, records.sharesDirector(_, model))
    }

  /** The film more of the families take than take the model's, by its TMDB id — a pick's TMDB or IMDb id, else the one
   *  film of `reached` its record is by its facts — with what they say, `uncounted`'s take aside; `Unknown` while a
   *  family's question, or TMDB's find of a taken IMDb id, is not answered yet — and not attempted: a family attempted and
   *  not answered is left out as no verdict, as a settled cluster leaves it (the settle rule). */
  private def familiesAgainst(listings: Seq[Listing], model: Int, modelRecord: SourceRecord, reached: => Seq[Int],
                              uncounted: Option[VoterFamily], reads: mutable.Map[Dependency, Long], asked: mutable.Set[Question]): Answer[Option[(Int, String)]] = {
    val gaps     = mutable.Set.empty[Question]
    val verdicts = evidence.verdictsOf(listings, gaps, reads)
    asked ++= gaps
    if (verdicts.contains(Answer.Unknown) && !gaps.forall(tried)) Answer.Unknown
    else {
      val mapped = verdicts.flatMap(_.answered).flatMap(_.pick).filterNot(pick => uncounted.contains(pick.family))
        .map(pick => pick -> pickedFilm(pick, reached, asked))
      if (mapped.exists(_._2 == Answer.Unknown)) Answer.Unknown
      else Answer.Known(outvoting(mapped, model, modelRecord))
    }
  }

  /** The TMDB film a family's `pick` is: its record's TMDB or IMDb id ([[AgreementEvidence.tmdbFilmOf]]) — a record
   *  linking none (Filmweb's) the one film the take's evidence reaches (`reached`) that its facts are. */
  private def pickedFilm(pick: FamilyPick, reached: => Seq[Int], asked: mutable.Set[Question]): Answer[Option[Int]] =
    evidence.tmdbFilmOf(pick.record.crossIds.get("tmdb").flatMap(_.toIntOption), pick.record.crossIds.get("imdb"), asked) match {
      case Answer.Known(None) => Answer.Known(reached.filter(film => records.source(film).exists(SameFilm.sameFilm(pick.record, _))) match {
        case Seq(one) => Some(one)
        case _        => None
      })
      case found => found
    }

  /** The film more families take (`mapped`: each pick with its TMDB film) than take the model's, alone at that count —
   *  with what they say. */
  private def outvoting(mapped: Seq[(FamilyPick, Answer[Option[Int]])], model: Int, modelRecord: SourceRecord): Option[(Int, String)] = {
    val forModel = mapped.collect { case (pick, id) if id.answered.flatten.contains(model) || SameFilm.sameFilm(pick.record, modelRecord) => pick.family }
    val others   = othersTaken(mapped, model, forModel)
    others.headOption.filter { case (_, takers) => takers.size > forModel.size && !others.drop(1).exists(_._2.size == takers.size) }
      .map { case (id, takers) =>
        id -> (s"${takers.map(_.label).sorted.mkString(", ")} take tmdb $id" +
          (if (forModel.isEmpty) ", none the take" else s", ${forModel.map(_.label).sorted.mkString(", ")} the take"))
      }
  }

  /** The films other than `model` the families not taking it (`forModel` aside) take, each with its takers, most taken
   *  first. */
  private def othersTaken(mapped: Seq[(FamilyPick, Answer[Option[Int]])], model: Int, forModel: Seq[VoterFamily]): Seq[(Int, Seq[VoterFamily])] =
    mapped.collect { case (pick, Answer.Known(Some(id))) if id != model && !forModel.contains(pick.family) => id -> pick.family }
      .groupMap(_._1)(_._2).toSeq.sortBy { case (id, takers) => (-takers.size, id) }
}
