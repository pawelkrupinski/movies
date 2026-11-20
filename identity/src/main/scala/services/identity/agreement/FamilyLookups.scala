package services.identity.agreement

import scala.collection.mutable

import services.identity.IdentityLookups
import services.identity.measures.Film
import services.identity.measures.Listing
import services.identity.vocabulary.{Answer, CandidateQuery, DetailFacts, Hit}
import services.titles.TitleContainment

/** A family's answers as the resolver's lookups — the family as the film database, as TMDB is to the model. Its ids
 *  are numbered as this resolve meets them (it asks its questions in one sorted order), so the resolve is a function of
 *  the answers; the venues' own detail pages come from `venues`. */
final class FamilyLookups(answers: FamilyAnswers, venues: IdentityLookups, listingTitles: Seq[String] = Nil) extends IdentityLookups {
  private val numbers = mutable.LinkedHashMap.empty[String, Int]
  private val named   = mutable.HashMap.empty[Int, SourceHit]
  /** Each numbered film's best place in a search that named it: what [[fetched]] reads. */
  private val bestRank = mutable.HashMap.empty[Int, Int]
  private val listingTokens: Set[String] = listingTitles.flatMap(TitleContainment.tokens).toSet
  private val read    = mutable.LinkedHashMap.empty[String, SourceRecord]
  /** Every film's record this resolve read, in the order it read them. */
  def weighed: Seq[SourceRecord] = read.values.toSeq
  private def hitOf(hit: SourceHit): Hit = {
    val number = numbers.getOrElseUpdate(hit.id, numbers.size + 1)
    named.getOrElseUpdate(number, hit)
    Hit(number, hit.title, hit.originalTitle, hit.year, 0.0)
  }
  /** The family's own id of a film this resolve numbered. */
  def idOf(number: Int): Option[String] = named.get(number).map(_.id)
  override def numbersFilmsByTmdbId: Boolean = false

  override def hasDetail(listing: Listing): Boolean                  = venues.hasDetail(listing)
  override def detail(listing: Listing): Answer[Option[DetailFacts]] = venues.detail(listing)
  override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = query match {
    case CandidateQuery.Title(text) if answers.family.latinTitlesOnly && !tools.LatinScript.hasLetter(text) => Answer.Known(Nil)
    case CandidateQuery.Title(text)    => answers.titled(text).map(ranked)
    case CandidateQuery.Director(_) if !answers.family.searchesDirectors => Answer.Known(Nil)
    case CandidateQuery.Director(name) => answers.directedBy(name).map(ranked)
    case _                             => Answer.Known(Nil)
  }
  private def ranked(hits: Seq[SourceHit]): Seq[Hit] = hits.zipWithIndex.map { case (hit, rank) =>
    val numbered = hitOf(hit)
    bestRank.updateWith(numbered.tmdbId)(was => Some(was.fold(rank)(_ min rank)))
    numbered
  }

  /** Is a numbered film's record worth asking for? Its search ranked it first, or its title shares a word with the
   *  listing's, or the search gave no title to judge it by — and in any case within the first [[FamilyLookups.Records]]
   *  a search returned. A hit failing that is read as no record (`film` is `None`): its question is never asked.
   *  Replayed on prod's answers (2026-10-05): ~29% fewer questions, 45% fewer record fetches, no agreed film lost —
   *  the first hit kept whatever its title (it rescues translated titles), the cap at 6 (4 lost one). */
  private def fetched(number: Int, hit: SourceHit): Boolean = bestRank.get(number).forall { rank =>
    rank < FamilyLookups.Records && (rank == 0 || {
      val titles = (Seq(hit.title) ++ hit.originalTitle).filter(_.nonEmpty)
      titles.isEmpty || titles.exists(title => TitleContainment.tokens(title).exists(listingTokens))
    })
  }

  override def film(number: Int): Answer[Option[Film]] =
    named.get(number).filter(fetched(number, _)).fold[Answer[Option[Film]]](Answer.Known(None)) { hit =>
      val answer = answers.record(hit.id)
      answer.answered.flatten.foreach(record => read(hit.id) = record)
      answer.map(_.map(_.film))
    }
}

object FamilyLookups {
  /** How many of a search's films are read at most. */
  val Records = 6
}
