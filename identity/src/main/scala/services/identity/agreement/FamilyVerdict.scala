package services.identity.agreement

import services.identity.VoterFamily

/** A family's identification of a cluster: its own id and record of the film it took, when it took one. */
final case class FamilyPick(family: VoterFamily, id: String, record: SourceRecord)

/** What a family made of a cluster: the film it took, if any, every film's record it weighed on the way, and the film its
 *  evidence `leaning` favours though no rule took it ([[Agreement.leaningOf]]) — a family that weighed a film and took
 *  none, leaning to another, has looked at it and turned it down. */
final case class FamilyVerdict(family: VoterFamily, pick: Option[FamilyPick], weighed: Seq[SourceRecord] = Nil,
                               leaning: Option[SourceRecord] = None)

/** The film a cluster's families agree on, with the families that named it, those whose evidence leaned to it, and what
 *  else corroborated it ([[Agreement.ListingFacts]], [[Agreement.ModelVote]]). */
final case class AgreedFilm(families: Set[VoterFamily], record: SourceRecord, ids: Map[VoterFamily, String], leaning: Set[VoterFamily] = Set.empty,
                            corroborated: Set[String] = Set.empty) {
  /** The film's id in `database` ("imdb", "tmdb"), as any agreeing family's record links it. */
  def crossId(database: String): Option[String] = record.crossIds.get(database)
}
