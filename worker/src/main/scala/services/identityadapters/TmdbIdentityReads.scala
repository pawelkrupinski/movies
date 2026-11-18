package services.identityadapters

import services.identity.*
import clients.TmdbClient

/**
 * A film as the identity measures read it, off the answers [[TmdbClient]] fetches for it: its record, its top-billed
 * cast and the words its overviews capitalise, each parsed by the calibration's own [[TmdbFilmRecord]]. The client
 * keeps the fetch (its requests, its key, its pace); the reading is the identity model's, so it lives here and the
 * client knows nothing of it. Each is `None` without a TMDB key or when TMDB knows no such film; a failed read THROWS,
 * so the resolver's lookups can tell "no film" from "not answered".
 */
object TmdbIdentityReads {

  extension (client: TmdbClient) {

    /** The film's record, from both answers [[TmdbClient.filmAnswers]] reads — so a tree recorded for either serves it. */
    def identityRecord(tmdbId: Int): Option[IdentityMeasures.Film] =
      client.filmAnswers(tmdbId).flatMap(TmdbFilmRecord.parse).map(_._1)

    /** The film's top-billed cast off its localized record ([[TmdbClient.localizedAnswer]]): what the identity store
     *  files of it, and the cast evidence reads ([[CastEvidence]]). */
    def identityCast(tmdbId: Int): Option[Seq[String]] =
      client.localizedAnswer(tmdbId).flatMap(answer => TmdbFilmRecord.cast(Seq(answer)))

    /** The words the film's overviews capitalise, off both answers [[identityRecord]] reads: what the identity store
     *  files of them, and the synopsis evidence reads (`services.identity.agreement.SynopsisEvidence`). */
    def identityOverview(tmdbId: Int): Option[ProseWords] =
      client.filmAnswers(tmdbId).flatMap(TmdbFilmRecord.overview)
  }
}
