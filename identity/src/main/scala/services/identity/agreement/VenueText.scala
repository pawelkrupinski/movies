package services.identity.agreement

import services.identity.measures.Listing
import services.identity.vocabulary.{DetailFacts, ProseWords}

/** What a cluster's VENUES say of it in their own words — each listing's synopsis and cast, and its venue's detail
 *  page's — none of a listing whose facts a feed catalogue states, whose text describes the catalogue's own entry, maybe
 *  another film than the venue's ([[services.identity.measures.SourceReliability]]): the people the cast evidence reads
 *  ([[names]]) and the synopsis that refuses an agreed or filled film ([[synopsisAgainst]]). Read through `evidence`
 *  ([[AgreementEvidence]]) and the cluster's `candidates`. */
private[agreement] final class VenueText(evidence: AgreementEvidence, candidates: AgreementCandidates) {
  import evidence.{looked, venues}
  import candidates.{candidatesOf, titledFilms}

  /** The people the listings' VENUES name — each listing's own synopsis and cast, and its venue's detail page's — that the
   *  cast evidence reads ([[services.identity.CastEvidence]]): none of a listing whose facts a feed catalogue states, whose
   *  text describes the catalogue's own entry, maybe another film than the venue's. */
  def names(listings: Seq[Listing]): services.identity.vocabulary.VenueNames =
    venueStated(listings).foldLeft(services.identity.vocabulary.VenueNames.None) { case (named, (listing, page)) =>
      named ++ listing.names ++ page.fold(services.identity.vocabulary.VenueNames.None)(_.names)
    }

  /** The listings whose text is their VENUE's own — none whose facts a feed catalogue states, its text describing the
   *  catalogue's entry ([[services.identity.SourceReliability]]) — each with its detail page's facts, where read. */
  private def venueStated(listings: Seq[Listing]): Seq[(Listing, Option[DetailFacts])] =
    listings.filter(services.identity.measures.SourceReliability.decides(_, services.identity.measures.SourceReliability.Fact.Names))
      .map(listing => listing -> venues.detail(listing).answered.flatten)

  /** Why the venues' own synopses refuse `film` ([[SynopsisEvidence.veto]]): they name, among the TMDB descriptions of the
   *  cluster's candidates, another's over its own. None where no venue text or no description of `film` is known: the
   *  descriptions are read as the cast is, never waited on. The venue's words are those of its listings and their detail
   *  pages, none of a listing whose facts a feed catalogue states, as [[names]] reads them. */
  def synopsisAgainst(id: String, digest: Long, listings: Seq[Listing], film: Int): Option[String] =
    looked.flatMap { lookups =>
      val venue = venueStated(listings).foldLeft(ProseWords.None) { case (words, (listing, page)) =>
        words ++ listing.words ++ page.fold(ProseWords.None)(_.words)
      }
      Option.when(venue.nonEmpty)(()).flatMap(_ => descriptionIn(lookups, film)).flatMap { own =>
        // the cluster's candidates, else what its titles' searches answered: a search not answered yet (a gap the model's
        // resolve met) leaves the candidates unread, and a namesake its other searches found is a namesake still
        val namesakes = Some(candidatesOf(id, digest, listings)).filter(_.nonEmpty).getOrElse(titledFilms(listings, answeredOnly = true))
          .distinct.filter(_ != film)
          .flatMap(other => descriptionIn(lookups, other).map(other -> _))
        val people = ProseWords.ofWords(lookups.cast(film).answered.flatten.getOrElse(Nil) ++ recordIn(lookups, film).flatMap(_.directors).getOrElse(Nil))
        val titles = ProseWords.ofWords(listings.flatMap(listing => Seq(listing.rawTitle, listing.title, listing.cleanTitle)) ++
          (film +: namesakes.map(_._1)).flatMap(recordIn(lookups, _).toSeq.flatMap(_.titles)))
        SynopsisEvidence.veto(venue, own, people, namesakes, titles).map { case (other, shared) =>
          def titled(of: Int) = recordIn(lookups, of).fold(s"tmdb $of")(_.titled)
          s"no agreement on ${titled(film)}: the venue's synopsis names $shared words of ${titled(other)}'s TMDB description and none of its own"
        }
      }
    }

  /** TMDB's description of `film` as `lookups` read it, if known and not empty. */
  private def descriptionIn(lookups: services.identity.IdentityLookups, film: Int): Option[ProseWords] =
    lookups.overview(film).answered.flatten.filter(_.nonEmpty)

  /** TMDB's record of `film` as `lookups` read it, if known. */
  private def recordIn(lookups: services.identity.IdentityLookups, film: Int): Option[services.identity.measures.Film] =
    lookups.film(film).answered.flatten
}
