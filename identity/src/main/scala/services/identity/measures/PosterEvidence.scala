package services.identity.measures

import services.titles.billing.BillingPieces

import services.titles.TitleContainment
import services.identity.vocabulary.PosterHash

/**
 * What a cluster's venue posters say about its candidate films — a bounded guard beside the calibrated model, which
 * has no poster signal (posters are fetched after the model, for the clusters it leaves unmatched):
 *
 *  - a VOTE: the one candidate a venue poster matches within [[VoteBits]], no other candidate within as near, is the
 *    cluster's film. Measured with this hash on hand-labelled unmatched listings over the resolver's own candidates
 *    (2026-10-05): at 4 bits 19 right, the wrongs a stage relay (a house reusing last season's artwork for this
 *    season's Nutcracker) and a namesake TMDB filed with the older film's artwork; at 6 bits a short its feature's
 *    poster matched;
 *  - a VETO: a film the agreement would take is not taken when a venue poster matches ANOTHER candidate within
 *    [[VetoMatchBits]] and this film's posters stay beyond [[VetoBits]] — the poster names another film. Measured on the
 *    same pairs, it fired on 1,106 wrong films and 7 "right" ones, each of those a TMDB duplicate, a stage relay or a
 *    film the label itself had wrong (PL "Dyrygent" at Patria, whose poster is Provazník's 2025 film, not Wajda's);
 *    within 4 bits it fired on 899 and the same right ones less "Dyrygent". A veto by distance alone is none: 23% of
 *    right films' nearest TMDB poster is over 20 bits from the venue's (a still, a festival's artwork, a local poster
 *    TMDB does not keep), 7% over 28 — against 36% of wrong ones.
 *
 * Each venue poster speaks on its own (a cluster may join two venues' posters of two films — PL "Dyrygent": Kino
 * Marzenie's is Wajda's, Patria's Provaznik's): one vetoes a film it names another candidate against, and a vote stands
 * only when no poster vetoes it. A listing billing a stage work ([[ListingShape.stagesAWork]]) or several works
 * ([[ListingShape.billsSeveral]]) shows no poster of the film: a relay's artwork is the house's season, a double
 * bill's one of two films. Nor is a candidate numbering another edition than the listing ([[editionsApart]]) compared:
 * a venue reuses last year's artwork for this year's event (PL Helios's "League of Legends Worlds 26" poster is its
 * Worlds 25 file).
 */
object PosterEvidence {
  /** A venue poster this near a candidate's is a vote for it. */
  val VoteBits = 4
  /** A venue poster this near another candidate's names that film, against the film being taken. */
  val VetoMatchBits = 8
  /** A film whose posters come no nearer than this to a venue poster another candidate matches is vetoed. */
  val VetoBits = 10
  /** How many of a film's TMDB posters are hashed: its own language's, then English, then language-neutral, by votes.
   *  Raising it asks again every film filed holding as many as the old cap (`PosterAnswerStore`'s cap mark). */
  val FilmPosters = 8

  /** Does `listing`'s poster speak for its film? Not when its source's poster is no evidence ([[SourceReliability]]: a
   *  feed's is the poster of the entry it linked, which votes for that entry whether or not it is the venue's film — DE
   *  "To The Bone": Filmstarts' poster of Erin Li's 2014 short). */
  def shows(listing: Listing): Boolean =
    SourceReliability.trust(listing, SourceReliability.Fact.Poster) != SourceReliability.Trust.Ignore &&
      !ListingShape.stagesAWork(listing) && !ListingShape.billsSeveral(listing)

  /** The posters `listings` show, by URL. */
  def urls(listings: Seq[Listing]): Seq[String] = listings.filter(shows).flatMap(_.poster).distinct.sorted

  /** The nearest any of `film`'s posters comes to any of `venue`, if both hold one. */
  def nearest(venue: Seq[PosterHash], film: Seq[PosterHash]): Option[Int] =
    venue.flatMap(v => film.map(v.distance)).minOption

  /** The film the venue posters vote for, with its distance: the one candidate any poster matches within [[VoteBits]],
   *  no poster vetoing it. `posters`: each venue poster's nearest distance to each candidate. */
  def vote(posters: Seq[Map[Int, Option[Int]]]): Option[(Int, Int)] =
    posters.flatMap(_.collect { case (film, Some(bits)) if bits <= VoteBits => film -> bits }).groupMapReduce(_._1)(_._2)(math.min).toSeq match {
      case Seq(only) if veto(Some(only._1), posters).isEmpty => Some(only)
      case _                                                 => None
    }

  /** The candidate a venue poster names against the TMDB film `taken` (`None`: a film TMDB holds no record of), with its
   *  distance: one within [[VetoMatchBits]] of a poster `taken` stays beyond [[VetoBits]] of, or shows no poster at all. */
  def veto(taken: Option[Int], posters: Seq[Map[Int, Option[Int]]]): Option[(Int, Int)] =
    posters.flatMap { distances =>
      Option.when(taken.flatMap(distances.get).flatten.forall(_ > VetoBits))(())
        .flatMap(_ => distances.toSeq.collect { case (film, Some(bits)) if !taken.contains(film) && bits <= VetoMatchBits => film -> bits }
          .sortBy(c => (c._2, c._1)).headOption)
    }.sortBy(c => (c._2, c._1)).headOption

  private val Digits = "\\d+".r
  /** The numbers a title writes (a year as its last two digits, "2026" as 26; "Worlds25" as 25) — one too long for an
   *  int numbers no edition ("Pi 3.14159265358"). */
  private def numbersOf(title: String): Set[Int] =
    Digits.findAllIn(title).flatMap(_.toIntOption).map(n => if (n >= 1900 && n <= 2099) n % 100 else n).toSet

  /** Do the listing's title and the film's number themselves apart — both carry a number, none in common: another
   *  edition of an event, another instalment ("League of Legends Worlds 26" against "… Worlds25")? Where the film's
   *  titles number nothing, a year the listing bills dates it instead: a film released more than a year from every
   *  year billed is another edition ("Disney Junior Cinema Club 2026" against TMDB's 2024 one) or another film of the
   *  name ("Siostry (1972)" against the 2005 "Siostry"). */
  def editionsApart(listing: Listing, film: Film): Boolean = billsAnEpisode(listing, film) || {
    val filed = film.titles.flatMap(numbersOf).toSet
    if (filed.nonEmpty) {
      val billed = numbersOf(listing.rawTitle) ++ numbersOf(listing.title)
      billed.nonEmpty && (billed intersect filed).isEmpty
    } else numbersAnInstalment(listing, film) || film.year.exists { released =>
      val billed = yearsOf(listing.rawTitle) ++ yearsOf(listing.title)
      billed.nonEmpty && billed.forall(year => !FactRelations.yearsNear(year, released))
    }
  }

  /** Does a year the listing's title bills date it away from the film — every year it bills more than one from the film's
   *  release, and none of them a year the film's own titles carry? DE "Weihnachten mit den Drei Herren 2026", a band's
   *  Christmas show, is no 1927 film. Years alone, not every number [[editionsApart]] reads: an anniversary ("Manhunter -
   *  40th Anniversary") numbers no edition against a record titled with its own year. Of a title in pieces, only the
   *  pieces naming the film date it: a festival's year beside the work ("O vecech nadprirozených - Von übernatürlichen
   *  Dingen | Tschechischer Filmmittwoch | TDKT 2026") dates the festival, not the 1958 film. A whole title is read as its
   *  pieces are ([[services.titles.billing.BillingParse.pieces]]), its seasons and bracketed years out: "Casablanca (2026)" brackets the
   *  year it is screened, as "Casablanca (2026) | Klassiker" does. Nor does the year an anniversary the title bills
   *  falls in ("Jaws 50th Anniversary 2025", the 1975 film's fiftieth) date it away. The agreement's guard
   *  ([[services.identity.agreement.Agreement.agreed]]); [[editionsApart]] is the fill's. That year, if one does. */
  def datingApart(listing: Listing, film: Film): Option[Int] = film.year.flatMap { released =>
    val named = film.titles.map(TitleContainment.tokens).filter(_.nonEmpty)
    lazy val anniversary = IdentityMeasures.anniversaryOf(Seq(listing.rawTitle, listing.title) ++ listing.originalTitle)
    val billed = Seq(listing.rawTitle, listing.title).distinct.flatMap { title =>
      val pieces = BillingPieces.pieces(title)
      (if (pieces.sizeIs <= 1) pieces else pieces.filter(piece => named.exists(TitleContainment.tokens(piece).containsSlice(_)))).flatMap(yearsOf)
    }.toSet.filterNot(year => anniversary.exists(years => FactRelations.yearsNear(year - years, released)))
    Option.when(billed.nonEmpty && billed.forall(year => !FactRelations.yearsNear(year, released)) &&
      !film.titles.exists(yearsOf(_).exists(billed)))(billed.min)
  }

  /** Does a title piece of the listing number the very title the film's own titles number none of — "Begegnungen der
   *  Seele 2" against "Begegnungen der Seele", its first part? A sequel TMDB titles otherwise ("Avatar 2", "Avatar: The
   *  Way of Water") is none of it. */
  private def numbersAnInstalment(listing: Listing, film: Film): Boolean = {
    val filmKeys = film.titles.map(IdentityMeasures.key).filter(_.nonEmpty).toSet
    Seq(listing.rawTitle, listing.title).distinct.flatMap(title => BillingPieces.pieces(title) :+ title).exists {
      case Instalment(base, _) => filmKeys(IdentityMeasures.key(base))
      case _                   => false
    }
  }

  /** Does a title piece of the listing bill a series' named episode ("LOVE ISLAND - EPISODE NATURE") the film's titles
   *  bill none of? An episode numbered ("Star Wars: Episode IV") is a film's own. */
  private def billsAnEpisode(listing: Listing, film: Film): Boolean =
    !film.titles.exists(title => EpisodeWord.findFirstIn(title).isDefined) &&
      Seq(listing.rawTitle, listing.title).distinct.flatMap(BillingPieces.pieces).exists {
        case NamedEpisode(name) => !Numeral.matches(name.trim.takeWhile(!_.isWhitespace))
        case _                  => false
      }
  private val EpisodeWord  = """(?iu)\b(?:episode|folge|odcinek|episodio)\b""".r
  private val NamedEpisode = """(?iu)(?:episode|folge|odcinek|episodio)\s+(\S.*)""".r
  private val Numeral      = """(?i)\d+\.?|[ivxlc]+""".r

  private val Instalment = """(.*\S)\s+(\d|II|III|IV)""".r
  private val Year = """(?<!\d)(?:19|20)\d\d(?!\d)""".r
  private def yearsOf(title: String): Set[Int] = Year.findAllIn(title).map(_.toInt).toSet
}
