package services.identity.agreement

import services.identity.{Answer, DecorationSegments, FactRelations, IdentityMeasures, Listing, ListingShape, ScreeningDays}
import services.identity.IdentityMeasures.Film


/**
 * The BROADCAST DATE JOIN: a stage relay airs on its house's published dates — the Met's "Samson et Dalila" live on
 * 5 December 2026 — and TMDB dates each production's record by its broadcast. A cluster billing a stage work
 * ([[IdentityMeasures.stageWorksBilled]]) whose venues screen it on the very day one record of that work was
 * broadcast is that production, whatever its title leaves out: PL "Samson i Dalila" names neither the house nor the
 * season, yet on 5 December 2026 it can only be the Met's. A strong prior, not the only date: an encore or a delayed
 * retransmission airs within [[EncoreDays]] after it, which a title BILLING the record's house or season still names.
 *
 * Read over the films the cluster's own evidence reaches, none of them denied, on the agreement's way to the
 * projection — the days a venue screens on are no fact the venue-free model reads ([[Listing.screenings]]).
 */
object Broadcast {

  /** How long after its live broadcast a production's encores and retransmissions run. */
  val EncoreDays = 60

  /** A record the cluster's screenings take, and why. */
  final case class Taken(film: Int, line: String)

  /** The one record the cluster's screenings name: broadcast on one of the days the cluster screens on, else — every
   *  listing billing the record's house or its season — the one broadcast within [[EncoreDays]] before one. Every record
   *  must bill a stage work each listing bills, and no fact a listing states may stand against it: its year, its
   *  director, its season, or a banner spelling another house — unless one of `productions` (film databases' records of
   *  the house's productions) credits the record's production with the director the listing credits. `Right(None)`: no
   *  such record, or two.
   *
   *  `Left`, the take WAITING, where a record it would weigh but for its day — every listing's billing fits it, and it
   *  states no release day — has a day not known yet (`undated`: a stored record filed when records kept only TMDB's
   *  year): those records, since any one might be the production broadcast on the cluster's day. A cluster one of whose
   *  listings' days could not be read ([[ScreeningDays.Unknown]]) waits too, asking no record: `Left` none; and so does
   *  one whose `productions` are not all answered.
   *
   *  The ONE broadcast join: the stage's take, a correction's switch and the unified fill's `broadcast.take` signal all
   *  read it; a reader that cannot wait reads a `Left` as no take. */
  def take(listings: Seq[Listing], measured: Listing => IdentityMeasures.Listing, records: Seq[(Int, Film)],
           productions: () => Answer[Seq[Film]] = () => Answer.Known(Nil))(
      undated: Int => Boolean = _ => false): Either[Seq[Int], Option[Taken]] =
    if (listings.exists(_.screenings.isUnknown)) Left(Nil)
    else billed(listings, measured).fold[Either[Seq[Int], Option[Taken]]](Right(None)) { (days, billing) =>
      // the films databases' productions are read only for a record every listing's billing fits but by its banner
      val credits = if (!records.exists { case (_, film) => billing.forall(_.fitsButTheHouse(film)) && !billing.forall(_.fits(film, Nil)) })
        Answer.Known(Nil) else productions()
      credits match {
        case Answer.Unknown => Left(Nil)
        case Answer.Known(credited) =>
          val waits = records.collect { case (id, film) if film.released.isEmpty && billing.forall(_.fits(film, credited)) && undated(id) => id }.distinct
          if (waits.nonEmpty) Left(waits) else Right(chosen(days, billing, records, credited))
      }
    }

  /** How long a relay's encores run after TMDB dates its broadcast. */
  val RelayRunDays = 365

  /** Why a relay's take is not the production its venues screen, if it is not. All of these must hold:
   *  - every listing is a house's RELAY of the record: it bills a house ([[ListingShape.billsAHouse]]) and puts a banner on
   *    the work the record also bills under one ([[IdentityMeasures.billing]]: "NT Live: Hamlet", "National Theatre
   *    Live: Hamlet") — not a film whose own title holds a house's word (US "Stage Fright", Hitchcock's 1950 film);
   *  - nothing the venue states itself ties it to the record: no title dates the record's year, and no listing whose
   *    facts are its own, not a listings site's catalogue entry ([[ListingShape.venueStated]]: not Flicks' film
   *    page, which links a new relay to an old production's page), states the record's year or director;
   *  - TMDB dates the record's broadcast more than [[RelayRunDays]] before the first screening, and a later record carrying
   *    the very same title within that run before it.
   *
   *  Measured on prod 2026-10-06 (every relay take of the five countries): it withdraws US "NT Live: All My Sons" at
   *  Oriental Theatre Milwaukee (Milwaukee Film bills the 2026 van Hove broadcast; Flicks links its 2019 page) and US
   *  "NT Live: Hamlet" (Burns Court bills the 2026 Abeysekera broadcast; Flicks links Cumberbatch's 2015 page), and
   *  keeps "Stage Fright" (Camelot Theatres' Hitchcock retrospective) and "Phantom of the Opera (1943)". `measured`:
   *  the listing as the model reads it; `others`: the other records the cluster's titles find, read only once the rest
   *  holds. */
  def superseded(listings: Seq[Listing], measured: Listing => IdentityMeasures.Listing, taken: Film,
                 others: => Seq[(Int, Film)]): Option[String] = {
    val days = listings.map(_.screenings).foldLeft(ScreeningDays.None)(_ ++ _)
    val run  = (day: java.time.LocalDate) => day.minusDays(RelayRunDays.toLong)
    def tiedToTaken(listing: Listing) =
      IdentityMeasures.titleYearOf(Seq(listing.title, listing.rawTitle).distinct).exists(taken.year.contains) ||
        ListingShape.venueStated(listing) && (listing.year.exists(taken.year.contains) || FactRelations.samePerson(listing.directors, taken.directors.getOrElse(Nil)))
    for {
      first <- days.first.filter(_ => listings.nonEmpty && !days.isUnknown)
      last  <- days.last
      aired <- taken.released.filter(_.isBefore(run(first)))
      if listings.forall(listing => ListingShape.billsAHouse(listing) && IdentityMeasures.billing(measured(listing), taken).isDefined &&
        !tiedToTaken(listing))
      (_, newer) <- others.filter { case (_, film) =>
                      IdentityMeasures.key(film.title) == IdentityMeasures.key(taken.title) &&
                        film.released.exists(day => day.isAfter(aired) && !day.isBefore(run(first)) && !day.isAfter(last))
                    }.sortBy { case (id, film) => (film.released.map(_.toEpochDay).getOrElse(0L), id) }.lastOption
    } yield s"it screens from $first, over a year after its record was broadcast ($aired), and '${newer.title}'" +
      s"${newer.year.fold("")(year => s" ($year)")} was broadcast ${newer.released.get}"
  }

  private def chosen(days: ScreeningDays, billing: Seq[Billing], records: Seq[(Int, Film)], productions: Seq[Film]): Option[Taken] = {
    val fitting = records.filter { case (_, film) => film.released.isDefined && billing.forall(_.fits(film, productions)) }.distinctBy(_._1)
    val onDay   = fitting.filter { case (_, film) => film.released.exists(days.contains) }
    def encore(film: Film) = film.released.exists(aired => days.days.exists(day => !day.isBefore(aired) && !day.isAfter(aired.plusDays(EncoreDays))))
    val chosen = onDay match {
      case Seq(one) => Some(one -> "on the day")
      case Seq()    => Option.when(billing.forall(_.marked))(fitting.filter { case (_, film) => encore(film) }).collect { case Seq(one) => one -> "after the day" }
      case _        => None
    }
    chosen.map { case ((id, film), when) =>
      Taken(id, s"screens $when '${film.title}'${film.year.fold("")(year => s" ($year)")} was broadcast, ${film.released.get}")
    }
  }

  /** Does a title bill a house's production: a stage work, and a banner beside it ("The Metropolitan Opera: Macbeth")? */
  def billsAProduction(title: String): Boolean =
    IdentityMeasures.stageWorksBilled(Seq(title), false).nonEmpty && Billing.bannerOf(Seq(title), IdentityMeasures.billedIn(_, false).nonEmpty).nonEmpty

  /** The days the cluster screens on and what each of its listings bills — `None` when it screens on none, or a listing
   *  bills no stage work. */
  private def billed(listings: Seq[Listing], measured: Listing => IdentityMeasures.Listing): Option[(ScreeningDays, Seq[Billing])] = {
    val days = listings.map(_.screenings).foldLeft(ScreeningDays.None)(_ ++ _)
    if (days.isEmpty || listings.isEmpty) None
    else {
      val billing = listings.map(listing => Billing.of(listing, measured(listing)))
      Option.when(!billing.exists(_.works.isEmpty))(days -> billing)
    }
  }

  /** What one listing bills: the stage works its title names, its season, and its BANNER — the words of its pieces naming
   *  no work, four letters or more, no number or event word ("live", "retransmisja") among them. */
  private final case class Billing(works: Set[String], season: Option[Int], banner: Set[String], listing: IdentityMeasures.Listing,
                                    stated: IdentityMeasures.Listing) {
    /** Does the listing name the production of a record: a record of one of its works, its season, a year and a director
     *  its facts do not deny, and a house its banner spells — or, its banner spelling another, a production of the
     *  record's house that one of `productions` (film databases' records) credits with the director the listing credits
     *  ([[credits]])? */
    def fits(film: Film, productions: Seq[Film]): Boolean =
      fitsButTheHouse(film) && (banner.isEmpty || Billing.spells(banner, Billing.bannerOf(film.titles, _ => false)) || credits(film, productions))

    /** [[fits]] on every count but the house the banner spells. */
    def fitsButTheHouse(film: Film): Boolean =
      IdentityMeasures.stageWorks(film).exists(works) &&
        season.forall(own => IdentityMeasures.filmSeason(film).forall(_ == own)) &&
        !FactRelations.yearsApart(stated.statedYear, film.year) && !FactRelations.otherPerson(stated.directors, film.directors.getOrElse(Nil))

    /** Does a film database credit the record's production with the director the listing credits: one of `productions`
     *  billing the record's house (its banner spelling the record's) and one of the works both bill, within a year of
     *  it where both are dated, and crediting the listing's director? The venue's own credit names the production where
     *  its banner names another house: UK Flicks' "RBO Cinema Season 2026-27: La Fanciulla Del West" credits Richard
     *  Jones, who staged the Met's — IMDb's and RT's records of it say so, TMDB's credits nobody. A listing crediting
     *  nobody ("Opéra National de Paris: La fanciulla del West") is never one. */
    def credits(film: Film, productions: Seq[Film]): Boolean = listing.directors.nonEmpty && {
      val house = Billing.bannerOf(film.titles, _ => false)
      val work  = IdentityMeasures.stageWorks(film).intersect(works)
      house.nonEmpty && productions.exists { production =>
        FactRelations.samePerson(listing.directors, production.directors.getOrElse(Nil)) &&
          IdentityMeasures.stageWorks(production).exists(work) &&
          !FactRelations.yearsApart(production.year, film.year) &&
          { val own = Billing.bannerOf(production.titles, _ => false); own.nonEmpty && Billing.spells(own, house) }
      }
    }
    /** Does the title mark itself a relay of a house or a season — what an encore after the broadcast day needs? */
    def marked: Boolean = banner.nonEmpty || season.isDefined
  }

  private object Billing {
    /** Where a lower-case letter runs into an upper-case one: the seam of a camel-cased word. */
    private val CamelCase = """(?<=\p{Ll})(?=\p{Lu})""".r

    /** `measured`'s facts are what a record's year and director are read against — but a listings site's catalogue entry
     *  linked as the listing's page ([[ListingShape.venueStated]]: Flicks links an encore to an old season's page) states
     *  the catalogue's claim, not the venue's: then only the listing's own facts are. */
    def of(listing: Listing, measured: IdentityMeasures.Listing): Billing = {
      val titles = Seq(listing.title, listing.rawTitle).distinct
      val named  = measured.seasonYear.isDefined
      val stated = if (ListingShape.venueStated(listing)) measured else services.identity.Evidence.of(listing, None).measured
      Billing(IdentityMeasures.stageWorksBilled(titles, named), measured.seasonYear, bannerOf(titles, IdentityMeasures.billedIn(_, named).nonEmpty), measured,
        stated)
    }

    /** The words of the titles' pieces that bill no stage work (`billsWork`, else the work a whole piece names), a word
     *  run together in camel case read as its parts: UK Flicks' "MetOpera" is the Met's "Opera". */
    def bannerOf(titles: Seq[String], billsWork: String => Boolean): Set[String] =
      titles.flatMap(IdentityMeasures.pieces)
        .filterNot(piece => billsWork(piece) || services.identity.StageWorks.resolver.named(IdentityMeasures.key(piece)).nonEmpty)
        .flatMap(piece => services.movies.TitleContainment.tokens(CamelCase.replaceAllIn(piece, " ")))
        .filter(word => word.length >= 4 && !word.forall(_.isDigit) && !DecorationSegments.EventWords(word)).toSet

    /** Does a listing's banner spell the record's house: every word of it the house's (an abbreviation: "Opera" of "The
     *  Metropolitan Opera"), or two of them at least ("Royal Ballet" of "Royal Ballet & Opera")? One shared word is a
     *  coincidence of vocabulary — the Paris Opera's banner shares only "opera" with the Met's. */
    def spells(banner: Set[String], house: Set[String]): Boolean = banner.subsetOf(house) || (banner intersect house).sizeIs >= 2
  }
}
