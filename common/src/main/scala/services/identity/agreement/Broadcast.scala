package services.identity.agreement

import services.identity.{DecorationSegments, IdentityMeasures, Listing, ScreeningDays}
import services.identity.IdentityMeasures.{Category, Film}


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
   *  director, its season, or a banner spelling another house. `None`: no such record, or two. */
  def take(listings: Seq[Listing], measured: Listing => IdentityMeasures.Listing, records: Seq[(Int, Film)]): Option[Taken] =
    billed(listings, measured).flatMap(chosen(_, _, records))

  /** [[take]], unless a record it would weigh but for its day — every listing's billing fits it, and it states no release
   *  day — has a day not known yet (`undated`: a stored record filed when records kept only TMDB's year): `Left` those
   *  records, which the take waits for, since any one might be the production broadcast on the cluster's day. */
  def takeOrWait(listings: Seq[Listing], measured: Listing => IdentityMeasures.Listing, records: Seq[(Int, Film)])(
      undated: Int => Boolean): Either[Seq[Int], Option[Taken]] =
    billed(listings, measured).fold[Either[Seq[Int], Option[Taken]]](Right(None)) { (days, billing) =>
      val waits = records.collect { case (id, film) if film.released.isEmpty && billing.forall(_.fits(film)) && undated(id) => id }.distinct
      if (waits.nonEmpty) Left(waits) else Right(chosen(days, billing, records))
    }

  private def chosen(days: ScreeningDays, billing: Seq[Billing], records: Seq[(Int, Film)]): Option[Taken] = {
    val fitting = records.filter { case (_, film) => film.released.isDefined && billing.forall(_.fits(film)) }.distinctBy(_._1)
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
  private final case class Billing(works: Set[String], season: Option[Int], banner: Set[String], listing: IdentityMeasures.Listing) {
    /** Does the listing name the production of a record: a record of one of its works, its season, a year and a director
     *  its facts do not deny, and a house its banner spells? */
    def fits(film: Film): Boolean =
      IdentityMeasures.stageWorks(film).exists(works) &&
        season.forall(own => IdentityMeasures.filmSeason(film).forall(_ == own)) &&
        listing.statedYear.forall(year => film.year.forall(filmYear => math.abs(filmYear - year) <= services.resolution.YearWindow.PublishedAdjacency)) &&
        !(listing.directors.nonEmpty && film.directors.exists(_.nonEmpty) &&
          IdentityMeasures.directorRelation(listing.directors, film.directors.get) == Category("different")) &&
        (banner.isEmpty || Billing.spells(banner, Billing.bannerOf(film.titles, _ => false)))
    /** Does the title mark itself a relay of a house or a season — what an encore after the broadcast day needs? */
    def marked: Boolean = banner.nonEmpty || season.isDefined
  }

  private object Billing {
    def of(listing: Listing, measured: IdentityMeasures.Listing): Billing = {
      val titles = Seq(listing.title, listing.rawTitle).distinct
      val named  = measured.seasonYear.isDefined
      Billing(IdentityMeasures.stageWorksBilled(titles, named), measured.seasonYear, bannerOf(titles, IdentityMeasures.billedIn(_, named).nonEmpty), measured)
    }

    /** The words of the titles' pieces that bill no stage work (`billsWork`, else the work a whole piece names). */
    def bannerOf(titles: Seq[String], billsWork: String => Boolean): Set[String] =
      titles.flatMap(IdentityMeasures.pieces)
        .filterNot(piece => billsWork(piece) || services.identity.StageWorks.resolver.named(IdentityMeasures.key(piece)).nonEmpty)
        .flatMap(services.movies.TitleContainment.tokens)
        .filter(word => word.length >= 4 && !word.forall(_.isDigit) && !DecorationSegments.EventWords(word)).toSet

    /** Does a listing's banner spell the record's house: every word of it the house's (an abbreviation: "Opera" of "The
     *  Metropolitan Opera"), or two of them at least ("Royal Ballet" of "Royal Ballet & Opera")? One shared word is a
     *  coincidence of vocabulary — the Paris Opera's banner shares only "opera" with the Met's. */
    def spells(banner: Set[String], house: Set[String]): Boolean = banner.subsetOf(house) || (banner intersect house).sizeIs >= 2
  }
}
