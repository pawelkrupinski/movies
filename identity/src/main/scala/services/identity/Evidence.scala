package services.identity

import services.identity.measures.{MeasuredListing, StageRelays}
import services.titles.TitleSeason

import services.identity.vocabulary.{DetailFacts, ListingDating, TitleDecorations}

/** A listing's evidence once its own detail page is merged in — listing values win, the page
 *  fills gaps. VENUE-FREE on purpose: two venues publishing the same evidence are asking the same
 *  question, and the resolver treats them as one node. */
final case class Evidence(title: String, cleanTitle: String, rawTitle: String, year: Option[Int], directors: Seq[String],
                          runtime: Option[Int], originalTitle: Option[String], countries: Seq[String] = Nil,
                          decorations: TitleDecorations = TitleDecorations.None, searchTitle: Option[String] = None,
                          /** [[Listing.properTitle]], searched beside `searchTitle`. */
                          properTitle: Option[String] = None,
                          /** What dates its film beside the year it states ([[ListingDating]]). */
                          dating: ListingDating = ListingDating.Undated,
                          /** The venue's certificate ([[Listing.certificate]]): what the weighted `certificate` measure reads. */
                          certificate: Option[String] = None) {
  import dating.{airDays, anniversaryScreeningYear, broadcastSeason, performanceYear, screenedSeason, screeningsSeason, yearUpperBound}

  lazy val key: String =
    Seq(title, cleanTitle, rawTitle, year.fold("")(_.toString), directors.sorted.mkString(","),
      runtime.fold("")(_.toString), originalTitle.getOrElse(""), countries.sorted.mkString(",")).mkString("\u0000") +
      broadcastSeason.fold("")(season => s"\u0000broadcast:$season") +
      screenedSeason.filterNot(broadcastSeason.contains).fold("")(season => s"\u0000screened:$season") +
      // only where a rule reads it -- it bounds what the title names, or the billing is a stage work's, whose season rules
      // read it ([[StageRelays.readsItsScreeningsSeason]]) and no season above keys it yet -- so every other listing keeps
      // its key, and a film's run past July re-resolves nothing
      screeningsSeason.filter(season => TitleSeason.of(Seq(title, rawTitle)) != TitleSeason.of(Seq(title, rawTitle), screeningsSeason) ||
        (!broadcastSeason.contains(season) && !screenedSeason.contains(season) && StageRelays.readsItsScreeningsSeason(measured)))
        .fold("")(season => s"\u0000screenings:$season") +
      (if (airDays.isEmpty) "" else airDays.mkString("\u0000aired:", ",", "")) +
      // only where a venue bills one, so every other listing keeps its key
      certificate.fold("")(c => s"\u0000certificate:$c") +
      // only where a source bounds the year, so every other listing keeps its key
      yearUpperBound.fold("")(bound => s"\u0000yearUpTo:$bound") +
      // only where a synopsis names the performance, so every other listing keeps its key
      performanceYear.fold("")(year => s"\u0000performed:$year") +
      // only where its title bills an anniversary, so every other listing keeps its key
      anniversaryScreeningYear.fold("")(year => s"\u0000anniversaryScreened:$year")

  /** The evidence as the calibrated measures read a listing, its title shapes undecorated by the
   *  resolve's learned `decorations` (the same for every listing of a resolve, so not in [[key]]). */
  lazy val measured: MeasuredListing =
    MeasuredListing(title, Some(rawTitle).filter(_ != title), originalTitle, year, runtime, directors, countries,
      decorations = decorations, searchTitles = searchTitle.toSeq ++ properTitle, dating = dating, certificate = certificate)

  /** [[measured]] with the title shapes the venue's own delimiters leave, no learned decoration nor search title
   *  stripped: what relates two LISTINGS (their families, title must-links and listing-listing
   *  measures). A learned decoration names a FILM to search for and relate to; it never links a
   *  "Horror Season 2026 Dracula" to every other venue's bare "Dracula". */
  lazy val published: MeasuredListing =
    measured.copy(decorations = TitleDecorations.None, searchTitles = Nil, dating = dating.copy(broadcastSeason = None, screenedSeason = None))

  /** The year this listing states: its own field, else the one its title brackets. */
  def statedYear: Option[Int] = measured.statedYear

  /** The evidence with its stated facts set aside — its titles alone: what a node whose facts a feed copied from an
   *  overruled link is read as ([[FeedLink]]). */
  def titlesOnly: Evidence = copy(year = None, directors = Nil, runtime = None, originalTitle = None, countries = Nil, certificate = None,
    dating = dating.copy(yearUpperBound = None))
}

object Evidence {
  def of(listing: Listing, detail: Option[DetailFacts], decorations: TitleDecorations = TitleDecorations.None): Evidence = Evidence(
    title         = listing.title,
    cleanTitle    = listing.cleanTitle,
    rawTitle      = listing.rawTitle,
    year          = listing.year.orElse(detail.flatMap(_.year)),
    directors     = (if (listing.directors.nonEmpty) listing.directors
                     else detail.map(_.directors.flatMap(Listing.credited).filter(_.nonEmpty).distinct).getOrElse(Nil)).sorted,
    runtime       = listing.runtime.filter(models.FilmRuntime.plausible).orElse(detail.flatMap(_.runtime).filter(models.FilmRuntime.plausible)),
    originalTitle = listing.originalTitle.orElse(detail.flatMap(_.originalTitle)),
    countries     = (if (listing.countries.nonEmpty) listing.countries else detail.map(_.countries).getOrElse(Nil)).distinct.sorted,
    decorations   = decorations,
    searchTitle   = listing.searchTitle,
    properTitle   = listing.properTitle,
    certificate   = listing.certificate.map(_.trim).filter(_.nonEmpty),
    dating        = datingOf(listing, detail))

  /** What dates `listing`'s film beside its year ([[ListingDating]]), its `detail` page read in. */
  private def datingOf(listing: Listing, detail: Option[DetailFacts]): ListingDating = ListingDating(
    // a year the detail page states dates the relay as a year the venue's own field does
    broadcastSeason = listing.broadcastSeason.filter(_ => detail.flatMap(_.year).isEmpty),
    screenedSeason  = listing.screenedSeason.filter(_ => detail.flatMap(_.year).isEmpty),
    screeningsSeason = listing.screenings.season,
    // ...but for a season named too far back, whose page is as old as its title (UK Flicks' "Royal Ballet & Opera
    // 2024-25: Romeo and Juliet", its page dated 2025, screening on the 2026/27 relay's air day)
    airDays         = listing.airDays.filter(_ => detail.flatMap(_.year).isEmpty || listing.namesAStaleSeason.isDefined),
    yearUpperBound  = listing.yearUpperBound,
    performanceYear = listing.performanceYear,
    anniversaryScreeningYear = listing.anniversaryScreeningYear)
}
