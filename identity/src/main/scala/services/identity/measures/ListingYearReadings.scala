package services.identity.measures

import services.identity.measures.IdentityMeasures.*
import services.titles.YearWindow
import Measure.{Category, Number}

/** What a listing-film measurement set's YEAR measures read once the plain deltas are taken ([[IdentityMeasures.listingFilmTitled]]):
 *  each reading below a pure function of the listing, the film and the set so far, returning the set with its published
 *  year read again or left absent. [[read]] applies them in their one order. */
private[identity] object ListingYearReadings {
  /** `m` with every year reading applied, innermost first: a screening's year, an anniversary's, a premiere's, a year
   *  upper bound, then the anniversary's dating of a listing that states no year. */
  def read(l: MeasuredListing, f: Film, country: Option[String], corroboratingVenues: Int, title: Category, m: Map[String, Measure]): Map[String, Measure] =
    anniversaryFilmDated(l, f, yearBound(l, f, country, corroboratingVenues, premiereYear(l, f, anniversaryYear(l, f, screeningYearAbsent(l, f, title, m)))))

  /** `m` with the year an anniversary the title bills dates its film ([[MeasuredListing.anniversaryFilmYear]]) read as a published
   *  year against a film released within a year of it — and only there, where nothing else dated the listing: US "The Host:
   *  20th Anniversary", screening in 2026, names Bong Joon-ho's 2006 film and Niccol's 2013 one alike, and the anniversary
   *  is the one fact telling them apart. A film further off stays as a listing stating no year weighs it: a venue's
   *  anniversary may count from another release (DE "SATURDAY NIGHT FEVER 40TH ANNIVERSARY", screening in 2026, is the
   *  1977 film), so the hint is evidence for a film, never against one. A re-release tours into the year after its
   *  anniversary, so a film released a year before the date is read as of it: DE Filmpalette's "LA HAINE (30TH
   *  ANNIVERSARY)", screening in 2026, is Kassovitz's 1995 film, and a `year.delta` of 1 weighs against a film (of the
   *  matched anniversary billings stating no year in prod, 2026-11-15, 1199 took a film of the date, 45 one a year
   *  before it, 1 one a year after). */
  private def anniversaryFilmDated(l: MeasuredListing, f: Film, m: Map[String, Measure]): Map[String, Measure] =
    l.anniversaryFilmYear match {
      case Some(dated) if m("year.delta") == MissingListing && f.year.exists(FactRelations.yearsNear(dated, _)) =>
        val anniversary = if (f.year.contains(dated - 1)) dated - 1 else dated
        m ++ Map("year.delta" -> delta(Some(anniversary), f.year), "year.distance" -> absDelta(Some(anniversary), f.year))
      case _ => m
    }

  /** `m` with a listing's year UPPER BOUND ([[MeasuredListing.yearUpperBound]]: Cineamo's German premiere of an older film) read
   *  as its published year against a film released more than [[YearWindow.PublishedAdjacency]] after it — and only there.
   *  No film premieres before it is made, so the film released past the premiere weighs as the year that far before it
   *  weighs (`year.delta` -2: -1.1; -3 and less: -6.9), the learned cannot-links reading it as they read a stated year:
   *  Eulenspiegel's "King Kong", premiered in Germany in 1933, is no 2005 remake. A film from the bound's year or before
   *  stays as a listing stating no year weighs it: the premiere trails a classic by any number of years (Rope 1948,
   *  premiered there in 1963), so it is no match, only not ruled out. Only a film made the premiere's very year is read
   *  as of the stated year: its premiere is its own (DE "Der kleine Prinz", premiered 1972, is the 1972 film, not the
   *  1962 or the 1974 one), as Cineamo's year read it before it became a bound.
   *  The bound is the premiere of the record the feed LINKS, a copy of the operator's pick, not the venue's own fact: it
   *  states nothing against a film another venue's own facts back (`corroboratingVenues`: its exact year or its
   *  director). DE Kommunales Kino im Künstlerhaus' "Close", linked to Cineamo's 148857m (premiered 2004), is Lukas
   *  Dhont's 2022 film that Passage Kinos Leipzig dates and times from its own source. */
  private def yearBound(l: MeasuredListing, f: Film, country: Option[String], corroboratingVenues: Int, m: Map[String, Measure]): Map[String, Measure] =
    l.dating.yearUpperBound.filter(_ => l.year.isEmpty && corroboratingVenues == 0) match {
      case Some(bound) =>
        val released = if (f.releases.isEmpty) f.year else f.closestYear(bound, country)
        if (released.exists(_ - bound > YearWindow.PublishedAdjacency)) m ++ Map("year.delta" -> delta(Some(bound), released), "year.distance" -> absDelta(Some(bound), released))
        else if (f.year.contains(bound)) m ++ Map("year.delta" -> delta(Some(bound), f.year), "year.distance" -> absDelta(Some(bound), f.year))
        else m
      case None => m
    }

  /** `m` with the published year absent when it predates the season the listing's screenings broadcast its stage work
   *  in ([[MeasuredListing.screenedSeason]]), against the house's record of that season ([[StageRelays.screenedSeasonRecord]]): the year dates
   *  the staging's premiere, not the broadcast. UK Aldeburgh's "The Royal Ballet: Romeo & Juliet" [2025], screening on 25
   *  May 2027, is "Royal Ballet & Opera 2026/27: Romeo and Juliet", aired that day — two years off its 2027, its title an
   *  overlap, it was denied, and the denial withdrew the record from every relay title-linked to it. Against any other
   *  record — the 2021/22 season's, the house's 2013 recording — the year stays the venue's fact. */
  private def premiereYear(l: MeasuredListing, f: Film, m: Map[String, Measure]): Map[String, Measure] =
    if (l.year.isDefined && StageRelays.screenedSeasonRecord(l, f)) m ++ PublishedYear.map(_ -> MissingListing) else m

  /** `m` with the published year absent when it is the year of an ANNIVERSARY the title bills: ES "Drácula. 30
   *  Aniversario" (original title "Bram Stoker's Dracula 30th Anniversary"), dated 2022, is Coppola's 1992 film — its
   *  year that many years before the stated one, within a year. Any other year stays the listing's fact, and so does one
   *  a re-release TMDB dates already agrees with (DE "Der mit dem Wolf tanzt (35-jähriges Jubiläum)" [2026], released
   *  again in 2026). */
  private def anniversaryYear(l: MeasuredListing, f: Film, m: Map[String, Measure]): Map[String, Measure] =
    if (l.anniversary.isEmpty || l.year.isEmpty || f.year.isEmpty || !FactRelations.yearsNear(l.year.get - l.anniversary.get, f.year.get) ||
        m("year.delta").match { case Number(d) => FactRelations.nearDelta(d); case _ => false }) m
    else m ++ PublishedYear.map(_ -> MissingListing)
  /** Is `a` dated by the screening of an anniversary it bills, `b` by the film's own year that many years before: two
   *  spellings of one re-release, neither year the other's fact (DE "Der mit dem Wolf tanzt (35-jähriges Jubiläum)" dated
   *  2026 at one venue and 1991 at another). */
  private[identity] def anniversaryDated(a: MeasuredListing, b: MeasuredListing): Boolean =
    a.anniversary.isDefined && a.year.isDefined && b.year.exists(FactRelations.yearsNear(a.year.get - a.anniversary.get, _))
  /** `m` with the published year absent when it dates a screening ([[PublishedYear]]): when the
   *  listing's title is the film's title ([[TitledRelations]]) and the same director is credited, a year
   *  that denies the film is the year the venue shows it (Kinoteka's 2026 on Ken Russell's 1971
   *  "Diabły") or releases it, not another film's — in the score as in a veto. A title that is
   *  NOT the film's leaves the year a fact: then it tells that director's films apart
   *  (KINOMUZEUM's 2026 "Błotem w twarz" is Jaak Kilmi's 2026 film, not his 2017 "Sangarid"), and
   *  a title that only starts or ends with the film's (`decorated`) may bill another work beside it
   *  (Nowe Horyzonty's 2026 double bill "Basia. Humor w paski mam + Kocia Szajka…" is not
   *  Wasilewski's 2018 "Basia").
   *  "Credited" is by the listing that published the year ([[MeasuredListing.creditedBesideYear]]), never
   *  borrowed from a sibling's credit. A re-release is the same cut: a runtime that contradicts
   *  the film ([[runtimeContradicts]]) makes the year another production's — DE's 2027 "MET Opera
   *  Live im Kino: Manon" at 264 minutes is the Met's revival of Laurent Pelly's staging, not his
   *  232-minute 2019 recording. */
  private def screeningYearAbsent(l: MeasuredListing, f: Film, title: Category, m: Map[String, Measure]): Map[String, Measure] =
    if (TitledRelations(title.value) && ownAgreement(m)._2("year") && !runtimeContradicts(m) &&
        f.directorCredits.exists(CreditRelation.onePerson(l.creditsBesideYear, _)))
      m ++ PublishedYear.map(_ -> MissingListing)
    else m
}
