package models

import java.util.Locale

import tools.Slugify

/** A named group of [[City]]s in a picker — a US STATE over its towns
 *  ("California → Los Angeles", the way a visitor finds one), or a UK NATION
 *  over its counties ("Scotland → Fife"), a Polish voivodeship or a Spanish
 *  province over its towns. Ordinarily the group is not a place you can open:
 *  `/california/` is nothing, and its films live at `/los-angeles/`. It is how
 *  the list of them is arranged, and nothing else.
 *
 *  The exception is [[soleCity]] — see there.
 *
 *  A country whose picker is one flat list leaves `Country.cityGroups` empty
 *  rather than declaring one group per city.
 *
 *  @param absorbed for a city of this group, the names of former pages that now
 *                  redirect to it — the UK's retired regions ([[UkPages.formerNames]])
 *                  — kept findable by [[aliasesOf]]. */
final case class CityGroup(label: String, slug: String, cities: Seq[City] = Nil,
                          groups: Seq[CityGroup] = Nil, absorbed: Map[City, Seq[String]] = Map.empty) {

  /** The one place this group holds, pulled up a level — and `None` for a group
   *  that really arranges several.
   *
   *  A heading you open to find exactly one row underneath is a tap that buys
   *  the reader nothing, whatever the two are called. A UK county holding one
   *  page collapses onto it — Rutland shows as Oakham, Shetland as Lerwick — as
   *  the District of Columbia shows as Washington and Saarland as Saarbrücken.
   *
   *  The group's own name is NOT lost when it differs — the picker keeps it on
   *  the row as a search alias (`data-alias`), so "shetland" still finds
   *  Lerwick. Dropping the heading is meant to save a tap, not to make the
   *  county unfindable, and the search is where that name was doing its work.
   *
   *  Nil `groups` is part of the test: a node with one city AND a subgroup under
   *  it is still arranging two things.
   *
   *  [[slug]] is deliberately NOT consulted. An earlier version required the
   *  city's slug to equal the group's, which collapsed Cheshire and left
   *  Merseyside, Greater Manchester, Norfolk and Saarland as headings over a
   *  single row — the tap this exists to remove. */
  def soleCity: Option[City] = (cities, groups) match {
    case (Seq(only), Nil) => Some(only)
    case _                => None
  }

  /** The name the picker actually SHOWS for this node: the place's, where the
   *  group collapsed onto one, and the group's own otherwise.
   *
   *  What a list of these must be sorted by. Sorting on [[label]] would order
   *  Merseyside and Greater Manchester under M and G while displaying Liverpool
   *  and Manchester — an alphabetical list that visibly is not one, which is
   *  precisely the failure the ordering exists to prevent. */
  def displayLabel: String = soleCity.map(_.labels.nominative).getOrElse(label)

  /** The group's own name, where it differs from the single place it collapsed
   *  onto — what the picker keeps searchable after pulling that place up, and
   *  `None` when there is nothing extra to remember (Cheshire under Cheshire) or
   *  when the group did not collapse at all. */
  def collapsedAlias: Option[String] =
    soleCity.filter(_.labels.nominative != label).map(_ => label)

  /** Every other name the picker should find one of this group's own cities by:
   *  the group's name where the group collapsed onto it ([[collapsedAlias]]),
   *  then the names of the former pages that redirect to it ([[absorbed]]) — so
   *  "Sandwell" still finds West Bromwich and "Highlands and Islands" Inverness. */
  def aliasesOf(city: City): Seq[String] =
    soleCity.filter(_ == city).flatMap(_ => collapsedAlias).toSeq ++ absorbed.getOrElse(city, Nil)

  /** [[aliasesOf]] the one place this group collapsed onto — empty for a group
   *  that did not collapse: its heading is already on screen. */
  def searchAliases: Seq[String] = soleCity.fold(Seq.empty[String])(aliasesOf)

  /** Every city under this group at any depth — its own, plus its subgroups'.
   *
   *  A picker nests (a UK nation over its counties over their places), but the
   *  things that read a grouping FLAT do not: `<optgroup>` may not contain
   *  another, and the apps' `region` is one string per city. Both take the top
   *  level's label over this, so a deeper tree arranges the page without
   *  changing what they get. */
  def allCities: Seq[City] = cities ++ groups.flatMap(_.allCities)
}

/** Each country's city picker: how its pages are grouped ([[CityGroup]]) — a Polish voivodeship,
 *  a UK nation over its counties, a Bundesland, a US state, a Spanish province over the pages
 *  inside it. Read by each [[Country]]'s `cityGroups`. */
private[models] object CityPickerGroups {

  /** Poland's picker: one group per voivodeship, holding its major cities and its
   *  town and cluster pages side by side, alphabetically — a flat list of 180
   *  pages is not one anybody reads. */
  lazy val polishVoivodeships: Seq[CityGroup] =
    CityListing.groupedAlphabetically(City.polishCities, Locale.forLanguageTag("pl-PL")) {
      case page: PolishPage => page.voivodeship
      case major            => PolishPages.majorVoivodeship(major.slug)
    }

  /** The UK picker's grouping: NATION over COUNTY over the pages inside it — the
   *  nation and county of each page's anchor town, read off GeoNames by
   *  `data/uk/scripts/geocode_towns.py` (England's ceremonial counties, Wales'
   *  preserved ones, Scotland's and Northern Ireland's councils, grouped).
   *
   *  264 pages is past what anyone reads as one A-to-Z, and a nation alone still
   *  is (England holds 200); the county is the level a visitor already knows a
   *  town by. A county holding one page collapses onto it ([[CityGroup.soleCity]]):
   *  Shetland shows as Lerwick, its own name kept as a search alias. A retired
   *  region's name is a search alias of the page its slug redirects to
   *  ([[UkPages.formerNames]]): "Tyrone" finds Dungannon.
   *
   *  Nations are in [[UkPages.nations]]' fixed reading order (England, Scotland,
   *  Northern Ireland, Wales, Crown Dependencies) — the same order every client's
   *  picker shows, kept in one place rather than re-derived per platform — and
   *  everything below is alphabetical, which is the only order a reader can
   *  search by eye. Every page appears exactly once (`CountrySpec` holds that). */
  lazy val ukNations: Seq[CityGroup] = {
    val en = Locale.forLanguageTag("en-GB")
    val placed: Seq[(String, String, City)] =
      (UkPages.london.nation, UkPages.london.county, London) +: City.ukPages.map(p => (p.nation, p.county, p))
    UkPages.nations.flatMap { nation =>
      val counties = placed.filter(_._1 == nation).groupBy(_._2).toSeq.map { case (county, pages) =>
        val cities = pages.map(_._3)
        CityGroup(county, Slugify.stable(county), CityListing.sorted(cities, en),
                  absorbed = cities.flatMap(c => UkPages.formerNames.get(c.slug).map(c -> _)).toMap)
      }
      // By what each row SHOWS, not by the county it came from: a county that
      // collapsed onto its one place displays that place's name, and sorting on
      // the county would file Shetland's Lerwick under S.
      Option.when(counties.nonEmpty)(
        CityGroup(nation, Slugify.stable(nation), Nil, CityListing.sortedLabels(counties, en)(_.displayLabel)))
    }
  }

  /** The German picker's grouping: one entry per BUNDESLAND over the pages inside
   *  it — major cities, towns and clusters side by side — alphabetically both
   *  ways, as Poland's voivodeships and Spain's provinces. The Land is the
   *  anchor town's, read off its coordinates by `build_pages.py`. */
  lazy val germanStates: Seq[CityGroup] =
    CityListing.groupedAlphabetically(City.germanPages, Locale.forLanguageTag("de-DE"))(_.region)


  /** The US picker's grouping: one entry per state or territory over the pages
   *  filed under it — major cities, towns and clusters side by side — the state
   *  being the anchor town's, so a cluster reaching across a state line is under
   *  the state of the town it is named after.
   *
   *  The pages inside a state are ALPHABETICAL: a visitor opening "California" is
   *  looking for a name they already know, and the only order that tells them
   *  where to look for it is the one the alphabet gives.
   *
   *  The states are in their SLUG's plain string order rather than re-sorted
   *  through [[CityListing.sortedLabels]] the way Germany's Bundesländer are: a
   *  Locale [[Collator]] weighs "Virgin Islands"' space against "Virginia"'s
   *  continuing letter differently than the slug does, which would silently swap
   *  the two. */
  lazy val usStates: Seq[CityGroup] = {
    val en = Locale.forLanguageTag("en-US")
    City.usCities.groupBy(_.region).toSeq
      .map { case (state, pages) => CityGroup(state, Slugify.stable(state), CityListing.sorted(pages, en)) }
      .sortBy(_.slug)
  }

  /** Spain's picker: one group per province, its major cities, towns and
   *  clusters side by side, alphabetically — as Poland's voivodeships. */
  lazy val spanishProvinces: Seq[CityGroup] =
    CityListing.groupedAlphabetically(City.spanishPages, Locale.forLanguageTag("es-ES"))(_.region)
}
