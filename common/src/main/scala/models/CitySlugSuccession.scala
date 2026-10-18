package models

/** Each country's retired page slugs → the live page each now redirects to: the regions, metros
 *  and provinces pages were filed under until 2026-10, and the slugs pages answered at while
 *  another country held their name. Read by each [[Country]]'s `renamedSlugs`. */
private[models] object CitySlugSuccession {

  /** UK pages that no longer exist → the page holding the town each was named
   *  after, or — where no such town has a venue — the page now holding most of
   *  its venues. The 76 Flicks regions the UK was filed under until 2026-10 and
   *  the three folded into a neighbour before that among them: `/antrim/` → the
   *  Lisburn area (Antrim's cluster), `/west-yorkshire/` → Leeds. A region whose
   *  slug is a page now (`/manchester/`, `/dudley/`) simply IS that town's page. */
  lazy val ukSuccession: Map[String, String] = UkPages.retired

  /** A retired page's URLs → the page it redirects to: under its own name AND
   *  under its qualified name, which is the one it answered at while another
   *  country held its name (until 2026-10, when every country came to own its
   *  slugs) — both are published. Neither may be a live page's slug. */
  private def succession(retired: Seq[(String, String, String)], live: Set[String]): Map[String, String] =
    retired.flatMap { case (preferred, qualified, page) => Seq(preferred -> page, qualified -> page) }
      .filter { case (from, to) => from != to && !live(from) }
      .toMap

  /** German pages that no longer exist → the page holding the town each was
   *  named after, or — where that town has no venue left — the page now holding
   *  most of its venues. The 19 regions that did not leave a page under their own
   *  slug when Germany went from regions to towns (2026-10) among them: `/vechta/`
   *  → Cloppenburg (Vechta's cluster), `/itzehoe/` → Elmshorn. A region whose slug is a page now
   *  (`/koeln/`, `/freiburg/`) simply IS that town's page. */
  lazy val germanSuccession: Map[String, String] =
    succession(GermanRosterData.retired, City.germanPages.map(_.slug).toSet)

  /** US pages that no longer exist → the page holding the town each was named
   *  after, or — where no such town has a venue — the page now holding most of its
   *  venues. The 149 metros that did not leave a page under their own slug when
   *  the US went from metros to towns (2026-10) among them: `/philadelphia-new-jersey/`
   *  → Philadelphia, `/inland-empire/` → Chino. A metro whose slug is a page now
   *  (`/los-angeles/`, `/springfield/`) simply IS that town's page. Plus
   *  [[FormerlyQualified]]'s US pages. */
  lazy val usSuccession: Map[String, String] =
    succession(UsRosterData.retired, City.usCities.map(_.slug).toSet) ++ FormerlyQualified.us

  /** The pages that answered at a slug qualified with their state or community
   *  while another country held their name — published URLs, live until every
   *  country came to own its own slugs (2026-10) — and the bare slug each answers
   *  at now. Ten US pages yielded to the UK, Germany and Spain; Spain's Toledo and
   *  Laredo yielded to the US. (The UK's qualified slugs never shipped.) */
  object FormerlyQualified {
    val us: Map[String, String] = Map(
      "birmingham-alabama" -> "birmingham", "bristol-tennessee" -> "bristol", "glasgow-kentucky" -> "glasgow",
      "kent-washington" -> "kent", "london-kentucky" -> "london", "manchester-new-hampshire" -> "manchester",
      "potsdam-new-york" -> "potsdam", "somerset-kentucky" -> "somerset", "stuttgart-arkansas" -> "stuttgart",
      "suffolk-virginia" -> "suffolk",
    )
    val es: Map[String, String] = Map("toledo-castilla-la-mancha" -> "toledo", "laredo-cantabria" -> "laredo")

    def of(country: Country): Map[String, String] = country match {
      case Country.UnitedStates => us
      case Country.Spain        => es
      case _                    => Map.empty
    }
  }

  /** Spanish pages that no longer exist → the page now holding most of their
   *  venues. The 13 provinces that did not leave a town page under their own slug
   *  when Spain went from provinces to towns (2026-10) among them: `/asturias/` →
   *  Gijón, `/islas-baleares/` → Palma de Mallorca. A province whose slug is a
   *  town page now (`/madrid/`, `/barcelona/`) simply IS that town's page. Plus
   *  [[FormerlyQualified]]'s Spanish pages. */
  lazy val spanishSuccession: Map[String, String] =
    succession(SpanishRosterData.retired, City.spanishPages.map(_.slug).toSet) ++ FormerlyQualified.es
}
