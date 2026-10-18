package services.identity

import play.api.libs.json.JsValue

/**
 * Age certificates on one scale per country: a venue's own certificate for a listing (its `ageRating`, from its feed or
 * peeled off its title at intake, `services.movies.TitleFacts`) against the one TMDB's `release_dates` record for the
 * film in the venue's country. A certificate is the venue's statement of WHICH cut of WHICH film it screens: UK Quad
 * Derby bills "Obsession (PG)" at 96 minutes, and Curry Barker's 2026 "Obsession", rated 18 in Britain, is no PG film.
 *
 * One rule for every country, the countries told apart only by their rating scales ([[Ladders]]): a certificate the
 * scale does not read (a US "R" at a UK venue, "TBC", "12A LIVE") says nothing.
 */
object Certificates {

  /** One country's rating scale, least restrictive first: each step's spellings as venues and TMDB write them, after
   *  [[normalised]] (upper case, a "BBFC"/"FSK"/"Cert" word and spaces off). */
  final case class Ladder(steps: Seq[Set[String]]) {
    private val stepOf: Map[String, Int] = steps.zipWithIndex.flatMap { case (spellings, step) => spellings.map(_ -> step) }.toMap
    /** The step `certificate` stands on, if the scale reads it. */
    def step(certificate: String): Option[Int] = stepOf.get(normalised(certificate))
  }

  /** The scales, by the ISO-3166-1 code of the country they rate in: the deployments' countries that have one (Poland
   *  rates no film). The ONLY per-country part of the rule. */
  val Ladders: Map[String, Ladder] = Map(
    "GB" -> Ladder(Seq(Set("U"), Set("PG"), Set("12A", "12"), Set("15"), Set("18"), Set("R18"))),
    "DE" -> Ladder(Seq(Set("0"), Set("6"), Set("12"), Set("16"), Set("18"))),
    "US" -> Ladder(Seq(Set("G"), Set("PG"), Set("PG-13"), Set("R"), Set("NC-17"))),
    "ES" -> Ladder(Seq(Set("A", "APTA", "TP", "APTA(I)"), Set("7", "+7", "7(I)"), Set("12", "+12"), Set("16", "+16"), Set("18", "+18"))))

  /** How many steps below the record's certificate a venue's must stand to deny the film: three. PG against 18 is no
   *  cut of the film (UK Quad Derby's "Obsession"); two steps would deny UK Queen Budapest's billings eleven times over
   *  (recording 38713274617), and a step or two apart is as often a re-rating, a cut for the matinee, or a venue's slip. */
  val StepsBelow = 3

  private val Words = """(?i)^(?:bbfc|fsk|cert(?:ificate)?\.?)\s*""".r
  /** `certificate` as the ladders spell it. */
  def normalised(certificate: String): String = Words.replaceFirstIn(certificate.trim, "").replace(" ", "").toUpperCase(java.util.Locale.ROOT)

  /** The certificates a `release_dates` block records, the least restrictive one per country a [[Ladders]] scale reads
   *  — "DE=16,GB=18,US=R" — or `None` when it records none there. Only those countries: every film's record holds it, so
   *  it keeps what a guard asks and no more (what `TmdbNormalizer.minimal` keeps of the block, as [[TmdbFilmRecord.releases]]). */
  def ofReleaseDates(block: JsValue): Option[String] = {
    val kept = (block \ "results").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap { result =>
      (result \ "iso_3166_1").asOpt[String].flatMap(country => Ladders.get(country).flatMap { ladder =>
        (result \ "release_dates").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(r => (r \ "certification").asOpt[String])
          .flatMap(c => ladder.step(c).map(_ -> normalised(c))).minByOption(_._1).map { case (_, c) => country -> c }
      })
    }.distinct.sortBy(_._1)
    Option.when(kept.nonEmpty)(kept.map { case (country, c) => s"$country=$c" }.mkString(","))
  }

  /** The certificate `certificates` ([[ofReleaseDates]]) holds for `country`. */
  def in(certificates: Option[String], country: String): Option[String] =
    certificates.toSeq.flatMap(_.split(',')).collectFirst { case entry if entry.startsWith(s"$country=") => entry.drop(country.length + 1) }

  /** Why `film` is no film `listing` screens: the venue's certificate stands [[StepsBelow]] steps or more below the one
   *  TMDB records for it in the venue's country — `None` when either is missing or the country's scale reads neither. */
  def below(listing: Listing, film: IdentityMeasures.Film): Option[String] =
    for {
      venue   <- listing.certificate
      country <- EvidenceNode.countryOf(listing.cinema)
      ladder  <- Ladders.get(country)
      own     <- ladder.step(venue)
      rated   <- in(film.certificates, country)
      step    <- ladder.step(rated)
      if step - own >= StepsBelow
    } yield s"the venue rates it ${venue.trim}, ${step - own} steps below ${film.titled}'s $country certificate $rated"
}
