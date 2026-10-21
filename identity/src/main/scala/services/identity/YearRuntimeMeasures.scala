package services.identity

import services.billing.DecorationWords
import java.util.Locale

import services.movies.{EmbeddedYear, TitleContainment, TitleSeason}

import IdentityMeasures.*

/** The facts a listing states against a film's: its countries, its year and runtime deltas, and the year its titles write.
 *  Part of [[IdentityMeasures]], which exports it. */
private[identity] object YearRuntimeMeasures {
  /** ISO 3166-1 alpha-2 code of a country as a venue spells it, read from the JDK's own
   *  country names in the deployment languages and its alpha-2/alpha-3 codes. No curated table:
   *  a spelling the JDK does not know is unmapped. */
  private lazy val isoByName: Map[String, String] = {
    val languages = Seq("pl", "en", "de", "es", "fr", "it").map(Locale.forLanguageTag)
    Locale.getISOCountries.iterator.flatMap { iso =>
      val l = Locale.of("", iso)
      // A country with no alpha-3 code throws; it simply has no such spelling.
      (languages.map(l.getDisplayCountry) ++ Seq(iso) ++ scala.util.Try(l.getISO3Country).toOption)
        .map(key).filter(_.nonEmpty).map(_ -> iso)
    }.toMap
  }

  def countryCode(name: String): Option[String] = isoByName.get(key(name))

  def countryRelation(listing: Seq[String], film: Option[Seq[String]]): Measure = {
    val own = listing.flatMap(countryCode).toSet
    if (listing.isEmpty) MissingListing
    else if (own.isEmpty) Missing("unmapped")
    else film.map(_.toSet) match {
      case None                => MissingFilm
      case Some(fc) if fc.isEmpty => MissingFilm
      case Some(fc)            => Category(if ((own intersect fc).nonEmpty) "match" else "mismatch")
    }
  }

  private[identity] def delta(a: Option[Int], b: Option[Int]): Measure = (a, b) match {
    case (None, _)          => MissingListing
    case (_, None)          => MissingFilm
    case (Some(x), Some(y)) => Number((x - y).toDouble)
  }

  /** The film's year minus a year the listing's title carries. */
  private[identity] def filmMinus(film: Option[Int], listing: Option[Int]): Measure = (listing, film) match {
    case (None, _)          => MissingListing
    case (_, None)          => MissingFilm
    case (Some(l), Some(f)) => Number((f - l).toDouble)
  }

  /** The listing's `runtime.delta` against the film, SIGNED: its stated runtime less the CLOSEST (by size of the gap) of
   *  the runtimes TMDB states for the film in any language it was fetched in ([[Film.runtimes]]) and of the cuts
   *  [[FilmCuts]] names — a listing running as any translation's cut runs as the film. Positive when the venue bills more
   *  minutes (an interval, an introduction, a short before the feature), negative when it bills fewer (another cut,
   *  another film); a gap the same size either way reads as the longer billing. Its table is fitted on each side of 0
   *  ([[EvidenceDirection.Peaked]]); the hand-written rules read its size ([[runtimeGap]]). A listing billing an EDITION ([[DecorationWords.billsAnEdition]]: "… (Extended Edition)",
   *  "Director's Cut", "Redux") that runs LONGER than every one of them states nothing for or against it: TMDB keeps no
   *  record of an extended cut apart from its film's ("The Return of the King" is 201 minutes in every language, its
   *  extended edition 263), so the longer cut is neutral. A shorter one still counts: an edition never makes a film shorter. */
  private[identity] def runtimeDelta(l: Listing, f: Film, country: Option[String]): Measure = l.statedRuntime match {
    case None => MissingListing
    case Some(stated) =>
      // walked in place, not as a collection: every listing is measured against every candidate
      var off = Int.MaxValue
      var longest = 0
      def against(runtime: Int): Unit = if (runtime > 0) {
        val d = stated - runtime
        if (math.abs(d) < math.abs(off) || (math.abs(d) == math.abs(off) && d > off)) off = d
        longest = math.max(longest, runtime)
      }
      f.runtime.foreach(against)
      if (f.alternativeRuntimes.nonEmpty) f.alternativeRuntimes.foreach(against)
      FilmCuts.of(f.imdbNumber).foreach(cut => against(cut.runtime))  // a billed cut runs as the film ([[FilmCuts]])
      if (longest == 0) MissingFilm
      else if (stated > longest && billsAnEdition(l, f, country)) MissingListing
      else Number(off.toDouble)
  }

  /** Does the listing bill an edition of the film: by its title ([[DecorationWords.billsAnEdition]]), or by its year, one
   *  TMDB dates a release of the film's edition in ([[Film.editionYear]]: Prince Charles's "Apocalypse Now", 2019, is
   *  its 2019 Final Cut)? */
  private def billsAnEdition(l: Listing, f: Film, country: Option[String]): Boolean =
    DecorationWords.billsAnEdition(l.ownTitles, f.titles) || l.year.exists(f.editionYear(_, country))

  private[identity] def absDelta(a: Option[Int], b: Option[Int]): Measure = delta(a, b) match {
    case Number(d) => Number(math.abs(d))
    case other     => other
  }

  /** A year titles put in as a delimited annotation ("(2026)"), outside any season. */
  def titleYearOf(titles: Seq[String]): Option[Int] = {
    val seasonless = titles.map(withoutSeasons)
    EmbeddedYear.ofAll(seasonless ++ seasonless.flatMap(labelledYears), Int.MaxValue)
  }

  /** A bracket dating the title beside a name — a studio's, a label's: UK Windmill Littlehampton's "DRACULA (Hammer 1958) 4K
   *  Restoration - Directors Cut" is Fisher's 1958 film — as the bracketed year [[titleYearOf]] reads ("(1958)"). A month,
   *  or a word an edition, an event or a print is billed by — a compound of an event's word too — dates the screening, not
   *  the film: DE "Der Physiopath (März 2027)", "Krimizeit (Krimifestival 2026)", "(Re-release 2025)". */
  private def labelledYears(title: String): Iterator[String] =
    if (!TitleSeason.hasAsciiDigit(title)) Iterator.empty
    else LabelledYear.findAllMatchIn(title).filterNot(m => TitleContainment.tokens(m.group(1)).exists(datesAScreening))
      .map(m => s"(${m.group(2)})")
  private[identity] def datesAScreening(word: String): Boolean =
    DecorationWords.EditionWords(word) || DecorationWords.EventWords(word) || services.movies.FormatTags.FormatToken.contains(word) ||
      MonthNames(word) || DecorationWords.EventWords.exists(event => event.length >= 5 && word.length > event.length && word.contains(event))
  /** Every locale's month names, as title words. */
  private lazy val MonthNames: Set[String] =
    (for { locale <- java.util.Locale.getAvailableLocales.iterator; month <- java.time.Month.values.iterator
           style <- Iterator(java.time.format.TextStyle.FULL, java.time.format.TextStyle.FULL_STANDALONE) }
      yield TitleContainment.tokens(month.getDisplayName(style, locale))).flatten.filter(_.length >= 3).toSet
  private val LabelledYear = """[(\[]\s*(\p{L}[\p{L}'’.&-]*(?:\s+\p{L}[\p{L}'’.&-]*)?)\s+((?:18|19|20)\d{2})\s*[)\]]""".r
}
