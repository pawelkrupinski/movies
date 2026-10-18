package services.identity

import models.{Cinema, CinemaMovie, Country, Movie, SourceData, UsCinema}
import services.cinemas.pl.NonMovieEventClassifier
import services.movies.{ScrapeListing, TitleNormalizer, TitleText}
import services.resolution.SearchTitles
import services.titlerules.{TitleRule, TitleRuleKey, TitleRules}
import tools.{CinemaSlotInvariants, SearchQueryMarkers}

import java.text.Normalizer
import java.util.Locale

/**
 * Every step a raw billing goes through, in production order, as text (`scripts/title-trace`): the ingest tidy, the
 * venue's own rules, the format peel ([[ScrapeListing.cleanTitle]]: the slot's title), the canonical tier (the slot
 * key), the search strip rule by rule ([[TitleNormalizer.searchQuery]]), the listing's shape as the identity model reads
 * it ([[ListingShape]], [[MultiFilmBill]], [[NonMovieEventClassifier]]), the searches it issues ([[CandidateQueries]],
 * [[SearchTitles]]) and the corpus-shape findings it would raise (`SearchQueryMarkersSpec`, `CinemaSlotInvariantsSpec`).
 *
 * Every step calls the production function itself; a rule's before and after are re-applied from the ids the rule set
 * reports as fired ([[services.titlerules.TitleRuleSet.firedSearch]] and its siblings), in their fold order, so the
 * trace cannot disagree with the fold. Lines carry no timing or path, so two refs' traces diff to what the code moved.
 */
object TitleTrace {

  final case class Options(raw: String = "", country: String = "", venue: Option[String] = None, batch: Option[String] = None)

  def parse(args: List[String], o: Options = Options()): Options = args match {
    case Nil                        => o
    case "--country" :: cc :: rest  => parse(rest, o.copy(country = cc))
    case "--venue" :: venue :: rest => parse(rest, o.copy(venue = Some(venue)))
    case "--batch" :: file :: rest  => parse(rest, o.copy(batch = Some(file)))
    case flag :: _ if flag.startsWith("--") => throw new IllegalArgumentException(s"unknown argument $flag")
    case raw :: rest                => parse(rest, o.copy(raw = raw))
  }

  def main(args: Array[String]): Unit = {
    val o = parse(args.toList)
    o.batch.fold(single(o))(batch)
  }

  private def single(o: Options): Unit = {
    val country = Country.byCode(o.country.toLowerCase(Locale.ROOT)).getOrElse {
      System.err.println(s"no country '${o.country}' (one of ${Country.all.map(_.code).mkString(", ")})"); sys.exit(2)
    }
    venueOf(o.venue) match {
      case Left(problem) => System.err.println(problem); sys.exit(2)
      case Right(cinema) => trace(o.raw, country, cinema).foreach(println)
    }
  }

  /** Every `country<TAB>venue<TAB>raw title` line of `file` traced in turn, each trace headed by its line — a whole
   *  corpus in one JVM, so two refs' outputs diff to every billing whose steps moved. A venue the roster names twice is
   *  traced with no venue rules (the roster's exact display names never are). */
  private def batch(file: String): Unit = {
    val source = scala.io.Source.fromFile(file, "UTF-8")
    try source.getLines().foreach { line =>
      line.split('\t') match {
        case Array(cc, venue, raw) =>
          val cinema = venueOf(Some(venue)).getOrElse(new UsCinema(venue, venue))
          println(s"### $line")
          Country.byCode(cc.toLowerCase(Locale.ROOT)) match {
            case Some(country) => trace(raw, country, cinema).foreach(println)
            case None          => println(s"no country '$cc'")
          }
        case _ => println(s"### $line\nskipped: not country<TAB>venue<TAB>raw title")
      }
    } finally source.close()
  }

  /** A synthetic venue: no rules of its own, as a venue the roster does not hold. */
  private final val AnyVenue = "(any venue)"

  /** The roster's venue named `name` — exactly, else the one whose name holds it (case and accents aside) — or, when the
   *  roster holds none, a venue of that name with no rules of its own. */
  def venueOf(name: Option[String]): Either[String, Cinema] = name match {
    case None => Right(new UsCinema(AnyVenue, AnyVenue))
    case Some(n) =>
      Cinema.byDisplayName.get(n).map(Right(_)).getOrElse {
        val wanted = folded(n)
        Cinema.all.filter(c => folded(c.displayName) == wanted) ++ Cinema.all.filter(c => folded(c.displayName).contains(wanted)) match {
          case Seq()                         => Right(new UsCinema(n, n))
          case exact +: _ if folded(exact.displayName) == wanted => Right(exact)
          case Seq(one)                      => Right(one)
          case several                       => Left(s"venue '$n' names ${several.size} venues: ${several.take(12).map(_.displayName).mkString(" / ")}")
        }
      }
  }

  private def folded(s: String): String =
    Normalizer.normalize(s.toLowerCase(Locale.ROOT), Normalizer.Form.NFD).replaceAll("\\p{M}", "").replace("ł", "l")

  private def q(s: String): String = s"'$s'"
  private def moved(before: String, after: String): String = if (before == after) s"${q(before)} (unchanged)" else s"${q(before)} → ${q(after)}"
  private def yesNo(b: Boolean): String = if (b) "YES" else "no"
  /** What a title's facts fill (`services.movies.TitleFacts.Facts`), each named — empty when it bills none. Read by name
   *  ([[OlderRefs]]): a ref from before TitleFacts (2026-10-18) has none, and the tracer must compile into it. */
  private def factsOf(f: Product): String = {
    val directors = OlderRefs.field(f, "directors").collect { case d: Seq[?] if d.nonEmpty => s"director ${d.mkString(", ")}" }
    val runtime   = OlderRefs.field(f, "runtimeMinutes").collect { case Some(m) => s"runtime $m min" }
    (directors ++ OlderRefs.optionalText(f, "originalTitle").map(t => s"original title ${q(t)}") ++ runtime ++
      OlderRefs.optionalText(f, "ageRating").map(r => s"age rating $r")).map(", " + _).mkString
  }
  extension (s: String) private def ifEmpty(other: String): String = if (s.isEmpty) other else s
  private val baselineIds: Set[String] = TitleRules.all.map(_.id).toSet

  /** `fired` (ids, in fold order) applied one by one from `from`: each rule's name, where it is defined, before and after. */
  private def steps(normalizer: TitleNormalizer, fired: Seq[String], from: String): (Seq[String], String) = {
    val byId = normalizer.rules.effectiveRules.groupBy(_.id)
    fired.foldLeft((Vector.empty[String], from)) { case ((lines, at), id) =>
      val rule: Option[TitleRule] = byId.get(id).flatMap(_.headOption)
      val next = rule.fold(at)(_.apply(at))
      val where = if (baselineIds(id)) "TitleRules" else "ExtraTitleRules"
      val tag = rule.flatMap(_.tag).fold("")(t => s", $t")
      (lines :+ s"    $id [$where$tag]: ${moved(at, next)}", next)
    }
  }

  def trace(raw: String, country: Country, cinema: Cinema): Seq[String] = {
    val normalizer = TitleNormalizer.forCountry(country)
    val out = Vector.newBuilder[String]
    val ruleKey = TitleRuleKey.of(cinema.displayName)
    val ownRules = normalizer.rules.effectiveRules.count(r => r.cinemaId.contains(ruleKey))
    val venueLabel =
      if (cinema.displayName == AnyVenue) "no venue (no venue rules)"
      else if (Cinema.byDisplayName.contains(cinema.displayName)) s"venue '${cinema.displayName}' (rules key $ruleKey: $ownRules rule(s) of its own)"
      else s"venue '${cinema.displayName}', not in the roster (no venue rules)"
    out += s"title-trace ${country.code.toUpperCase(Locale.ROOT)} · $venueLabel"
    out += s"raw ${q(raw)}"

    // 1. ingest: what the slot carries (ScrapeListing.cleanTitle = TitleNormalizer.listingTitle)
    out += "1. ingest — the slot's title (ScrapeListing.cleanTitle)"
    val tidied = TitleText.tidy(raw)
    out += s"  tidy (entities, sentence marks, filed article): ${moved(raw, tidied)}"
    val peeled: Option[Product] = OlderRefs.call(normalizer, "facts").flatMap(OlderRefs.call(_, "peel", tidied)).collect { case p: Product => p }
    val peeledTitle = peeled.flatMap(OlderRefs.field(_, "title")).collect { case t: String => t }.getOrElse(tidied)
    out += peeled.fold("  title facts: none (this ref's code has no TitleFacts)")(p =>
      s"  title facts (TitleFacts, ${country.code}'s rules): ${moved(tidied, peeledTitle)}" +
        OlderRefs.field(p, "facts").collect { case f: Product => factsOf(f) }.getOrElse(""))
    val venueFired = normalizer.rules.firedPerCinema(ruleKey, peeledTitle)
    out += (if (cinema.displayName == AnyVenue) "  venue rules: none (no venue)"
            else s"  venue rules ($ruleKey): ${if (venueFired.isEmpty) "none fired" else ""}")
    val (venueLines, afterVenue) = steps(normalizer, venueFired, peeledTitle)
    out ++= venueLines
    val (clean, tags) = ScrapeListing.cleanTitle(cinema, raw, normalizer)
    require(normalizer.cinemaClean(ruleKey, raw) == afterVenue, s"the venue rules' trace ${q(afterVenue)} is not the fold's ${q(normalizer.cinemaClean(ruleKey, raw))}")
    out += s"  format peel (FormatTags.extractFormatTags): ${moved(afterVenue, clean)}" +
      (if (tags.nonEmpty) s" tags ${tags.mkString("[", ", ", "]")}" else " no tags") +
      (if (filedArticleFronted(afterVenue, clean)) ", filed article fronted" else "")
    out += s"  = slot title ${q(clean)}"
    val listingFacts = OlderRefs.call(normalizer, "titleFacts", cinema, raw).collect { case f: Product => factsOf(f) }.getOrElse("")
    out += s"  = listing facts${listingFacts.stripPrefix(",").ifEmpty(" none")}"

    // 2. identity key
    out += "2. canonical tier — the slot key (TitleNormalizer.sanitize)"
    val canonicalFired = normalizer.rules.firedCanonical(clean)
    val (canonicalLines, _) = steps(normalizer, canonicalFired, clean.trim)
    if (canonicalLines.isEmpty) out += "    none fired" else out ++= canonicalLines
    out += s"  = slot key ${q(normalizer.sanitize(clean))}"

    // 3. the search strip
    out += "3. search strip — the lookup query (TitleNormalizer.searchQuery)"
    val recased = normalizer.recase(clean)
    out += s"  recase (banner-aware): ${moved(clean, recased)}"
    val searchFired = normalizer.rules.firedSearch(recased)
    val (searchLines, stripped) = steps(normalizer, searchFired, recased)
    if (searchLines.isEmpty) out += "    no strip fired" else out ++= searchLines
    val apiQuery = normalizer.apiQuery(recased)
    require(stripped.trim == apiQuery, s"the strip's trace ${q(stripped.trim)} is not the fold's ${q(apiQuery)}")
    val query = normalizer.searchQuery(clean)
    out += s"  format re-peel: ${moved(apiQuery, query)}"
    out += s"  = search query ${q(query)}"

    // 4. the listing's shape
    val cm = CinemaMovie(Movie(title = raw), cinema, None, None, None, Nil, Nil, Nil)
    val listing = Listing.of(cinema, cm, normalizer)
    val lower = raw.toLowerCase(Locale.ROOT)
    out += "4. listing shape — as the identity model reads it"
    out += s"  identity title ${q(listing.title)}, search title ${listing.searchTitle.fold("(the title)")(q)}"
    out += s"  NonMovieEventClassifier: mystery screening (blind / sneak preview / isBareSneak) ${yesNo(NonMovieEventClassifier.isMysteryScreening(raw))}" +
      s", non-screening ${yesNo(NonMovieEventClassifier.isNonScreening(raw))}, live event ${yesNo(NonMovieEventClassifier.isLiveEvent(raw))}" +
      s", screened broadcast ${yesNo(NonMovieEventClassifier.isScreenedBroadcast(lower))}"
    val titles = Seq(listing.rawTitle, listing.title).distinct
    out += s"  MultiFilmBill: marker ${MultiFilmBill.marker(titles).fold("none")(q)}, billed one ${MultiFilmBill.billedOne(titles).fold("none")(q)}"
    out += s"  ListingShape: billsSeveral ${yesNo(ListingShape.billsSeveral(listing))}, quotesSeveral ${yesNo(ListingShape.quotesSeveral(raw))}" +
      s", quotesTwoJoined ${yesNo(ListingShape.quotesTwoJoined(raw))}, stagesAWork ${yesNo(ListingShape.stagesAWork(listing))}" +
      s", billsAHouse ${yesNo(ListingShape.billsAHouse(listing))}, relays ${yesNo(ListingShape.relays(listing, lower))}" +
      s", billsASilentFilm ${yesNo(ListingShape.billsASilentFilm(listing))}, programme slot ${ListingShape.programmeSlotOf(raw).fold("none")(q)}"
    out += s"  pieces ${IdentityMeasures.pieces(raw).map(q).mkString("[", ", ", "]")}"
    val candidates = SearchTitles.candidates(listing.title, None, Seq(clean))
    val beside = candidates.flatMap(c => ListingShape.billedBeside(listing, Seq(IdentityMeasures.Film(c))).map(c -> _))
    out += s"  billedBeside a film titled as each search candidate: ${if (beside.isEmpty) "none" else ""}"
    beside.foreach { case (c, why) => out += s"    ${q(c)}: $why" }

    // 5. the searches
    out += "5. searches"
    val queries = CandidateQueries.of(Evidence.of(listing, None))
    def of(kind: String)(pick: PartialFunction[CandidateQuery, String]): Unit = {
      val texts = queries.collect(pick)
      out += s"  $kind ${if (texts.isEmpty) "none" else texts.map(q).mkString(", ")}"
    }
    of("identity title searches (IdentityMeasures.searchQueries):") { case CandidateQuery.Title(t) => t }
    of("identity IMDb title paths:") { case CandidateQuery.Imdb(t) => t }
    out += s"  SearchTitles.candidates(identity title, slot title): ${candidates.map(q).mkString(", ")}"

    // 6. findings
    out += "6. corpus-shape findings (before any allowlist)"
    val markers = SearchQueryMarkers.markersIn(country, query)
    out += "  SearchQueryMarkersSpec: " + (
      if (SearchQueryMarkers.billsSeveralFilms(raw)) "exempt (bills several films)"
      else if (markers.isEmpty) "none"
      else s"searches ${q(query)} (${markers.map(m => s"$m → ${SearchQueryMarkers.strippedBy(m)}").mkString(", ")})")
    val violations = CinemaSlotInvariants.violations(SourceData(title = Some(clean)))
    out += s"  CinemaSlotInvariantsSpec (title): ${if (violations.isEmpty) "none" else violations.mkString(", ")}"
    out.result()
  }

  private val FiledArticle = """(?i)^(the|a|an|der|die|das|ein|eine|le|la|les|el|los|las|il|lo)\s""".r
  private def filedArticleFronted(before: String, after: String): Boolean =
    FiledArticle.findPrefixMatchOf(after).exists(m => !before.toLowerCase(Locale.ROOT).startsWith(m.matched.toLowerCase(Locale.ROOT)))
}
