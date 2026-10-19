package scripts

import models.{Cinema, CinemaMovie, Country, SourceData, VenueClock}
import services.cinemas.CinemaScraperCatalog
import services.cinemas.common.{CinemaScraper, DetailEnricher}
import services.movies.{CinemaSlotBuilder, LatestTitleYear, ScrapeListing, ScreeningTokens, StringPool, TitleNormalizer}
import services.scrapes.ArchivedScrape
import tools.{CinemaSlotInvariants, CorpusFixture, FallbackHttpFetch, HttpFetch, RecordedCorpus, SearchQueryMarkers}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import java.time.{Clock, LocalDate, ZoneOffset}
import scala.util.{Failure, Success, Try}

/**
 * ONE venue's listings, as the code at this checkout reads them, from a recording — never the network
 * (`scripts/venue-replay`, which runs it once per ref; `.claude/skills/identity-investigation/SKILL.md`).
 *
 * The listings come from one of two places:
 *  - the recording's own row for the venue (`cinema-scrapes-<cc>.json.gz`: what PROD's client parsed at the
 *    recording), each built into its slot and cleaned by this code, and every detail page the code reads for it
 *    (a [[DetailEnricher]]'s `fetchFilmDetail`, or a client's own `parseDetail(html, url)`) read again from the
 *    recorded trees — so a title-rule, detail-parser or slot change shows at each ref;
 *  - with `--scrape`, the venue's scraper itself (`fetch()`), over the recorded trees, the venue clock pinned to
 *    `--date` — so a listing-parser change shows. A recording holds no listing pages (it is pulled from prod's
 *    `cinema_scrapes`), so this needs a tree that holds them: `--fixtures` (a client's checked-in fixture tree).
 *
 * The recorded trees are served in order: the recording's `enrichment-<cc>` tree and the verdicts remembered beside
 * it ([[RecordedCorpus]]), then each `--fixtures` tree, then a leaf that NAMES every request none of them holds: a
 * replay that met one prints them and exits 3 — whatever the client did with the failure.
 *
 * Each listing is held to the corpus-shape rules (`CinemaSlotInvariantsSpec`, `SearchQueryMarkersSpec`), each
 * finding named, and a title-trace line for each title a rule moved. `--out` writes the same as a TSV the two-ref diff reads.
 */
object VenueReplay {

  final case class Args(country: Country, venue: String, corpus: Option[Path], recording: Option[Path], fixtures: Seq[Path],
                        titles: Seq[String], scrape: Boolean, date: Option[LocalDate], prod: Option[Path], fields: Boolean,
                        out: Option[Path], ref: String)

  def parse(argv: Seq[String]): Args = {
    def go(rest: List[String], a: Args): Args = rest match {
      case "--venue" :: v :: t      => go(t, a.copy(venue = v))
      case "--corpus" :: v :: t     => go(t, a.copy(corpus = Some(Paths.get(v))))
      case "--recording" :: v :: t  => go(t, a.copy(recording = Some(Paths.get(v))))
      case "--fixtures" :: v :: t   => go(t, a.copy(fixtures = a.fixtures :+ Paths.get(v)))
      case "--title" :: v :: t      => go(t, a.copy(titles = a.titles :+ v))
      case "--scrape" :: t          => go(t, a.copy(scrape = true))
      case "--date" :: v :: t       => go(t, a.copy(date = Some(LocalDate.parse(v))))
      case "--prod" :: v :: t       => go(t, a.copy(prod = Some(Paths.get(v))))
      case "--fields" :: t          => go(t, a.copy(fields = true))
      case "--out" :: v :: t        => go(t, a.copy(out = Some(Paths.get(v))))
      case "--ref" :: v :: t        => go(t, a.copy(ref = v))
      case other :: _               => sys.error(s"unknown argument $other")
      case Nil                      => a
    }
    argv.toList match {
      case cc :: rest =>
        val country = Country.byCode(cc.toLowerCase(java.util.Locale.ROOT)).getOrElse(sys.error(s"no country $cc"))
        go(rest, Args(country, "", None, None, Nil, Nil, scrape = false, None, None, fields = false, None, "worktree"))
      case Nil => sys.error("usage: VenueReplay <cc> --venue S [--corpus GZ] [--recording DIR] [--fixtures DIR]... [--title S]... [--scrape] [--date D] [--prod JSON] [--fields] [--out TSV]")
    }
  }

  /** Case and accents aside, single-spaced: how a selector matches a venue or a title. */
  def folded(s: String): String =
    java.text.Normalizer.normalize(s, java.text.Normalizer.Form.NFD).replaceAll("\\p{M}", "").replace('ł', 'l').replace('Ł', 'L')
      .toLowerCase(java.util.Locale.ROOT).replaceAll("\\s+", " ").trim

  /** The one venue `selector` names among `names`: an exact (folded) name wins, else the only one containing it. */
  def pickVenue(selector: String, names: Seq[String]): Either[String, String] = {
    val s = folded(selector)
    names.distinct.find(folded(_) == s).map(Right(_)).getOrElse {
      names.distinct.filter(folded(_).contains(s)).sorted match {
        case Seq(one) => Right(one)
        case Seq()    => Left(s"no venue matches '$selector'")
        case many     => Left(s"'$selector' matches ${many.size} venues — name one: ${many.take(12).mkString(", ")}")
      }
    }
  }

  // ── The fetch ────────────────────────────────────────────────────────────────

  /** The end of the chain: a request no recorded tree holds is NAMED here and fails — never a stand-in answer. */
  final class NamingLeaf extends HttpFetch {
    private val lacked = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    def misses: Seq[String] = { import scala.jdk.CollectionConverters.*; lacked.asScala.toSeq.distinct }
    private def miss(method: String, url: String): Nothing = {
      lacked.add(s"$method $url")
      throw new java.io.FileNotFoundException(s"the recording lacks $method $url")
    }
    override def get(url: String): String = miss("GET", url)
    override def get(url: String, headers: Map[String, String]): String = miss("GET", url)
    override def getBytes(url: String): Array[Byte] = miss("GET", url)
    override def post(url: String, body: String, contentType: String): String = miss("POST", url)
    override def getAsync(url: String): java.util.concurrent.CompletableFuture[String] =
      java.util.concurrent.CompletableFuture.failedFuture(Try(get(url)).failed.get)
  }

  /** The recording's rows (`corpus`) and the fetch over its trees in order, then each `fixtures` tree, then `leaf`. */
  def opened(country: Country, corpus: Option[Path], recording: Option[Path], fixtures: Seq[Path],
             leaf: NamingLeaf): (Seq[ArchivedScrape], HttpFetch) = {
    val trees = fixtures.map { dir =>
      val abs = dir.toAbsolutePath.normalize
      s"fixtures:$abs" -> (new clients.tools.FakeHttpFetch(abs.getFileName.toString, strict = true, foldYear = true,
        root = settings.FixtureRoot(abs.getParent)): HttpFetch)
    }
    val behind: HttpFetch = new FallbackHttpFetch(trees :+ ("lacks" -> leaf))
    (corpus, recording.filter(r => Files.isDirectory(r.resolve(s"enrichment-${country.code}")))) match {
      case (Some(rows), Some(root)) =>
        val recorded = RecordedCorpus.open(country, rows, settings.FixtureRoot(root), _ => behind)
        recorded.rows -> recorded.fetch
      case (None, Some(root)) => sys.error(s"$root's trees need its corpus (--corpus)")
      case (rows, None)       => rows.map(CorpusFixture.readFrom).getOrElse(Nil) -> behind
    }
  }

  // ── Reading the venue ────────────────────────────────────────────────────────

  /** A listing as this code reads it: the film the client handed over, the slot the pipeline builds of it, its clean
   *  title, and where its detail fields came from. */
  final case class Read(cm: CinemaMovie, raw: String, clean: String, formats: Seq[String], slot: SourceData, detail: String,
                        recorded: SourceData) {
    /** The page fields this read states otherwise than the recorded listing did, each with its recorded value. */
    def moved: Seq[String] = Seq(
      "synopsis" -> ((s: SourceData) => head(s.synopsis, 70)), "director" -> ((s: SourceData) => list(s.director)),
      "runtime" -> ((s: SourceData) => s.runtimeMinutes.fold("-")(_.toString)), "year" -> ((s: SourceData) => s.releaseYear.fold("-")(_.toString)),
      "countries" -> ((s: SourceData) => list(s.countries)), "original" -> ((s: SourceData) => s.originalTitle.getOrElse("-"))
    ).collect { case (name, show) if show(slot) != show(recorded) => s"$name was ${show(recorded)}" }
  }

  def catalog(http: HttpFetch, day: LocalDate, country: Country): CinemaScraperCatalog = {
    val clock = Clock.fixed(day.atTime(10, 0).toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
    new CinemaScraperCatalog(http, VenueClock.fixedOn(day), clock, TitleNormalizer.forCountry(country))
  }

  /** A client's own `parseDetail(html, url): Option[CinemaMovie]` — the detail read of a client that reads its pages
   *  inside `fetch()` rather than as a [[DetailEnricher]] (Teatr Ziemi Rybnickiej). */
  private def ownDetailParse(scraper: CinemaScraper): Option[(String, String) => Option[CinemaMovie]] =
    Try(scraper.getClass.getMethod("parseDetail", classOf[String], classOf[String])).toOption.map { m =>
      (html: String, url: String) => m.invoke(scraper, html, url) match {
        case o: Option[?] => o.collect { case cm: CinemaMovie => cm }
        case _            => None
      }
    }

  /** The detail page of `cm` read again by `scraper`'s code, merged over its slot as a re-read lands (the page wins
   *  what it states): the new slot and what read it. */
  def reread(scraper: Option[CinemaScraper], http: HttpFetch, country: Country, cm: CinemaMovie, slot: SourceData): (SourceData, String) =
    (scraper, cm.filmUrl.filter(_.trim.nonEmpty)) match {
      case (Some(enricher: DetailEnricher), Some(url)) =>
        Try(enricher.fetchFilmDetail(url)) match {
          case Success(Some(d)) => d.landed(country.language).refreshInto(slot, ScreeningTokens.of(country)) -> "detail page re-read"
          case Success(None)    => slot -> "detail page read, nothing parsed"
          case Failure(e)       => slot -> s"detail page failed: ${e.getMessage.linesIterator.nextOption().getOrElse("")}"
        }
      case (Some(own), Some(url)) if ownDetailParse(own).isDefined =>
        Try(http.get(url)).map(html => ownDetailParse(own).get(html, url)) match {
          case Success(Some(page)) =>
            // the page's own read wins every field it owns, a synopsis it no longer finds too: a credit line read as one
            // is what such a fix cures
            slot.copy(synopsis = page.synopsis, director = page.director, cast = page.cast, runtimeMinutes = page.movie.runtimeMinutes,
              releaseYear = page.movie.releaseYear.orElse(slot.releaseYear), countries = page.movie.countries,
              genres = page.movie.genres) -> "detail page re-read (client parseDetail)"
          case Success(None) => slot -> "detail page read, nothing parsed"
          case Failure(e)    => slot -> s"detail page failed: ${e.getMessage.linesIterator.nextOption().getOrElse("")}"
        }
      case _ => slot -> "as recorded (no detail read)"
    }

  def read(country: Country, cinema: Cinema, films: Seq[CinemaMovie], scraper: Option[CinemaScraper], http: HttpFetch,
           rereadPages: Boolean): Seq[Read] = {
    val normalizer = TitleNormalizer.forCountry(country)
    val slots      = new CinemaSlotBuilder(country.language, new StringPool)
    films.map { cm =>
      val raw            = cm.movie.rawTitle.getOrElse(cm.movie.title)
      val (clean, peeled) = ScrapeListing.cleanTitle(cinema, cm.movie.title, normalizer)
      val built          = slots.build(cm, clean, None)
      val (slot, detail) = if (rereadPages) reread(scraper, http, country, cm, built) else built -> "as scraped"
      Read(cm, raw, clean, (peeled ++ cm.showtimes.flatMap(_.format)).distinct, slot, detail, built)
    }
  }

  // ── The checks ───────────────────────────────────────────────────────────────

  /** Every finding the corpus-shape specs raise on `r` — their own checks ([[CinemaSlotInvariants]],
   *  [[SearchQueryMarkers]]), as `CinemaSlotInvariantsSpec` / `SearchQueryMarkersSpec` apply them to every corpus. Their
   *  allowlists live with the specs (e2e): a finding here may be one they excuse. */
  def checksOf(country: Country, r: Read, query: String): Seq[String] =
    CinemaSlotInvariants.violations(r.slot).map("CinemaSlotInvariants " + _) ++
      SearchQueryMarkers.finding(country, r.raw, query).map("SearchQueryMarkers " + _)

  // ── Printing ─────────────────────────────────────────────────────────────────

  private def head(s: Option[String], n: Int): String =
    s.map(_.replaceAll("\\s+", " ").trim).filter(_.nonEmpty).map(t => if (t.length <= n) t else t.take(n - 1) + "…").getOrElse("-")
  private def list(xs: Seq[String]): String = if (xs.isEmpty) "-" else xs.mkString(", ")

  /** One listing's fields, named — what the text prints and the TSV carries. */
  def fieldsOf(r: Read, query: String, checks: Seq[String]): Seq[(String, String)] = Seq(
    "raw"       -> r.raw,
    "clean"     -> r.clean,
    "search"    -> query,
    "formats"   -> list(r.formats),
    "year"      -> r.slot.releaseYear.fold("-")(_.toString),
    "runtime"   -> r.slot.runtimeMinutes.fold("-")(_.toString),
    "director"  -> list(r.slot.director),
    "original"  -> r.slot.originalTitle.getOrElse("-"),
    "countries" -> list(r.slot.countries),
    "synopsis"  -> head(r.slot.synopsis, 400),
    "ids"       -> (if (r.cm.externalIds.isEmpty) "-" else r.cm.externalIds.toSeq.sorted.map((k, v) => s"$k:$v").mkString(" ")),
    "url"       -> r.cm.filmUrl.getOrElse("-"),
    "showtimes" -> r.cm.showtimes.size.toString,
    "detail"    -> r.detail,
    "recorded"  -> (if (r.moved.isEmpty) "-" else r.moved.mkString(" | ")),
    "checks"    -> (if (checks.isEmpty) "-" else checks.mkString(" | ")))

  /** The `scripts/title-trace` line that shows every step `raw` takes to its clean title and searches at `venue`. */
  def traceCommand(country: Country, venue: String, raw: String): String = {
    def quoted(s: String) = "'" + s.replace("'", "'\\''") + "'"
    s"scripts/title-trace ${quoted(raw)} --country ${country.code} --venue ${quoted(venue)}"
  }

  def render(title: String, reads: Seq[Read], country: Country, fields: Boolean): (String, Seq[Seq[(String, String)]]) = {
    val normalizer = TitleNormalizer.forCountry(country)
    val rows = reads.sortBy(r => (folded(r.raw), r.cm.filmUrl.getOrElse(""))).map { r =>
      val query = normalizer.searchQuery(r.clean)
      fieldsOf(r, query, checksOf(country, r, query)) :+ ("trace" -> traceCommand(country, r.cm.cinema.displayName, r.raw))
    }
    val sb = new StringBuilder(s"== $title: ${reads.size} listing(s)\n")
    rows.foreach { row =>
      val f = row.toMap
      if (fields) {
        sb ++= s"- ${f("raw")}\n"
        row.tail.foreach((k, v) => sb ++= f"    $k%-10s $v%s\n")
      } else {
        sb ++= s"- ${f("raw")}${if (f("clean") != f("raw")) s"  → ${f("clean")}" else ""}   [search: ${f("search")}]\n"
        sb ++= s"    formats ${f("formats")} · year ${f("year")} · runtime ${f("runtime")} · director ${f("director")} · " +
          s"ids ${f("ids")} · showtimes ${f("showtimes")} · ${f("detail")}\n"
        sb ++= s"    url ${f("url")}\n"
        sb ++= s"    synopsis ${head(Some(f("synopsis")), 110)}\n"
        if (f("recorded") != "-") f("recorded").split(" \\| ").foreach(m => sb ++= s"    RE-READ $m\n")
        if (f("checks") != "-") f("checks").split(" \\| ").foreach(c => sb ++= s"    CHECK $c\n")
        // the title's own steps, rule by rule, wherever a rule moved it or a check names it
        if (f("checks") != "-" || f("clean") != f("raw")) sb ++= s"    trace ${f("trace")}\n"
      }
    }
    (sb.toString, rows)
  }

  def tsv(rows: Seq[Seq[(String, String)]]): String = {
    def cell(s: String) = s.replace('\t', ' ').replace('\n', ' ')
    (rows.headOption.map(_.map(_._1)).getOrElse(Seq("raw")).mkString("\t") +: rows.map(_.map(c => cell(c._2)).mkString("\t"))).mkString("", "\n", "\n")
  }

  // ── The run ──────────────────────────────────────────────────────────────────

  final case class Outcome(text: String, rows: Seq[Seq[(String, String)]], misses: Seq[String])

  def run(a: Args): Outcome = {
    val leaf         = new NamingLeaf
    val (rows, http) = opened(a.country, a.corpus, a.recording, a.fixtures, leaf)
    val row0   = rows.find(r => pickVenue(a.venue, rows.map(_.cinema.displayName)).toOption.contains(r.cinema.displayName))
    val day    = a.date.orElse(row0.flatMap(_.contentAt).map(i => LocalDate.ofInstant(i, ZoneOffset.UTC)))
      .getOrElse(sys.error(pickVenue(a.venue, rows.map(_.cinema.displayName)).fold(e => s"the recording: $e", _ => "") +
        " — no recorded row names the scrape's day: give --date to scrape a venue the recording does not hold"))
    given LatestTitleYear = LatestTitleYear(Clock.fixed(day.atStartOfDay().toInstant(ZoneOffset.UTC), ZoneOffset.UTC))
    val scrapers = catalog(http, day, a.country).all
    val venue  = pickVenue(a.venue, rows.map(_.cinema.displayName) ++ scrapers.map(_.cinema.displayName)).fold(e => sys.error(e), identity)
    val scraper = scrapers.find(_.cinema.displayName == venue)
    val row    = rows.find(_.cinema.displayName == venue)
    val cinema = scraper.map(_.cinema).orElse(row.map(_.cinema)).get
    def selected(films: Seq[CinemaMovie]) =
      if (a.titles.isEmpty) films else films.filter(cm => a.titles.exists(t => folded(cm.movie.rawTitle.getOrElse(cm.movie.title)).contains(folded(t))))
    val client = scraper.fold("no scraper in this code's catalog")(s => s.getClass.getSimpleName)
    var scrapeFailure: Option[Throwable] = None
    val (title, films, reread) =
      if (a.scrape) {
        val s = scraper.getOrElse(sys.error(s"$venue has no scraper in this code's catalog"))
        val scraped = Try(s.fetch()).fold(e => { scrapeFailure = Some(e); Nil }, identity)
        (s"$venue — scraped again by ${s.getClass.getSimpleName} on $day, over the recorded trees", selected(scraped), false)
      } else {
        val r = row.getOrElse(sys.error(s"the recording holds no row for $venue"))
        (s"$venue — the recording's row (prod's scrape of ${r.contentAt.getOrElse("?")}), read by this code ($client)", selected(r.films), true)
      }
    val reads  = read(a.country, cinema, films, scraper, http, reread)
    val (text, out) = render(title, reads, a.country, a.fields)
    val failed   = scrapeFailure.fold("")(e =>
      s"    SCRAPE FAILED: ${e.getClass.getSimpleName}: ${Option(e.getMessage).getOrElse("").linesIterator.nextOption().getOrElse("")}\n")
    val prodText = a.prod.fold("")(p => prodSection(Files.readString(p, UTF_8), venue, a, cinema, selected))
    Outcome(s"[venue-replay] ${a.ref}\n" + text + failed + prodText, out, leaf.misses)
  }

  /** The venue's CURRENT prod state, from the local mirror's export (`scripts/venue-replay-prod.js`): its cinema_scrapes
   *  row read by this code and held to the same rules, each listing's venue_pages read, and whether identity_listings
   *  carries the same listings. Nothing is fetched for it. */
  def prodSection(json: String, venue: String, a: Args, cinema: Cinema, selected: Seq[CinemaMovie] => Seq[CinemaMovie])
                 (using LatestTitleYear): String = {
    import play.api.libs.json.{JsObject, Json}
    val parsed = Json.parse(json)
    def rows(coll: String) = CorpusFixture.parse((parsed \ coll).toOption.fold("[]")(_.toString)).filter(_.cinema.displayName == venue)
    val pages = (parsed \ "venue_pages").asOpt[Seq[JsObject]].getOrElse(Nil).map { p =>
      (p \ "page").as[String] -> (s"read ${(p \ "readAt").asOpt[String].getOrElse("?")}: " + head((p \ "synopsis").asOpt[String], 90) +
        (p \ "director").asOpt[Seq[String]].filter(_.nonEmpty).fold("")(d => s" · director ${d.mkString(", ")}"))
    }.toMap
    rows("cinema_scrapes").headOption.fold(s"\n== PROD now — the mirror holds no cinema_scrapes row for $venue\n") { p =>
      val pr = read(a.country, cinema, selected(p.films), None, new NamingLeaf, rereadPages = false)
      val (text, _) = render(s"PROD now — $venue, the mirror's cinema_scrapes (scraped ${p.contentAt.getOrElse("?")})", pr,
        a.country, a.fields)
      val pageLines = pr.flatMap(r => r.cm.filmUrl.flatMap(pages.get).map(l => s"  venue_pages ${r.raw}: $l\n")).mkString
      val scraped   = selected(p.films).map(_.movie.title).toSet
      val identity  = rows("identity_listings").headOption.map(l => selected(l.films).map(_.movie.title).toSet).fold("  identity_listings: no row\n") { l =>
        if (l == scraped) s"  identity_listings: the same ${l.size} listing(s)\n"
        else s"  identity_listings differ — only there: ${(l -- scraped).toSeq.sorted.mkString("; ")}; only in cinema_scrapes: " +
          s"${(scraped -- l).toSeq.sorted.mkString("; ")}\n"
      }
      "\n" + text + pageLines + identity
    }
  }

  def main(args: Array[String]): Unit = {
    // the clients' and the fetch chain's logs are noise here: every miss is named below, once
    org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME) match {
      case root: ch.qos.logback.classic.Logger => root.setLevel(ch.qos.logback.classic.Level.OFF)
      case _                                   => ()
    }
    val a = parse(args.toSeq)
    val o = run(a)
    print(o.text)
    a.out.foreach(p => Files.writeString(p, tsv(o.rows), UTF_8))
    if (o.misses.nonEmpty) {
      System.err.println(s"\nFAIL: the recorded trees lack ${o.misses.size} request(s) this code made — no listing above is a replay of them:")
      o.misses.foreach(m => System.err.println(s"  $m"))
      System.exit(3)
    }
    System.exit(0)
  }
}
