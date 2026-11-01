package scripts

import models.Country
import play.api.libs.json.{JsObject, JsString, Json}
import services.identity._
import services.movies.{ListingKey, ScrapeListing, TitleNormalizer}
import services.identity.adapters.TmdbStore
import tools.{MutableClock, ProdFilm, ProdFilms, ProdIdentityInputs, ProdListings, ProdLookupInputs, ProdRetained}

import java.nio.file.{Files, Path}

/**
 * PRODUCTION'S LIVE TAKES AGAINST A WHOLE RESOLVE, off the worker. The worker's own nightly check
 * (`services.identity.adapters.IdentityDriftCheck`) resolved every listing whole beside the model's state and OOM-killed
 * worker-us two nights running (442cff245); this is the same comparison on CI's heap, over what a "Record scrape
 * fixtures" recording holds of one moment in prod:
 *
 *  - LIVE: the films prod was screening when the corpus was recorded (`corpus/prod-films-<cc>.json.gz`, [[ProdFilm]]),
 *    each with the venue slots it screens in — what the identity projection last wrote, its TMDB film or none;
 *  - WHOLE: the listings prod's model read resolved whole with nothing kept — the model's resolve over prod's pins and a
 *    fresh agreement stage asked to its fixpoint over prod's filed family answers ([[IdentityDrift.whole]], the CI gate's
 *    own whole resolve), answered as the worker answers its model: prod's TMDB store, venue pages and question memory
 *    first, the recording's live answers only where the worker would ask live ([[tools.ProdLookupInputs.lookups]]). All
 *    of it as the recording captured it at its moment ([[tools.ProdIdentityInputs]], [[tools.ProdListings]],
 *    [[tools.ProdLookupInputs]]); the capture (`--families`) lends only what prod's answers lack.
 *
 * What the whole side still reads otherwise than the worker did is named in the TSV's header and the JSON's `inputs`
 * ([[Differences]]), never left implicit. A recording without prod's inputs, or a `--families` file that is not there,
 * is refused (exit 2): an unpinned resolve over a capture's week-old answers compared nothing but the inputs.
 *
 * Each listing is joined to prod's film through its slot key — the page's own slot where prod keeps a page apart
 * (`VenueSlots.slotKey`), else the venue's slot for its title. A listing in no screening slot, a slot two of prod's films
 * name, and a slot whose listings the whole resolve splits between films are not compared (one film per slot is all the
 * read model can say); a listing the whole resolve leaves on a model gap — an answer the recording does not hold — is
 * unsettled, no evidence either way. Every other listing whose TMDB film differs is DRIFT, by what adopting the whole
 * resolve would do (`lost`, `gained`, `changed`). Fallback films (IMDb, Filmweb, Wikidata) are not compared: the read
 * model keeps only some of them.
 *
 * {{{
 *   IdentityLiveDrift <cc> --recording DIR --families CAPTURE|none [--inputs DIR] [--out DIR]
 * }}}
 * `--inputs DIR`: where prod's inputs were captured (`corpus/prod-pins-<cc>.json`, `corpus/prod-family-answers-<cc>.json.gz`,
 * `corpus/prod-listings-<cc>.json.gz`, `corpus/prod-lookups-<cc>.json.gz` under it), the recording's own by default.
 * Writes `live-drift-<cc>.tsv` (every drifted listing) and `live-drift-<cc>.json` (the counts
 * scripts/ci/identity_live_drift.py pushes as `kinowo_identity_drift_*`). Exit: 0 compared, whatever drifted — the count
 * is the finding, and the `IdentityIncrementalDrift` alert reads it; 2 it could not compare.
 */
object IdentityLiveDrift {
  import IdentitySlice.Recording

  final case class Options(country: String = "", recording: Option[Path] = None,
                           /** `--families`: Some(None) is `none`, said outright; None is not said, refused. */
                           families: Option[Option[Path]] = None, inputs: Option[Path] = None,
                           out: Path = Path.of("target/identity-live-drift"))

  def parse(args: List[String], o: Options = Options()): Options = args match {
    case Nil                        => o
    case "--recording" :: v :: rest => parse(rest, o.copy(recording = Some(Path.of(v))))
    case "--families" :: v :: rest  => parse(rest, o.copy(families = Some(Option.when(v != "none")(Path.of(v)))))
    case "--inputs" :: v :: rest    => parse(rest, o.copy(inputs = Some(Path.of(v))))
    case "--out" :: v :: rest       => parse(rest, o.copy(out = Path.of(v)))
    case cc :: rest if !cc.startsWith("--") && o.country.isEmpty => parse(rest, o.copy(country = cc.toLowerCase(java.util.Locale.ROOT)))
    case other :: _                 => usage(s"unknown argument $other")
  }

  private def usage(why: String): Nothing = {
    System.err.println(s"IdentityLiveDrift: $why\n  IdentityLiveDrift <cc> --recording DIR --families CAPTURE|none [--inputs DIR] [--out DIR]")
    sys.exit(2)
  }

  /** Each screening slot's film in prod — its TMDB id, or none — kept only where one film names the slot. */
  def liveBySlot(films: Seq[ProdFilm]): Map[String, Option[Int]] =
    films.flatMap(f => f.slots.map(_ -> f.tmdbId)).groupMap(_._1)(_._2).collect { case (slot, Seq(film)) => slot -> film }

  /** A listing's slot keys, in the order they are tried: the page's own (`<venue>␟<title key>#<page hash>`), then the
   *  venue's slot for its title — as `VenueSlots.slotKey` keys them. */
  def slotKeys(cinema: models.Cinema, cm: models.CinemaMovie, normalizer: TitleNormalizer): Seq[String] = {
    val venue = s"${cinema.displayName}${models.CinemaShowing.Separator}${ScrapeListing.slotKey(cinema, cm.movie.title, normalizer)}"
    cm.filmUrl.map(_.trim).filter(_.nonEmpty).map(page => s"$venue#${Integer.toHexString(page.hashCode)}").toSeq :+ venue
  }

  /** A listing whose live TMDB film differs from the whole resolve's — and its decision over prod's filed answers alone
   *  (`filedOnly`: no question asked, every cluster waiting on one left waiting), when that resolve was run. */
  final case class Drift(listing: ListingKey, slot: String, live: Option[Int], whole: ResolverDecision,
                         filedOnly: Option[ResolverDecision] = None) {
    def kind: String = (live.isDefined, whole.film.isDefined) match {
      case (true, false) => TakeChange.Kind.Lost.label
      case (false, true) => TakeChange.Kind.Gained.label
      case _             => TakeChange.Kind.Changed.label
    }
    def tsv(country: String): String =
      Seq(country, listing.venue, listing.rawTitle, slot, kind, live.fold("none")(id => s"tmdb:$id"), whole.film.fold("none")(id => s"tmdb:$id"),
        whole.basis.toString, whole.explanation.lastOption.getOrElse(""), filedOnly.fold("")(_.film.fold("none")(id => s"tmdb:$id")),
        filedOnly.fold("")(_.basis.toString)).map(_.replace('\t', ' ').replace('\n', ' ')).mkString("\t")
    /** Prod's take is what its filed answers alone decide: the whole reaches its film only by asking what prod has not
     *  filed yet — an input prod lacks, not a take its incremental state kept. */
    def waitsOnAsks: Boolean = filedOnly.exists(_.film == live)
  }
  val Header = "country\tvenue\trawTitle\tslot\tkind\tliveFilm\twholeFilm\twholeBasis\twholeWhy\tfiledOnlyFilm\tfiledOnlyBasis"

  /** `listings` recorded, `compared` of them, `unsettled` left on a model gap, and the drifted. */
  final case class Report(listings: Int, compared: Int, unsettled: Int, drifted: Seq[Drift]) {
    def byKind: Map[String, Int] = drifted.groupMapReduce(_.kind)(_ => 1)(_ + _)
  }

  /** Every listing (with its slot keys) joined to prod's film and compared with the whole resolve's — see the header for
   *  what is left out. */
  /** The model's input as prod's take-up holds it: the listings `billed`, and those of `recorded` (prod's kept set,
   *  [[ProdRetained]]) a billed listing still bills the title of ([[services.identity.ListingRetention.restored]]). */
  def modelInput(billed: Seq[services.identity.Listing], recorded: Seq[services.identity.Listing],
                 normalizer: TitleNormalizer): Seq[services.identity.Listing] =
    billed ++ services.identity.ListingRetention.restored(recorded, billed, normalizer).kept

  def compare(listings: Seq[(ListingKey, Seq[String])], live: Map[String, Option[Int]], whole: Map[ListingKey, ResolverDecision]): Report = {
    val joined = listings.flatMap { case (key, slots) =>
      for { slot <- slots.find(live.contains); decision <- whole.get(key) } yield (key, slot, decision)
    }
    val (settled, unsettled) = joined.partition(j => WaitingOn.model(j._3).settled)
    val compared = settled.groupBy(_._2).values.filter(_.map(_._3.film).distinct.sizeIs == 1).flatten.toSeq
    val drifted = compared.collect { case (key, slot, decision) if live(slot) != decision.film => Drift(key, slot, live(slot), decision) }
    Report(listings.size, compared.size, unsettled.size, drifted.sortBy(d => (d.listing.venue, d.listing.rawTitle)))
  }

  /** The `--families` capture's answers: refused when the file is not there — never an empty set read as "prod filed
   *  nothing" — and none only when said outright. */
  def filedOf(families: Option[Path], country: Country): Either[String, tools.FiledAnswers] = families match {
    case None                              => Right(tools.FiledAnswers.of(country))
    case Some(path) if Files.exists(path) => Right(tools.FiledAnswers.ofCapture(path))
    case Some(path)                        => Left(s"no --families capture at $path")
  }

  /** What the whole side read of prod's inputs, and what it still reads otherwise than the worker did: the TSV's header
   *  lines (`# …`) and the JSON's `inputs`, so a drift count never stands on an input difference nobody named. */
  final case class Differences(pins: Int, familyAnswers: Int, acceptedListings: Int, lookups: String, capture: String) {
    def inputs: String = s"prod's $pins pin(s), $familyAnswers family answer(s), $acceptedListings accepted listing(s) and $lookups as recorded, " +
      s"the capture ${if (capture.isEmpty) "none" else capture} beneath"
    def header: Seq[String] = s"# inputs: $inputs" +: Differences.Otherwise.map { case (input, why) => s"# differs: $input — $why" }
    def json: JsObject = Json.obj("pins" -> pins, "familyAnswers" -> familyAnswers, "acceptedListings" -> acceptedListings, "lookups" -> lookups,
      "capture" -> capture, "differs" -> JsObject(Differences.Otherwise.map { case (input, why) => input -> JsString(why) }))
  }
  object Differences {
    /** What the whole side still reads otherwise than the worker: each by design, none for want of a capture. */
    val Otherwise: Seq[(String, String)] = Seq(
      "stored verdicts" -> "none: a whole resolve keeps nothing, which is what the check compares against",
      "live asks" -> ("the recording's answer where the worker would ask TMDB or IMDb live (prod's store holding none, its question " +
        "memory having the question due), and a candidate's cast and overview, which only the agreement stage reads whole"),
      "agreement questions" -> "a family question prod filed no answer to is asked of the recording's sites until the stage settles, not after the queue's backoff",
      "clock" -> "the recording's moment, the capture's within minutes of it")
  }

  def main(args: Array[String]): Unit = {
    val o         = parse(args.toList)
    val country   = Country.all.find(_.code == o.country).getOrElse(usage(s"no country '${o.country}'"))
    val recording = Recording(o.recording.getOrElse(usage("--recording DIR (a recording's test/resources/fixtures)")), country)
    val prodFilms = recording.root.resolve("corpus").resolve(s"prod-films-${country.code}.json.gz")
    if (!Files.exists(prodFilms)) usage(s"no $prodFilms: the recording holds no prod films to compare with")
    val started   = tools.Stopwatch.start()
    val capture   = filedOf(o.families.getOrElse(usage("--families CAPTURE|none: the capture whose answers prod's do not hold")), country)
      .fold(usage, identity)
    val inputsDir = o.inputs.getOrElse(recording.root)
    val prod      = ProdIdentityInputs.readFrom(inputsDir, country.code).getOrElse(usage(
      s"no ${ProdIdentityInputs.pinsPath(inputsDir, country.code)} and ${ProdIdentityInputs.familiesPath(inputsDir, country.code)}: " +
        "the recording holds no capture of prod's pins and family answers (recorded before 2026-10-25, or the recorder could not read them)"))
    val (accepted, prodLookups) = ProdListings.readFrom(inputsDir, country.code).zip(ProdLookupInputs.readFrom(inputsDir, country.code)).getOrElse(usage(
      s"no ${ProdListings.path(inputsDir, country.code)} and ${ProdLookupInputs.path(inputsDir, country.code)}: the recording holds no capture of " +
        "the listings prod's model read and its TMDB store, venue pages and question memory (recorded before 2026-10-26, or the recorder could not read them)"))
    val filed     = tools.FiledAnswers.preferring(capture, prod.families, s"prod's family answers at $inputsDir")
    val opened    = IdentitySlice.openRecording(recording, filed)
    // the listings prod's model read: each venue's accepted listing, the recording's scrape where it accepted none
    val rows      = ProdListings.merged(opened.rows, accepted)
    val normalizer = TitleNormalizer.forCountry(country)
    // and the listings prod's model kept for their facts alone (ProdRetained), as its take-up resumes them
    val billed    = IdentitySlice.listingsOf(recording, rows)
    val listings  = modelInput(billed, ProdRetained.readFrom(inputsDir, country.code), normalizer)
    val venues    = tools.CountryScrapeCorpus.cinemasOf(country).toSet
    val slots     = rows.filter(r => venues(r.cinema)).flatMap(r => r.films.map(cm => ListingKey.of(r.cinema, cm) -> slotKeys(r.cinema, cm, normalizer)))
      .distinctBy(_._1)
    val differences = Differences(prod.pins.size, prod.families.size, accepted.size, prodLookups.render, capture.source)
    System.err.println(s"[identity-live-drift] ${country.code}: ${listings.size} listings, ${filed.render}; ${differences.inputs}")
    val clock     = new MutableClock(recording.recordedAt)
    // prod's pins as the worker reads them; its stored verdicts not: a whole resolve keeps nothing (Differences)
    val reads     = new IdentitySlice.Reads(country, opened.fetch, opened.gaps, clock, filed,
      tools.ProdState(s"recording ${recording.root}", Nil, prod.pins, _ => Map.empty))
    // answered as the worker answers its model: prod's store first, the recording only where the worker would ask live
    val held      = new ProdLookupInputs.Held(prodLookups)
    val answering = IdentityDrift.WholeLookups(
      ProdLookupInputs.lookups(held, prodLookups.pages, reads.enrichers, IdentityDrift.memo(reads), country, clock), new TmdbStore(held, clock).foundFirst)
    // prod's filed answers alone first: the asking resolve files what it asks, which this one must not read
    val filedOnly = IdentityDrift.whole(reads, listings, clock, answering, ask = false).decisionOf
    val whole     = IdentityDrift.whole(reads, listings, clock, answering)
    val compared  = compare(slots, liveBySlot(ProdFilms.readFrom(prodFilms)), whole.decisionOf)
    val report    = compared.copy(drifted = compared.drifted.map(d => d.copy(filedOnly = filedOnly.get(d.listing))))
    Files.createDirectories(o.out)
    Files.writeString(o.out.resolve(s"live-drift-${country.code}.tsv"),
      ((differences.header :+ Header) ++ report.drifted.map(_.tsv(country.code))).mkString("", "\n", "\n"))
    Files.writeString(o.out.resolve(s"live-drift-${country.code}.json"), Json.stringify(Json.obj(
      "country" -> country.code, "recordedAt" -> recording.recordedAt.toString, "listings" -> report.listings, "compared" -> report.compared,
      "unsettled" -> report.unsettled, "drifted" -> Json.toJson(Seq("lost", "gained", "changed").map(k => k -> report.byKind.getOrElse(k, 0)).toMap),
      "waitsOnAsks" -> report.drifted.count(_.waitsOnAsks),
      "inputs" -> differences.json)))
    println(f"## ${country.code}: ${report.drifted.size} of ${report.compared} compared listings drifted from a whole resolve " +
      f"(${report.listings} recorded, ${report.unsettled} unsettled; ${report.byKind.toSeq.sorted.map { case (k, n) => s"$k $n" }.mkString(", ")}) " +
      f"in ${started.seconds}%.0fs; ${report.drifted.count(_.waitsOnAsks)} of them prod's filed answers alone take as prod does")
    report.drifted.take(LinesPrinted).foreach { d =>
      println(s"      ${d.listing.venue} | ${d.listing.rawTitle}: ${d.kind}, live ${d.live.fold("none")(id => s"tmdb:$id")}, " +
        s"whole ${d.whole.film.fold("none")(id => s"tmdb:$id")} (${d.whole.basis})" + (if (d.waitsOnAsks) ", waits on asks" else ""))
    }
  }

  /** How many drifted listings the report prints: the TSV holds every one. */
  val LinesPrinted = 40
}
