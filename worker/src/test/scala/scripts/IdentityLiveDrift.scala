package scripts

import models.Country
import play.api.libs.json.Json
import services.identity._
import services.movies.{ListingKey, ScrapeListing, TitleNormalizer}
import tools.{MutableClock, ProdFilm, ProdFilms}

import java.nio.file.{Files, Path}

/**
 * PRODUCTION'S LIVE TAKES AGAINST A WHOLE RESOLVE, off the worker. The worker's own nightly check
 * (`services.identity.adapters.IdentityDriftCheck`) resolved every listing whole beside the model's state and OOM-killed
 * worker-us two nights running (442cff245); this is the same comparison on CI's heap, over what a "Record scrape
 * fixtures" recording holds of one moment in prod:
 *
 *  - LIVE: the films prod was screening when the corpus was recorded (`corpus/prod-films-<cc>.json.gz`, [[ProdFilm]]),
 *    each with the venue slots it screens in — what the identity projection last wrote, its TMDB film or none;
 *  - WHOLE: the recording's listings resolved whole with nothing kept — the model's resolve and a fresh agreement stage
 *    asked to its fixpoint over prod's filed family answers ([[IdentityDrift.whole]], the CI gate's own whole resolve).
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
 *   IdentityLiveDrift <cc> --recording DIR [--families CAPTURE|none] [--out DIR]
 * }}}
 * Writes `live-drift-<cc>.tsv` (every drifted listing) and `live-drift-<cc>.json` (the counts
 * scripts/ci/identity_live_drift.py pushes as `kinowo_identity_drift_*`). Exit: 0 compared, whatever drifted — the count
 * is the finding, and the `IdentityIncrementalDrift` alert reads it; 2 it could not compare.
 */
object IdentityLiveDrift {
  import IdentitySlice.Recording

  final case class Options(country: String = "", recording: Option[Path] = None, families: Option[Path] = None,
                           out: Path = Path.of("target/identity-live-drift"))

  def parse(args: List[String], o: Options = Options()): Options = args match {
    case Nil                        => o
    case "--recording" :: v :: rest => parse(rest, o.copy(recording = Some(Path.of(v))))
    case "--families" :: v :: rest  => parse(rest, o.copy(families = Option.when(v != "none")(Path.of(v))))
    case "--out" :: v :: rest       => parse(rest, o.copy(out = Path.of(v)))
    case cc :: rest if !cc.startsWith("--") && o.country.isEmpty => parse(rest, o.copy(country = cc.toLowerCase(java.util.Locale.ROOT)))
    case other :: _                 => usage(s"unknown argument $other")
  }

  private def usage(why: String): Nothing = {
    System.err.println(s"IdentityLiveDrift: $why\n  IdentityLiveDrift <cc> --recording DIR [--families CAPTURE|none] [--out DIR]")
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

  /** A listing whose live TMDB film differs from the whole resolve's. */
  final case class Drift(listing: ListingKey, slot: String, live: Option[Int], whole: ResolverDecision) {
    def kind: String = (live.isDefined, whole.film.isDefined) match {
      case (true, false) => TakeChange.Kind.Lost.label
      case (false, true) => TakeChange.Kind.Gained.label
      case _             => TakeChange.Kind.Changed.label
    }
    def tsv(country: String): String =
      Seq(country, listing.venue, listing.rawTitle, slot, kind, live.fold("none")(id => s"tmdb:$id"), whole.film.fold("none")(id => s"tmdb:$id"),
        whole.basis.toString, whole.explanation.lastOption.getOrElse("")).map(_.replace('\t', ' ').replace('\n', ' ')).mkString("\t")
  }
  val Header = "country\tvenue\trawTitle\tslot\tkind\tliveFilm\twholeFilm\twholeBasis\twholeWhy"

  /** `listings` recorded, `compared` of them, `unsettled` left on a model gap, and the drifted. */
  final case class Report(listings: Int, compared: Int, unsettled: Int, drifted: Seq[Drift]) {
    def byKind: Map[String, Int] = drifted.groupMapReduce(_.kind)(_ => 1)(_ + _)
  }

  /** Every listing (with its slot keys) joined to prod's film and compared with the whole resolve's — see the header for
   *  what is left out. */
  def compare(listings: Seq[(ListingKey, Seq[String])], live: Map[String, Option[Int]], whole: Map[ListingKey, ResolverDecision]): Report = {
    val joined = listings.flatMap { case (key, slots) =>
      for { slot <- slots.find(live.contains); decision <- whole.get(key) } yield (key, slot, decision)
    }
    val (settled, unsettled) = joined.partition(j => WaitingOn.model(j._3).settled)
    val compared = settled.groupBy(_._2).values.filter(_.map(_._3.film).distinct.sizeIs == 1).flatten.toSeq
    val drifted = compared.collect { case (key, slot, decision) if live(slot) != decision.film => Drift(key, slot, live(slot), decision) }
    Report(listings.size, compared.size, unsettled.size, drifted.sortBy(d => (d.listing.venue, d.listing.rawTitle)))
  }

  def main(args: Array[String]): Unit = {
    val o         = parse(args.toList)
    val country   = Country.all.find(_.code == o.country).getOrElse(usage(s"no country '${o.country}'"))
    val recording = Recording(o.recording.getOrElse(usage("--recording DIR (a recording's test/resources/fixtures)")), country)
    val prodFilms = recording.root.resolve("corpus").resolve(s"prod-films-${country.code}.json.gz")
    if (!Files.exists(prodFilms)) usage(s"no $prodFilms: the recording holds no prod films to compare with")
    val started   = tools.Stopwatch.start()
    val filed     = o.families.filter(Files.exists(_)).fold(tools.FiledAnswers.of(country))(tools.FiledAnswers.ofCapture)
    val opened    = IdentitySlice.openRecording(recording, filed)
    val listings  = IdentitySlice.listingsOf(recording, opened.rows)
    val normalizer = TitleNormalizer.forCountry(country)
    val venues    = tools.CountryScrapeCorpus.cinemasOf(country).toSet
    val slots     = opened.rows.filter(r => venues(r.cinema)).flatMap(r => r.films.map(cm => ListingKey.of(r.cinema, cm) -> slotKeys(r.cinema, cm, normalizer)))
      .distinctBy(_._1)
    System.err.println(s"[identity-live-drift] ${country.code}: ${listings.size} listings, ${filed.render}")
    val clock     = new MutableClock(recording.recordedAt)
    val whole     = IdentityDrift.whole(new IdentitySlice.Reads(country, opened.fetch, opened.gaps, clock, filed), listings, clock)
    val report    = compare(slots, liveBySlot(ProdFilms.readFrom(prodFilms)), whole.decisionOf)
    Files.createDirectories(o.out)
    Files.writeString(o.out.resolve(s"live-drift-${country.code}.tsv"), (Header +: report.drifted.map(_.tsv(country.code))).mkString("", "\n", "\n"))
    Files.writeString(o.out.resolve(s"live-drift-${country.code}.json"), Json.stringify(Json.obj(
      "country" -> country.code, "recordedAt" -> recording.recordedAt.toString, "listings" -> report.listings, "compared" -> report.compared,
      "unsettled" -> report.unsettled, "drifted" -> Json.toJson(Seq("lost", "gained", "changed").map(k => k -> report.byKind.getOrElse(k, 0)).toMap))))
    println(f"## ${country.code}: ${report.drifted.size} of ${report.compared} compared listings drifted from a whole resolve " +
      f"(${report.listings} recorded, ${report.unsettled} unsettled; ${report.byKind.toSeq.sorted.map { case (k, n) => s"$k $n" }.mkString(", ")}) " +
      f"in ${started.seconds}%.0fs")
    report.drifted.take(LinesPrinted).foreach { d =>
      println(s"      ${d.listing.venue} | ${d.listing.rawTitle}: ${d.kind}, live ${d.live.fold("none")(id => s"tmdb:$id")}, " +
        s"whole ${d.whole.film.fold("none")(id => s"tmdb:$id")} (${d.whole.basis})")
    }
  }

  /** How many drifted listings the report prints: the TSV holds every one. */
  val LinesPrinted = 40
}
