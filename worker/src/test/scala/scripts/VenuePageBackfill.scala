package scripts

import models.{Country, SourceData}
import org.mongodb.scala.{Document, MongoDatabase}
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.Projections
import services.cinemas.common.FilmDetail
import services.venuepages.{MongoVenuePageStore, VenuePage, VenuePageKey}

import java.time.Instant

/**
 * Seeds `venue_pages` — the one place a venue page's facts are written — from what the pages read BEFORE
 * it existed left behind: each page's own stamp in `freshness` (`detail-page|<group>|<page>|read` or
 * `|gone`) and the slot its facts were merged into. A venue's own page answers from the slot that names
 * it; a chain's page from the film's chain slot (keyed by the chain's name, e.g. `Cinema City`), only
 * where the film names that chain no other page — the rule `VenueDetailSlots` reads them by. A page the
 * store already holds is left alone: a real read beats a seeded one. The seeded facts are the slot's,
 * the listing's own values merged in, exactly what the slots answered until now; each page's next read
 * replaces them with what the page itself says.
 *
 * DRY RUN BY DEFAULT; `--apply` writes. Each country is its own database (`Country.mongoDb`).
 *   sbt "worker/Test/runMain scripts.VenuePageBackfill pl uk"           # dry run
 *   sbt "worker/Test/runMain scripts.VenuePageBackfill --apply pl uk"   # WRITE
 */
object VenuePageBackfill {

  /** A page's stamp: read, or found gone. */
  final case class Stamp(key: VenuePageKey, gone: Boolean)
  /** A slot as the scan reads it: the film it is on, its key there, and what it holds. */
  final case class Slot(filmId: String, slotKey: String, data: SourceData)

  final case class Plan(writes: Seq[VenuePage], alreadyStored: Int, readWithoutSlot: Seq[VenuePageKey], chainAmbiguous: Seq[VenuePageKey])
  /** A read stamped on the FILM, before pages had stamps of their own: `detail|<group>|<film>|read`. */
  final case class FilmMarker(detailGroup: String, filmId: String)

  /** The pages per-film markers name, as page stamps: the film's venue slots at the group's cinemas —
   *  learned from the group's page-stamped pages, the only place a group says which venues it reads.
   *  A marker whose group stamped no page, so whose venues are unknown, names nothing: never a guess.
   *  Answers the stamps and how many markers could not be placed. */
  def markerStamps(markers: Seq[FilmMarker], stamps: Seq[Stamp], slots: Seq[Slot]): (Seq[Stamp], Int) = {
    def cinemaOf(slot: Slot) = slot.slotKey.takeWhile(_ != SlotSep)
    val venueSlots = slots.filter(_.slotKey.contains(SlotSep))
    val byPage     = venueSlots.flatMap(sl => services.cinemas.common.DetailEnricher.nativeRefOf(sl.data).map(_ -> sl)).groupMap(_._1)(_._2)
    val cinemasOf  = stamps.groupMap(_.key.detailGroup)(st => byPage.getOrElse(st.key.page, Nil).map(cinemaOf)).view
      .mapValues(_.flatten.toSet).toMap
    val byFilm     = venueSlots.groupBy(_.filmId)
    val placed = markers.distinct.map { m =>
      val venues = cinemasOf.getOrElse(m.detailGroup, Set.empty)
      byFilm.getOrElse(m.filmId, Nil).filter(sl => venues(cinemaOf(sl)))
        .flatMap(sl => services.cinemas.common.DetailEnricher.nativeRefOf(sl.data))
        .distinct.map(page => Stamp(VenuePageKey(m.detailGroup, page), gone = false))
    }
    (placed.flatten.distinctBy(_.key.id), placed.count(_.isEmpty))
  }


  private val SlotSep = '␟'

  /** The whole seed as a value: pure, and the same whatever order the rows came in. */
  def plan(stamps: Seq[Stamp], slots: Seq[Slot], stored: Set[String], at: Instant): Plan = {
    val byPage   = slots.flatMap(s => services.cinemas.common.DetailEnricher.nativeRefOf(s.data).map(_ -> s)).groupMap(_._1)(_._2)
    val byFilm   = slots.groupBy(_.filmId)
    val fresh    = stamps.filterNot(st => stored(st.key.id)).distinctBy(_.key.id)
    // A read stamp wins over a gone one for the same page: the page came back.
    val decided  = fresh.groupBy(_.key.id).values.map(sts => sts.find(!_.gone).getOrElse(sts.head)).toSeq.sortBy(_.key.id)
    val groupPages = stamps.groupMap(_.key.detailGroup)(_.key.page).view.mapValues(_.toSet).toMap
    val writes   = Seq.newBuilder[VenuePage]
    val orphans  = Seq.newBuilder[VenuePageKey]
    val ambiguous = Seq.newBuilder[VenuePageKey]
    decided.foreach { st =>
      if (st.gone) writes += VenuePage(st.key, VenuePage.Gone(404), at)
      else factsOf(st.key, byPage.getOrElse(st.key.page, Nil), byFilm, groupPages.getOrElse(st.key.detailGroup, Set.empty)) match {
        case Right(detail)   => writes += VenuePage(st.key, VenuePage.Read(detail), at)
        case Left(ambiguity) => if (ambiguity) ambiguous += st.key else orphans += st.key
      }
    }
    Plan(writes.result(), stamps.map(_.key.id).distinct.count(stored), orphans.result(), ambiguous.result())
  }

  /** The page's facts: the naming venue slot's when it holds any, else the film's chain slot (keyed by the
   *  chain's name) when the film names that chain's group no other page. `Left(true)`: a chain slot exists but
   *  the film names the chain several pages, whose facts the one shared slot cannot tell apart. */
  private def factsOf(key: VenuePageKey, naming: Seq[Slot], byFilm: Map[String, Seq[Slot]],
                      groupPages: Set[String]): Either[Boolean, FilmDetail] =
    naming.sortBy(s => (s.filmId, s.slotKey)).find(s => holdsPageFacts(s.data)) match {
      case Some(slot) => Right(detailOf(slot.data))
      case None =>
        val chained = naming.map(_.filmId).distinct.sorted.flatMap { film =>
          val slots = byFilm.getOrElse(film, Nil)
          slots.find(s => !s.slotKey.contains(SlotSep) && groupOf(s.slotKey) == key.detailGroup).map { chain =>
            val named = slots.flatMap(s => services.cinemas.common.DetailEnricher.nativeRefOf(s.data)).filter(groupPages).distinct
            if (named == Seq(key.page)) Right(detailOf(chain.data)) else Left(true)
          }
        }
        chained.collectFirst { case r @ Right(_) => r }.getOrElse(Left(chained.nonEmpty))
    }

  /** `Cinema City` → `cinema-city`: how a chain's slot key spells its detail group. */
  private[scripts] def groupOf(slotKey: String): String = slotKey.trim.toLowerCase.replaceAll("\\s+", "-")

  /** Does the slot hold anything a detail page states (beyond what every listing carries)? */
  private def holdsPageFacts(d: SourceData): Boolean =
    d.director.nonEmpty || d.synopsis.isDefined || d.cast.nonEmpty || d.originalTitle.isDefined || d.countries.nonEmpty || d.genres.nonEmpty

  private[scripts] def detailOf(d: SourceData): FilmDetail =
    FilmDetail(synopsis = d.synopsis, cast = d.cast, director = d.director, runtimeMinutes = d.runtimeMinutes,
      releaseYear = d.releaseYear, originalTitle = d.originalTitle, countries = d.countries, genres = d.genres,
      posterUrl = d.posterUrl, trailerUrl = d.trailerUrl, ageRating = d.ageRating)

  // ── reading and writing one country ─────────────────────────────────────────────────────────

  def main(args: Array[String]): Unit = {
    val apply     = args.contains("--apply")
    val requested = args.filterNot(_.startsWith("--")).toSeq
    val countries = if (requested.isEmpty) Country.all else requested.map(code => Country.byCode(code).getOrElse {
      println(s"Unknown country code '$code'"); sys.exit(1)
    })
    println(if (apply) "APPLY — venue_pages entries will be WRITTEN." else "DRY RUN — nothing is written. Pass --apply to write.")
    countries.foreach(seed(_, apply))
    sys.exit(0)
  }

  private def seed(country: Country, apply: Boolean): Unit = {
    val (connection, database) = ListingKeyBackfill.openCountry(country)
    val started = System.nanoTime()
    val slots   = slotsOf(database)
    val paged   = stampsOf(database)
    val (marked, unplaced) = markerStamps(markersOf(database, services.movies.TitleNormalizer.forCountry(country)), paged, slots)
    val stamps  = paged ++ marked.filterNot(m => paged.exists(_.key.id == m.key.id))
    val stored  = ListingKeyBackfill.ids(database, MongoVenuePageStore.Collection).toSet
    val p       = plan(stamps, slots, stored, Instant.now())
    val reads   = p.writes.count(_.outcome.isInstanceOf[VenuePage.Read])
    println(f"${country.displayName}%-15s ${paged.size} page stamps + ${marked.size} pages from per-film markers ($unplaced markers unplaced), ${slots.size} slots · ${p.alreadyStored} already in venue_pages · " +
      s"to write ${p.writes.size} ($reads read, ${p.writes.size - reads} gone) · ${p.readWithoutSlot.size} read with no slot left · " +
      s"${p.chainAmbiguous.size} chain pages ambiguous")
    p.readWithoutSlot.take(5).foreach(k => println(s"    no slot names: ${k.id}"))
    p.chainAmbiguous.take(5).foreach(k => println(s"    ambiguous chain page: ${k.id}"))
    if (apply) {
      val store   = new MongoVenuePageStore(database)
      // Re-check each page just before writing it: one the reader stored since the scan keeps its real read.
      val written = p.writes.count(page => store.get(page.key).isEmpty && store.put(page))
      println(s"    wrote $written of ${p.writes.size}")
    }
    println(f"    in ${(System.nanoTime() - started) / 1e9}%.1fs")
    connection.close()
  }

  private val FilmReadMarker = """^detail\|([^|]+)\|(.+)\|read$""".r

  /** The marker names the film by its display title and year (`Marsupilami|2026`); a film's id is that
   *  title sanitized by the country's rules (`marsupilami|2026`). */
  private def markersOf(database: MongoDatabase, normalizer: services.movies.TitleNormalizer): Seq[FilmMarker] =
    ListingKeyBackfill.scan(database.getCollection[Document]("freshness"), Projections.include("_id"))(d => ListingKeyBackfill.text(d, "_id"))
      .collect { case FilmReadMarker(group, film) =>
        val (title, year) = (film.take(film.lastIndexOf('|')), film.drop(film.lastIndexOf('|') + 1))
        FilmMarker(group, s"${normalizer.sanitize(title)}|$year")
      }

  private val PageStamp = """^detail-page\|([^|]+)\|(.+)\|(read|gone)$""".r

  private def stampsOf(database: MongoDatabase): Seq[Stamp] =
    ListingKeyBackfill.scan(database.getCollection[Document]("freshness"), Projections.include("_id"))(d => ListingKeyBackfill.text(d, "_id"))
      .collect { case PageStamp(group, page, kind) => Stamp(VenuePageKey(group, page), kind == "gone") }

  /** Every slot a page's facts may sit in: the side collection's, the films' inline ones, the staged rows'. */
  private def slotsOf(database: MongoDatabase): Seq[Slot] = {
    val side = ListingKeyBackfill.scan(database.getCollection[Document](services.movies.SlotsRepository.Collection),
      Projections.include("filmId", "slotKey", "slot")) { d =>
      Slot(ListingKeyBackfill.text(d, "filmId"), ListingKeyBackfill.text(d, "slotKey"), ListingKeyBackfill.slotOf(d))
    }
    def inline(collection: String) = ListingKeyBackfill.scan(database.getCollection[Document](collection),
      Projections.include("sourceData")) { d =>
      val film = ListingKeyBackfill.text(d, "_id")
      Option(d.get("sourceData")).filter(_.isDocument).toSeq.flatMap { sd =>
        import scala.jdk.CollectionConverters._
        sd.asDocument.entrySet.asScala.toSeq.collect { case e if e.getValue.isDocument =>
          Slot(film, e.getKey, ListingKeyBackfill.slotOf(new BsonDocument("slot", e.getValue)))
        }
      }
    }.flatten
    side ++ inline("movies") ++ inline("pending_movies")
  }

}
