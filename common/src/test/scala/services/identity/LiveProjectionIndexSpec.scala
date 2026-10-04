package services.identity

import models.{Cinema, CinemaMovie, CinemaShowing, Helios, KinoApollo, KinoMuza, Movie, MovieRecord, Multikino, Rialto, Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{FilmId, ListingKey, SingleCountryNormalizer, StoredMovieRecord}

import java.time.LocalDateTime
import scala.util.Random

/**
 * [[LiveProjectionIndex]] moved step by step is, after every step, the index [[IdentityProjectionPlan.index]] builds afresh
 * from the same inputs — venues joining and leaving the roster, listings re-scraped, the model taking a listing up or
 * letting it go with no decision of its moving, decisions replaced only where a family moved, stored films written,
 * changed and deleted, two films of one slot title at a venue. `ScopedProjectionEquivalenceSpec` holds the projection
 * built on it to the whole one; this holds the index itself, over inputs that spec's worlds do not reach.
 */
class LiveProjectionIndexSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val start      = LocalDateTime.of(2026, 10, 5, 18, 0)
  private val venues: Seq[Cinema] = Seq(Multikino, Helios, KinoApollo, Rialto, KinoMuza)
  private val titles = Seq(("Lalka", Some(2026), Seq("Maciej Kawalski")), ("Lalka", Some(1968), Seq("Wojciech Has")), ("Lalka 2D", Some(2026), Nil),
    ("Obcy", Some(1979), Nil), ("Diuna", Some(2021), Nil), ("Belle", Some(2013), Seq("Amma Asante")), ("Belle", Some(2021), Nil))

  private def row(rng: Random, cinema: Cinema): CinemaMovie = {
    val (title, year, director) = titles(rng.nextInt(titles.size))
    CinemaMovie(Movie(title, releaseYear = year), cinema, None, Option.when(rng.nextBoolean())(s"https://${cinema.pillName}/$title-$year"), None, Nil,
      director, Seq(Showtime(start.plusHours(rng.nextInt(48).toLong), None)))
  }

  /** A stored film holding slots for some of `listings` — its own, or a same-title fold's — at their venues. */
  private def film(rng: Random, id: String, listings: Seq[ProjectedListing]): StoredMovieRecord = {
    val held = rng.shuffle(listings).take(1 + rng.nextInt(3))
    val data = held.map { l =>
      (CinemaShowing.keyFor(l.listing.cinema, l.listing.cleanTitle, normalizer): models.Source) -> SourceData(title = Some(l.listing.cleanTitle),
        rawTitle = Some(l.listing.rawTitle), releaseYear = if (rng.nextInt(4) == 0) Some(1990) else l.listing.year,
        director = if (rng.nextInt(4) == 0) Seq("Someone") else l.listing.directors, filmUrl = l.listing.page)
    }.toMap
    StoredMovieRecord("Film", None, MovieRecord(tmdbId = Option.when(rng.nextBoolean())(rng.nextInt(5)), data = data), FilmId(id), Some(s"film$id|"))
  }

  "The index kept between projections" should "be, after every step, the index built afresh from the same inputs" in {
    (1 to 300).foreach { seed =>
      val rng       = new Random(seed)
      val live      = new LiveProjectionIndex(normalizer)
      var programme = venues.map(c => c.displayName -> Seq.fill(1 + rng.nextInt(4))(row(rng, c))).toMap
      var objects   = Map.empty[String, Seq[ProjectedListing]]            // each venue's listing object, kept while unmoved
      var groupOf   = Map.empty[ListingKey, Int]
      var decisions = Map.empty[Int, ResolverDecision]                    // by group: replaced only when the group moved
      var unheld    = Set.empty[ListingKey]
      var stored    = Map.empty[String, StoredMovieRecord]
      def listings(venue: String): Seq[ProjectedListing] =
        objects.getOrElse(venue, Nil)
      (1 to 20).foreach { step =>
        // The world moves.
        rng.nextInt(7) match {
          case 0 => val v = venues(rng.nextInt(venues.size)); programme += v.displayName -> Seq.fill(1 + rng.nextInt(4))(row(rng, v))
          case 1 => programme -= venues(rng.nextInt(venues.size)).displayName               // a venue leaves the roster
          case 2 => val v = venues(rng.nextInt(venues.size)); if (!programme.contains(v.displayName)) programme += v.displayName -> Seq(row(rng, v))
          case 3 => objects.values.flatten.toSeq.sortBy(_.listing.key).headOption.foreach(l => unheld = if (unheld(l.listing.key)) unheld - l.listing.key else unheld + l.listing.key)
          case 4 => objects.values.flatten.toSeq.sortBy(_.listing.key).lastOption.foreach(l => groupOf += l.listing.key -> rng.nextInt(4))
          case 5 =>
            val all = objects.values.flatten.toSeq.sortBy(_.listing.key)
            if (all.nonEmpty) { val id = f"f$step%03d${rng.nextInt(9)}"; stored += id -> film(rng, id, all) }
          case _ => if (stored.nonEmpty) stored -= stored.keys.toSeq.sorted.apply(rng.nextInt(stored.size))
        }
        objects = programme.map { case (venue, rows) =>
          val now = rows.map(cm => ProjectedListing.of(Listing.of(cm.cinema, cm, normalizer), cm))
          venue -> objects.get(venue).filter(_ == now).getOrElse(now)
        }
        val all     = objects.values.flatten.toSeq
        val grouped = all.map(_.listing.key).distinct.groupBy(k => groupOf.getOrElse(k, k.## & 3))
        decisions = grouped.map { case (g, keys) =>
          val members = keys.sorted
          g -> decisions.get(g).filter(_.members == members).getOrElse(
            ResolverDecision(members, Option.when(g % 2 == 0)(g), 0.9, ResolverDecision.Basis.OwnMatch, Nil)())
        }
        val held    = (k: ListingKey) => !unheld(k)
        val counters = FilmIdCounters.of(stored.keys.toSeq.sorted.zipWithIndex.map { case (id, i) => FilmIdCounter(id, i + 1L) }).toOption.get
        live.update(objects.toSeq, held, decisions.values.toSeq, stored.values.toSeq)
        // Now and then a projection writes: a stored film as written, one retired.
        if (rng.nextInt(3) == 0 && stored.nonEmpty) {
          val id      = stored.keys.toSeq.sorted.head
          val written = stored(id)
          live.written(Seq(ProjectedFilm(written.id, 1, written.title, written.year, written.key(normalizer), written.record,
            written.record.data.keys.toSeq.flatMap(s => ListingKey.ofSource(s, written.record.data(s))))), Nil)
        }
        val kept  = live.index(counters)
        val built = IdentityProjectionPlan.index(all.filter(l => held(l.listing.key)),
          Resolution(decisions.values.toSeq, 0, Map.empty, Nil, Nil, 0, 0, 0, 0, 0, Map.empty), stored.values.toSeq, counters, normalizer)
        withClue(s"seed $seed, step $step: ") {
          kept.byKey shouldBe built.byKey
          kept.previousOf shouldBe built.previousOf
          kept.listingsOf shouldBe built.listingsOf
          kept.clusters shouldBe built.clusters
          kept.clusterOf shouldBe built.clusterOf
          kept.storedById.keySet shouldBe built.storedById.keySet
        }
      }
    }
  }

  it should "hold a venue's listings as last read, not the read they replaced" in {
    // A scrape reads every venue again into new objects, moved or not. An unmoved listing is no change — but indexed as
    // first read, every venue read since kept a second copy of its listings, showtimes and all, for as long as it ran.
    val live    = new LiveProjectionIndex(normalizer)
    val rows    = Seq(row(new Random(1), Multikino), row(new Random(2), Multikino))
    def read()  = rows.map(cm => ProjectedListing.of(Listing.of(cm.cinema, cm, normalizer), cm))
    val first   = read()
    val decided = first.map(l => ResolverDecision(Seq(l.listing.key), Some(1), 0.9, ResolverDecision.Basis.OwnMatch, Nil)())
    live.update(Seq(Multikino.displayName -> first), _ => true, decided, Nil)
    val again   = read()
    again shouldBe first
    val changes = live.update(Seq(Multikino.displayName -> again), _ => true, decided, Nil)
    changes.listings shouldBe empty
    val byKey   = live.index(FilmIdCounters.empty).byKey
    again.foreach(l => withClue(l.listing.rawTitle)(byKey(l.listing.key) should be theSameInstanceAs l))
  }

  it should "name each listing by its own key object, not the copies a decision or a stored slot was read with" in {
    // A decision's keys and a stored slot's are read anew from the store: kept as they came, the index held ~4.8 key
    // objects per listing on worker-us.
    val live    = new LiveProjectionIndex(normalizer)
    val rows    = Seq(row(new Random(1), Multikino), row(new Random(2), Helios))
    val read    = rows.map(cm => ProjectedListing.of(Listing.of(cm.cinema, cm, normalizer), cm))
    def copy(k: ListingKey): ListingKey = k match {
      case ListingKey.Native(v, p, r)        => ListingKey.Native(new String(v), new String(p), new String(r))
      case ListingKey.Published(v, r, y, ds) => ListingKey.Published(new String(v), new String(r), y, ds.map(new String(_)))
    }
    val decided = read.map(l => ResolverDecision(Seq(copy(l.listing.key)), Some(1), 0.9, ResolverDecision.Basis.OwnMatch, Nil)())
    val stored  = Seq(film(new Random(3), "f1", read))
    live.update(read.groupBy(_.listing.venue).toSeq, _ => true, decided, stored)
    val index   = live.index(FilmIdCounters.of(Seq(FilmIdCounter("f1", 1L))).toOption.get)
    val own     = read.map(l => l.listing.key -> l.listing.key).toMap
    index.clusterOf.keySet should not be empty
    (index.clusterOf.keysIterator ++ index.clusters.valuesIterator.flatMap(_.members) ++ index.previousOf.keysIterator ++
      index.listingsOf.valuesIterator.flatten).foreach(k => withClue(k)(k should be theSameInstanceAs own(k)))
  }
}
