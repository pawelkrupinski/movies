package services.identity

import models.{Cinema, CinemaCityChain, CinemaCityKinepolis, CinemaMovie, CinemaShowing, Helios, KinoApollo, KinoMuza, KinoPalacowe, Movie,
  MovieRecord, Multikino, Rialto, Showtime, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CacheKey, InMemoryMovieRepository, ListingKey}

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}
import scala.util.Random

/**
 * A projection of a SCOPE — only the films its changes reach ([[ProjectionScope]]) — leaves the store exactly as a
 * projection of the whole corpus would, tick after tick, whatever moves: listings added, gone and re-scraped, the
 * resolver merging, splitting and re-matching clusters (two clusters of one TMDB film joined), TMDB films and
 * unmatched clusters of one title and year contending for one key, a venue's two films of one slot title (Belle 2013 and
 * 2021), TMDB details that fail and are fetched again, other writers changing a stored film (a rating, a chain's
 * network slot), venues emptied past the shrink guard, and the worker restarting.
 *
 * Two worlds take the same scrapes, decisions, details and outside writes: one projects as production does (a scope
 * each tick, the whole corpus every [[IdentityProjection.ScopedBetweenWhole]] + 1), the other the whole corpus every
 * tick. After each tick their stores, FilmId maps, announcements and refusals must be identical — a film the scope
 * missed is a film the two stores disagree on. The whole world also checks each tick against the scope it would have
 * drafted, and must find no drift.
 */
class ScopedProjectionEquivalenceSpec extends AnyFlatSpec with Matchers {

  private val normalizer = ProjectionWorld.normalizer
  private val clock      = Clock.fixed(Instant.parse("2026-09-26T10:00:00Z"), ZoneOffset.UTC)
  private val start      = LocalDateTime.of(2026, 9, 27, 18, 0)
  private val venues: Seq[Cinema] = Seq(Multikino, Helios, KinoApollo, Rialto, KinoMuza, KinoPalacowe, CinemaCityKinepolis)

  /** A title a venue may publish: its year and director as the venue prints them. Lalka and Diuna each twice — one
   *  slot title, two films a venue tells apart by year and director — and Lalka once decorated. */
  private val titles: Seq[(String, Option[Int], Option[String])] = Seq(
    ("Lalka", Some(2026), Some("Maciej Kawalski")), ("Lalka", Some(1968), Some("Wojciech Has")), ("Lalka 2D", Some(2026), None),
    ("Obcy", Some(1979), None), ("Diuna", Some(2021), Some("Denis Villeneuve")), ("Diuna", Some(1984), Some("David Lynch")),
    ("Belle", Some(2013), Some("Amma Asante")), ("Belle", Some(2021), Some("Mamoru Hosoda")), ("Nosferatu", None, None))

  /** What TMDB answers for each film the scenario matches to: two films of one title and year (1, 2), so their keys
   *  contend. */
  private val tmdb: Map[Int, (String, Int)] =
    Map(1 -> ("Lalka", 2026), 2 -> ("Lalka", 2026), 3 -> ("Obcy", 1979), 4 -> ("Diuna", 2021), 5 -> ("Belle", 2013), 6 -> ("Belle", 2021))

  /** One scenario: the venues' programmes, how the resolver groups listings and matches each group, and the tick. */
  private final class Scenario(rng: Random) {
    val programme = scala.collection.mutable.Map.empty[Cinema, Vector[CinemaMovie]]
    private val defaultGroup = scala.collection.mutable.Map.empty[String, Int]
    val groupOf  = scala.collection.mutable.Map.empty[ListingKey, Int]
    val filmOf   = scala.collection.mutable.Map.empty[Int, Option[Int]]
    var tick     = 0
    /** Whether TMDB fails one film in four (by film and tick); a film whose details failed is drafted again each tick. */
    var failing  = true
    private var groups = 0

    def newGroup(): Int = { groups += 1; filmOf(groups) = pickFilm(); groups }
    def pickFilm(): Option[Int] = if (rng.nextInt(3) == 0) None else Some(1 + rng.nextInt(6))
    def group(l: Listing): Int = groupOf.getOrElse(l.key,
      defaultGroup.getOrElseUpdate(s"${normalizer.sanitize(l.cleanTitle)}|${l.year}|${l.directors.mkString}", newGroup()))

    /** The resolver: one decision per group. */
    val resolve: (() => Seq[Listing]) => Option[IdentityProjection.Resolved] = read => {
      val listings = read()
      val decisions = listings.distinctBy(_.key).groupBy(group).toSeq.map { case (g, ls) =>
        val film = filmOf(g)
        ResolverDecision(ls.map(_.key).sorted, film, 0.9,
          if (film.isDefined) ResolverDecision.Basis.OwnMatch else ResolverDecision.Basis.NoCandidate, Nil)()
      }.sortBy(_.members.head)(using ListingKey.ordering)
      Some(IdentityProjection.Resolved(Resolution(decisions, listings.size, decisions.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap,
        Nil, Nil, 0, 0, 0, 0, 0, Map.empty), listings.map(_.key).toSet))
    }

    /** TMDB's details, failing for one film in four on a given tick — fetched again by a later projection. */
    val details: (MovieRecord, Int) => Option[MovieRecord] = (record, film) =>
      Option.unless(failing && (film + tick) % 4 == 0)(tmdb(film)).map { case (title, year) =>
        record.copy(data = record.data + (Tmdb -> SourceData(title = Some(title), releaseYear = Some(year))))
      }

    def listing(cinema: Cinema): CinemaMovie = {
      val (title, year, director) = titles(rng.nextInt(titles.size))
      val page  = Option.when(rng.nextBoolean())(s"https://${cinema.pillName.toLowerCase}/${normalizer.sanitize(title)}-${year.getOrElse(0)}")
      val hours = (0 until 1 + rng.nextInt(4)).map(_ => rng.nextInt(24 * 6)).distinct.sorted
      CinemaMovie(Movie(title, releaseYear = year), cinema, None, page, None, Nil, director.toSeq, hours.map(h => Showtime(start.plusHours(h.toLong), None)))
    }
  }

  // Production's storage shape: showtimes in `screenings`, venue slots in `movie_slots`.
  private def world(s: Scenario, scopedBetweenWhole: Int) =
    new ProjectionWorld(new InMemoryMovieRepository(screenings = Some(new services.movies.InMemoryScreeningsRepository),
      slots = Some(new services.movies.InMemorySlotsRepository), normalizer = normalizer), venues, clock, s.resolve, s.details,
      scopedBetweenWhole = scopedBetweenWhole)

  /** Everything a world stores: every film's id, key, title, year and record, and every slot's showtimes. */
  private def stored(w: ProjectionWorld) = w.repository.findAll().sortBy(_.id.value).map { r =>
    (r.id, r.key(normalizer), r.title, r.year, r.record,
      r.record.data.toSeq.collect { case (s: CinemaShowing, sd) => s"${s.cinema.displayName}|${s.titleKey}" -> sd.showtimes.map(_.dateTime) }.sortBy(_._1))
  }

  private def render(film: (services.movies.FilmId, String, String, Option[Int], MovieRecord, Seq[(String, Seq[LocalDateTime])])): String = {
    val (id, key, title, year, record, slots) = film
    s"$id $key '$title' $year tmdb=${record.tmdbId} attempt=${record.tmdbAttempt.isDefined} imdb=${record.imdbRating} " +
      s"sources=${record.data.keys.map(_.toString).toSeq.sorted.mkString(",")} slots=${slots.map { case (k, ts) => s"$k:${ts.size}" }.mkString(",")}"
  }

  /** The index the projection kept, moved where its inputs moved, against the index built afresh from the corpus as it
   *  now stands — the store with this tick's writes, the listings, the decisions: entry for entry. */
  private def indexAsBuilt(w: ProjectionWorld, s: Scenario): Unit = {
    val counters = FilmIdCounters.of(w.filmIds.allChecked().required).toOption.get
    val kept     = w.projection.keptIndex(counters)
    val listings = w.intake.projected(venues)
    val resolved = s.resolve(() => listings.map(_.listing)).get
    val built    = IdentityProjectionPlan.index(listings.filter(l => resolved.listings(l.listing.key)), resolved.resolution,
      w.cache.snapshot(), counters, normalizer)
    withClue("the kept index's listings: ")(kept.byKey shouldBe built.byKey)
    withClue("the kept index's stored films: ")(kept.storedById.keySet shouldBe built.storedById.keySet)
    kept.storedById.foreach { case (id, r) =>
      val b = built.storedById(id)
      withClue(s"the kept index's stored film $id: ")((r.key(normalizer), services.movies.ShowtimesDigest.leanEqual(r.record, b.record)) shouldBe
        ((b.key(normalizer), true)))
    }
    withClue("the kept index's previous films: ")(kept.previousOf shouldBe built.previousOf)
    withClue("the kept index's films' listings: ")(kept.listingsOf shouldBe built.listingsOf)
    withClue("the kept index's clusters: ")(kept.clusters shouldBe built.clusters)
    withClue("the kept index's listings' clusters: ")(kept.clusterOf shouldBe built.clusterOf)
    withClue("the kept index's FilmId map: ")((kept.covered.entries, kept.additions) shouldBe ((built.covered.entries, built.additions)))
  }

  private def run(seed: Int, ticks: Int): Unit = {
    val rng = new Random(seed)
    val s   = new Scenario(rng)
    var scoped = world(s, IdentityProjection.ScopedBetweenWhole)
    var whole  = world(s, 0)
    def scrape(cinemas: Iterable[Cinema]): Unit = {
      val programmes = cinemas.map(c => c -> s.programme.getOrElse(c, Vector.empty)).toMap
      scoped.scrape(programmes); whole.scrape(programmes)
    }
    venues.foreach(c => s.programme(c) = Vector.fill(1 + rng.nextInt(4))(s.listing(c)))
    scrape(venues)
    (1 to ticks).foreach { t =>
      s.tick = t
      val touched = scala.collection.mutable.Set.empty[Cinema]
      val events  = scala.collection.mutable.ListBuffer.empty[String]
      (0 until 1 + rng.nextInt(3)).foreach { _ =>
        val cinema = venues(rng.nextInt(venues.size))
        val shown  = s.programme.getOrElse(cinema, Vector.empty)
        val op = rng.nextInt(11)
        events += s"op $op at ${cinema.pillName}"
        op match {
          case 0 | 1 => s.programme(cinema) = shown :+ s.listing(cinema); touched += cinema
          case 2 if shown.nonEmpty => s.programme(cinema) = shown.patch(rng.nextInt(shown.size), Nil, 1); touched += cinema
          case 3 if shown.nonEmpty =>
            val i = rng.nextInt(shown.size)
            s.programme(cinema) = shown.updated(i, shown(i).copy(showtimes = s.listing(cinema).showtimes)); touched += cinema
          case 4 if shown.nonEmpty =>
            // The resolver moves one listing: into another group (a merge) or a group of its own (a split).
            val key = Listing.of(cinema, shown(rng.nextInt(shown.size)), normalizer).key
            s.groupOf(key) = if (rng.nextBoolean() && s.filmOf.nonEmpty) s.filmOf.keys.toSeq.sorted.apply(rng.nextInt(s.filmOf.size)) else s.newGroup()
          case 5 if s.filmOf.nonEmpty =>
            // The resolver matches a group to another film, or to none.
            val g = s.filmOf.keys.toSeq.sorted.apply(rng.nextInt(s.filmOf.size)); s.filmOf(g) = s.pickFilm()
          case 6 =>
            // Another writer changes a stored film — each change drawn once, so both worlds take the same write: one a whole
            // projection keeps (a rating, a chain's network slot), or one it puts back (detail pending again, a venue slot's
            // year or director — which can move a same-title listing to the other film — TMDB's title, TMDB's details gone).
            val films = scoped.repository.findAll().sortBy(_.id.value)
            if (films.nonEmpty) {
              val film   = films(rng.nextInt(films.size))
              val key    = CacheKey.stored(film.title, film.key(normalizer))
              val rating = 5.0 + rng.nextInt(40) / 10.0
              val (other, otherYear, otherDirector) = titles(rng.nextInt(titles.size))
              val change: MovieRecord => MovieRecord = rng.nextInt(6) match {
                case 0 => _.copy(imdbRating = Some(rating))
                case 1 => r => r.copy(data = r.data + (CinemaCityChain -> SourceData(synopsis = Some(s"Network $t"))))
                case 2 => _.copy(rottenTomatoes = Some((rating * 10).toInt))
                case 3 => r => r.copy(data = r.data.map {
                  case (venue: CinemaShowing, slot) => venue -> slot.copy(releaseYear = otherYear, director = otherDirector.toSeq)
                  case other => other
                })
                case 4 => r => r.copy(data = r.data.updatedWith(Tmdb)(_.map(_.copy(title = Some(other)))))
                case _ => r => r.copy(data = r.data - Tmdb)
              }
              scoped.cache.putIfPresent(key, change); whole.cache.putIfPresent(key, change)
            }
          case 9 =>
            // Another writer stores a film of its own — over a listing a projected film holds, or over none — or deletes one.
            val films = scoped.repository.findAll().sortBy(_.id.value)
            rng.nextInt(3) match {
              case 0 if films.nonEmpty =>
                val gone = films(rng.nextInt(films.size)).id
                scoped.cache.retireProjected(gone); whole.cache.retireProjected(gone)
              case n =>
                val shown = s.programme.getOrElse(cinema, Vector.empty)
                val row   = if (n == 1 && shown.nonEmpty) shown(rng.nextInt(shown.size)) else s.listing(cinema)
                val title = row.movie.title
                val stray = MovieRecord(data = Map(CinemaShowing.keyFor(cinema, title, normalizer) ->
                  SourceData(title = Some(title), rawTitle = Some(title), releaseYear = row.movie.releaseYear, director = row.director,
                    filmUrl = row.filmUrl, showtimes = row.showtimes)))
                val key = CacheKey(s"$title stray $t", row.movie.releaseYear, normalizer)
                scoped.cache.put(key, stray); whole.cache.put(key, stray)
            }
          case 7 if rng.nextInt(3) == 0 => scoped = scoped.restarted; whole = whole.restarted
          case 8 if rng.nextInt(4) == 0 => s.programme(cinema) = Vector.empty; touched += cinema
          case _ => ()
        }
      }
      scrape(touched)
      scoped.announced.clear(); whole.announced.clear()
      // The scoped world projects as the model takes its scrapes in about half the time (`tickChanged`), on the period
      // otherwise — and on the period whenever none has run yet.
      val a = Option.when(rng.nextBoolean())(scoped.projection.tickChanged()).flatten.getOrElse(scoped.projection.tick())
      val b = whole.projection.tick()
      withClue(s"seed $seed, tick $t (scoped: ${a.scoped}, refused: ${a.refused}; ${events.mkString(", ")}): ") {
        a.refused shouldBe b.refused
        val (mine, theirs) = (stored(scoped), stored(whole))
        if (mine != theirs) fail(s"the stores differ —\n  scoped only: ${mine.diff(theirs).map(render).mkString("\n    ")}\n  whole only: " +
          theirs.diff(mine).map(render).mkString("\n    "))
        scoped.filmIds.allChecked().required.sortBy(_.counter) shouldBe whole.filmIds.allChecked().required.sortBy(_.counter)
        scoped.announced.map(_.toString).sorted shouldBe whole.announced.map(_.toString).sorted
        scoped.reported shouldBe whole.reported   // the films and the canary they report
        if (a.refused.isEmpty) indexAsBuilt(scoped, s)
        whole.drifts.filter(_ != 0) shouldBe empty
        scoped.drifts.filter(_ != 0) shouldBe empty
      }
    }
  }

  "A projection of what moved" should "store, tick after tick, exactly what a projection of the whole corpus stores" in {
    (1 to 400).foreach(seed => run(seed, ticks = 24))
  }

  // Two unmatched films of one title and year, the older under the plain key: another writer deletes it as its venue
  // drops the listing — nothing now leads to the younger film but the key the deleted one held.
  it should "give a key a deleted film held to the film waiting for it, though nothing else of either moved" in {
    val s = new Scenario(new Random(1))
    s.failing = false
    val older   = CinemaMovie(Movie("Obcy", releaseYear = Some(1979)), Rialto, None, None, None, Nil, Nil, Seq(Showtime(start, None)))
    val younger = older.copy(cinema = KinoMuza)
    s.programme(Rialto) = Vector(older)
    val (scoped, whole) = (world(s, IdentityProjection.ScopedBetweenWhole), world(s, 0))
    def both[A](f: ProjectionWorld => A): (A, A) = (f(scoped), f(whole))
    both(_.scrape(Map(Rialto -> Vector(older))))
    both(_.projection.tick())
    // Its own group, so the younger stays a film of its own under the variant key.
    s.groupOf(Listing.of(KinoMuza, younger, normalizer).key) = s.newGroup()
    s.filmOf(s.groupOf(Listing.of(KinoMuza, younger, normalizer).key)) = None
    s.programme(KinoMuza) = Vector(younger)
    both(_.scrape(Map(KinoMuza -> Vector(younger))))
    both(_.projection.tick())
    both(_.projection.tick())
    scoped.repository.findAll().map(_.key(normalizer)).sorted should have size 2
    val holder = scoped.repository.findAll().find(_.key(normalizer) == "obcy|1979").get.id
    // The venue drops the older film for another (an empty scrape would be held by the scrape guard).
    val instead = older.copy(movie = Movie("Diuna", releaseYear = Some(2021)))
    s.programme(Rialto) = Vector(instead)
    both(w => { w.cache.retireProjected(holder); w.scrape(Map(Rialto -> Vector(instead))) })
    val (a, _) = both(_.projection.tick())
    a.scoped shouldBe true
    stored(scoped) shouldBe stored(whole)
    scoped.repository.findAll().filter(_.title == "Obcy").map(_.key(normalizer)) shouldBe Seq("obcy|1979")
  }

  it should "project only the films its changes reach, not the whole corpus" in {
    val s = new Scenario(new Random(7))
    s.failing = false
    val w = world(s, IdentityProjection.ScopedBetweenWhole)
    venues.foreach(c => s.programme(c) = Vector.fill(4)(s.listing(c)))
    w.scrape(s.programme.toMap)
    w.projection.tick().scoped shouldBe false
    w.projection.tick()
    val idle = w.projection.tick()
    idle.scoped shouldBe true
    idle.wroteNothing shouldBe true
    idle.slotsReused + idle.slotsBuilt shouldBe 0   // nothing moved: no film drafted
    s.programme(Rialto) = s.programme(Rialto) :+ s.listing(Rialto)
    w.scrape(Map(Rialto -> s.programme(Rialto)))
    val moved = w.projection.tick()
    moved.scoped shouldBe true
    (moved.slotsReused + moved.slotsBuilt) should be < w.repository.findAll().map(_.record.cinemaSlotCount).sum
  }

  it should "project the whole corpus again after every ScopedBetweenWhole scoped projections" in {
    val s = new Scenario(new Random(3))
    val w = world(s, 2)
    venues.foreach(c => s.programme(c) = Vector.fill(2)(s.listing(c)))
    w.scrape(s.programme.toMap)
    (1 to 7).map(_ => w.projection.tick().scoped) shouldBe Seq(false, true, true, false, true, true, false)
    w.drifts shouldBe Seq(0, 0)
    w.reconciles shouldBe Seq(true, true)   // the first, with nothing before it, is the boot's, not a reconcile
  }

  it should "project on scrapes once the boot's projection ran, and count that one as no reconcile" in {
    // The boot path (ResolutionWiring.bootProjectionTick) is projectQuietly: through tick(), which lets every projection
    // the model's scrapes ask for run — any other way in, and each would wait for the first forever (9b8d0aacc's outage).
    val s = new Scenario(new Random(9))
    val w = world(s, IdentityProjection.ScopedBetweenWhole)
    venues.foreach(c => s.programme(c) = Vector.fill(2)(s.listing(c)))
    w.scrape(s.programme.toMap)
    w.projection.tickChanged() shouldBe empty        // nothing before the boot's
    w.projection.projectQuietly() shouldBe true
    w.projection.tickChanged() shouldBe defined
    w.reconciles shouldBe empty
  }

  it should "count an hourly reconcile with no projection before it as unmeasured, and measure the next one's drift" in {
    // Prod 2026-10-04: every worker restarted within the hour, and its only whole projection was the boot's first —
    // no projection before it, so no scope to check: drift was never measured, and no series said so.
    val s = new Scenario(new Random(5))
    val w = world(s, IdentityProjection.ScopedBetweenWhole)
    venues.foreach(c => s.programme(c) = Vector.fill(2)(s.listing(c)))
    w.scrape(s.programme.toMap)
    w.projection.tick(whole = true)
    (w.reconciles.toSeq, w.drifts.toSeq) shouldBe ((Seq(false), Nil))
    w.projection.tick(whole = true)
    (w.reconciles.toSeq, w.drifts.toSeq) shouldBe ((Seq(false, true), Seq(0)))
  }

  it should "project as the model takes the scrapes in only once the period has projected, and never count toward the reconcile" in {
    // A worker's first projection is of the whole corpus, on the period after boot; one run on scrapes waits for it. Run
    // on scrapes, a projection reads only the venues the intake took (no stamps), records no slot fingerprints, and leaves
    // the hour between two whole projections to be counted by the periodic ones.
    val s = new Scenario(new Random(3))
    val w = world(s, 2)
    venues.foreach(c => s.programme(c) = Vector.fill(2)(s.listing(c)))
    w.scrape(s.programme.toMap)
    w.projection.tickChanged() shouldBe None
    w.projection.tick().scoped shouldBe false
    val recorded = w.fingerprints.all()
    (1 to 3).foreach { i =>
      s.programme(Rialto) = s.programme(Rialto) :+ s.listing(Rialto)
      w.scrape(Map(Rialto -> s.programme(Rialto)))
      val changed = w.projection.tickChanged().get
      withClue(s"run $i: ")(changed.scoped shouldBe true)
    }
    w.fingerprints.all() shouldBe recorded
    (1 to 3).map(_ => w.projection.tick().scoped) shouldBe Seq(true, true, false)
    w.drifts shouldBe Seq(0)
  }

  it should "not count a projection settled while a written film lacks its TMDB details, nor one that failed" in {
    // No period comes back for them: the trigger tries an unsettled projection again (`ProjectionTrigger.retry`).
    val s = new Scenario(new Random(3))
    var broken = false
    val w = new ProjectionWorld(new InMemoryMovieRepository(screenings = Some(new services.movies.InMemoryScreeningsRepository),
      slots = Some(new services.movies.InMemorySlotsRepository), normalizer = normalizer), venues, clock,
      read => if (broken) throw new IllegalStateException("model down") else s.resolve(read), details = (_, _) => None)
    venues.foreach(c => s.programme(c) = Vector.fill(3)(s.listing(c)))
    w.scrape(s.programme.toMap)
    val first = w.projection.tick()
    first.changed.exists(_.record.tmdbId.isDefined) shouldBe true
    w.projection.settled(first) shouldBe false
    broken = true
    w.projection.tickChangedQuietly() shouldBe false
    broken = false
    // With nothing written to build on, the next projection on scrapes reads and projects the whole corpus.
    w.projection.tickChanged().map(_.scoped) shouldBe Some(false)
    w.projection.tickChanged().map(_.scoped) shouldBe Some(true)
  }
}
