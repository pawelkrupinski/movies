package services.identity

import models.{Cinema, CinemaMovie, CinemaShowing, Country, Helios, KinoApollo, KinoMuza, Movie, MovieRecord, Multikino, Rialto, Showtime, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CinemaSlotBuilder, FilmId, ListingKey, ScreeningTokens, SingleCountryNormalizer, StoredMovieRecord, StringPool}

import java.time.{Instant, LocalDateTime}

/**
 * The identity projection's pure decisions over hand-built decisions: which film keeps which id,
 * what each film's record holds, and that a second projection over the first's output writes
 * nothing. The resolver itself is exercised elsewhere; here its decisions are given, so each case
 * pins exactly one rule of the plan.
 */
class IdentityProjectionPlanSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val slots      = new CinemaSlotBuilder(Country.Poland.language, new StringPool)
  private val tokens     = ScreeningTokens.of(Country.Poland)
  private val at         = Instant.parse("2026-09-26T12:00:00Z")
  private val start      = LocalDateTime.of(2026, 9, 27, 18, 0)

  private def row(cinema: Cinema, title: String, year: Option[Int] = None, director: Option[String] = None,
                  page: Option[String] = None, hours: Seq[Int] = Seq(0, 24)): ProjectedListing = {
    val cm = CinemaMovie(Movie(title, releaseYear = year), cinema, None, page, None, Nil, director.toSeq,
      hours.map(h => Showtime(start.plusHours(h.toLong), None)))
    ProjectedListing(Listing.of(cinema, cm, normalizer), cm)
  }

  /** The slot a stored film holds for `l`, as the landing wrote it. */
  private def slotOf(l: ProjectedListing): (models.Source, SourceData) =
    CinemaShowing.keyFor(l.listing.cinema, l.listing.cleanTitle, normalizer) ->
      SourceData(title = Some(l.listing.cleanTitle), rawTitle = Some(l.listing.rawTitle), releaseYear = l.listing.year,
        director = l.listing.directors, filmUrl = l.listing.page)

  private def decision(film: Option[Int], members: ProjectedListing*): ResolverDecision =
    ResolverDecision(members.map(_.listing.key).sorted, film, 0.9,
      if (film.isDefined) ResolverDecision.Basis.OwnMatch else ResolverDecision.Basis.NoCandidate, Nil)

  private def resolution(decisions: ResolverDecision*): Resolution = {
    val ds = decisions.sortBy(_.members.head)(using ListingKey.ordering)
    Resolution(ds, ds.size, ds.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
  }

  /** A film's TMDB details, as the projection's details fetch would add them. */
  private def detailed(film: ProjectedFilm, title: String, year: Int): ProjectedFilm =
    film.copy(record = film.record.copy(data = film.record.data + (Tmdb -> SourceData(title = Some(title), releaseYear = Some(year)))))

  private def plan(listings: Seq[ProjectedListing], r: Resolution, stored: Seq[StoredMovieRecord] = Nil,
                   counters: FilmIdCounters = FilmIdCounters.empty): ProjectionPlan = {
    val d = IdentityProjectionPlan.draft(listings, r, stored, counters, normalizer, slots, tokens, at)
    IdentityProjectionPlan.finish(d, normalizer, id => stored.exists(_.id == id))
  }

  private def storedOf(p: ProjectionPlan): Seq[StoredMovieRecord] =
    p.films.map(f => StoredMovieRecord(f.title, f.year, f.record, f.id, Some(f.key)))

  private def counters(p: ProjectionPlan): FilmIdCounters = FilmIdCounters.of(p.counterAdditions).toOption.get

  private val lalka = Seq(row(Multikino, "Lalka", Some(2026), Some("Maciej Kawalski")), row(Helios, "Lalka", Some(2026)),
    row(KinoApollo, "Lalka 2D PL", page = Some("https://apollo/lalka")))
  private val obcy  = Seq(row(Rialto, "Obcy", Some(1979)), row(KinoMuza, "Obcy", Some(1979), hours = Seq(3)))

  "A first projection" should "make one film per cluster, each under its own key, keeping every listing's showtimes" in {
    val p = plan(lalka ++ obcy, resolution(decision(Some(1), lalka*), decision(None, obcy*)))
    p.films.map(_.members.toSet) should contain theSameElementsAs Seq(lalka.map(_.listing.key).toSet, obcy.map(_.listing.key).toSet)
    p.films.map(_.key).distinct.size shouldBe 2
    p.retired shouldBe empty
    p.regroupings shouldBe Regroupings(0, 0, 0, 2, 0)
    p.counterAdditions.map(_.counter).sorted shouldBe Seq(1L, 2L)
    val (matched, none) = p.films.partition(_.record.tmdbId.isDefined)
    matched.head.record.tmdbId shouldBe Some(1)
    none.head.record.tmdbAttempt.map(_.evidence) shouldBe Some(IdentityProjectionPlan.ResolverVerdict)
    none.head.record.readyToProject shouldBe true
    // P4: every listing's showtimes are on its film, at its venue.
    (lalka ++ obcy).foreach { l =>
      val film = p.films.find(_.members.contains(l.listing.key)).get
      val atVenue = film.record.data.collect { case (CinemaShowing(c, _), sd) if c == l.listing.cinema => sd.showtimes }.flatten
      atVenue.map(_.dateTime) should contain allElementsOf l.row.showtimes.map(_.dateTime)
    }
  }

  "The films a first projection wrote" should "project to themselves: a second projection changes no id, key or record" in {
    val r     = resolution(decision(Some(1), lalka*), decision(None, obcy*))
    val first = plan(lalka ++ obcy, r)
    val withDetails = first.copy(films = first.films.map(f => if (f.record.tmdbId.isDefined) detailed(f, "Lalka", 2026) else f))
    val second = plan(lalka.reverse ++ obcy, r, storedOf(withDetails), counters(first))
    second.films.map(f => (f.id, f.key, f.record)) should contain theSameElementsAs withDetails.films.map(f => (f.id, f.key, f.record))
    second.regroupings.isEmpty shouldBe true
    second.counterAdditions shouldBe empty
    second.canary(ShadowRelation.Identical) shouldBe 2
  }

  "A stored film" should "keep its legacy id, its ratings and its TMDB slot when the resolver keeps its listings on the same film" in {
    val legacy = StoredMovieRecord("Lalka", Some(2026), MovieRecord(tmdbId = Some(1), imdbRating = Some(7.1),
      data = Map(Tmdb -> SourceData(title = Some("Lalka")), slotOf(lalka.head))),
      FilmId("lalka|2026"), Some("lalka|2026"))
    val p = plan(lalka, resolution(decision(Some(1), lalka*)), Seq(legacy))
    p.films.map(_.id) shouldBe Seq(FilmId("lalka|2026"))
    p.films.head.record.imdbRating shouldBe Some(7.1)
    p.films.head.record.data.keySet should contain(Tmdb)
    p.regroupings shouldBe Regroupings(0, 0, 0, 0, 0)
    p.counterAdditions shouldBe Seq(FilmIdCounter("lalka|2026", 1))
  }

  it should "lose the old film's ratings and details when the resolver names another film" in {
    val stored = StoredMovieRecord("Lalka", Some(1968), MovieRecord(tmdbId = Some(2), imdbRating = Some(8.0),
      data = Map(Tmdb -> SourceData(title = Some("Lalka")), slotOf(lalka.head))),
      FilmId("lalka|1968"), Some("lalka|1968"))
    val d = IdentityProjectionPlan.draft(lalka, resolution(decision(Some(1), lalka*)), Seq(stored), FilmIdCounters.empty,
      normalizer, slots, tokens, at)
    d.drafts.map(_.inherited) shouldBe Seq(Some(FilmId("lalka|1968")))
    d.drafts.head.record.imdbRating shouldBe None
    d.drafts.head.needsDetails shouldBe Some(1)
  }

  "Two stored films the resolver joins" should "keep the OLDER id, retire the other and count one merge" in {
    val older = StoredMovieRecord("Lalka", Some(2026), MovieRecord(tmdbId = Some(1),
      data = Map(slotOf(lalka.head))),
      FilmId("f-older"), Some("lalka|2026"))
    val newer = StoredMovieRecord("Lalka 2D PL", None, MovieRecord(tmdbAttempt = Some(services.resolution.TmdbAttempt.Legacy),
      data = Map(slotOf(lalka(1)), slotOf(lalka(2)))),
      FilmId("f-newer"), Some("lalka2dpl|"))
    val map = FilmIdCounters.of(Seq(FilmIdCounter("f-older", 1), FilmIdCounter("f-newer", 2))).toOption.get
    val p = plan(lalka, resolution(decision(Some(1), lalka*)), Seq(newer, older), map)
    p.films.map(_.id) shouldBe Seq(FilmId("f-older"))
    p.retired shouldBe Seq(FilmId("f-newer"))
    p.regroupings.merges shouldBe 1
    p.regroupings.retired shouldBe 1
    p.regroupings.moves shouldBe 2
  }

  "A stored film the resolver splits" should "stay on the larger half, the smaller getting a fresh id" in {
    val star1954 = Seq(row(Rialto, "Narodziny gwiazdy", Some(1954), Some("George Cukor")))
    val star2018 = Seq(row(Multikino, "Narodziny gwiazdy", Some(2018)), row(Helios, "Narodziny gwiazdy", Some(2018)))
    val mixed = StoredMovieRecord("Narodziny gwiazdy", Some(2018), MovieRecord(tmdbId = Some(2018),
      data = (star1954 ++ star2018).map(slotOf).toMap),
      FilmId("narodzinygwiazdy|2018"), Some("narodzinygwiazdy|2018"))
    val p = plan(star1954 ++ star2018, resolution(decision(Some(2018), star2018*), decision(Some(1954), star1954*)), Seq(mixed))
    p.films.find(_.record.tmdbId.contains(2018)).get.id shouldBe FilmId("narodzinygwiazdy|2018")
    p.films.find(_.record.tmdbId.contains(1954)).get.id.value should startWith("f")
    p.regroupings.splits shouldBe 1
    p.regroupings.fresh shouldBe 1
    p.films.map(_.key).distinct.size shouldBe 2
  }

  "Two clusters the resolver matched to ONE film" should "be stored as one film: `movies` holds one document per TMDB id" in {
    val p = plan(lalka, resolution(decision(Some(1), lalka.take(2)*), decision(Some(1), lalka.drop(2)*)))
    p.films.map(_.members.toSet) shouldBe Seq(lalka.map(_.listing.key).toSet)
  }

  "Two films one title and year name" should "be stored under two keys, the older plain" in {
    val a = Seq(row(Multikino, "Lalka", Some(2026), Some("Maciej Kawalski")), row(Helios, "Lalka", Some(2026), Some("Maciej Kawalski")))
    val b = Seq(row(Rialto, "Lalka", Some(2026), Some("Someone Else")))
    val p = plan(a ++ b, resolution(decision(None, a*), decision(None, b*)))
    p.films.map(_.key).toSet shouldBe Set("lalka|2026", s"lalka~${p.films.map(_.counter).max}|2026")
    p.films.find(_.key == "lalka|2026").get.members.toSet shouldBe a.map(_.listing.key).toSet
  }

  "A venue printing one listing twice" should "keep both rows' showtimes on the film (P4)" in {
    val twice = Seq(row(Multikino, "Lalka", Some(2026), hours = Seq(0)), row(Multikino, "Lalka", Some(2026), hours = Seq(48)))
    twice.map(_.listing.key).distinct.size shouldBe 1
    val p = plan(twice, resolution(decision(Some(1), twice.head)))
    p.films.head.record.data.values.flatMap(_.showtimes).map(_.dateTime).toSet shouldBe Set(start, start.plusHours(48))
  }

  "The plan" should "not depend on the order the listings, decisions or stored films arrive in (P1)" in {
    val r = resolution(decision(Some(1), lalka*), decision(None, obcy*))
    val once = plan(lalka ++ obcy, r)
    val rng = new scala.util.Random(2026)
    (1 to 10).foreach { _ =>
      val shuffled = Resolution(rng.shuffle(r.decisions), r.nodes, r.familyOf, r.edges, r.queries, 0, 0, 0, 0, 0, Map.empty)
      plan(rng.shuffle(lalka ++ obcy), shuffled) shouldBe once
    }
  }
}
