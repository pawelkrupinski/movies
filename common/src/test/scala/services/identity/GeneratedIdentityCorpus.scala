package services.identity

import models._
import services.movies.{ListingKey, TitleNormalizer}

import scala.util.Random

/**
 * Random corpora shaped like the ones that broke, with a synthetic film database honest to the
 * lookup contract (every answer a function of its argument alone):
 *
 *  - several films under one title (Belle 2013/2021, the "A Star Is Born"s), a numbered sequel
 *    beside its original, a title contained in another ("Faust" / "Zärtlich kreist die Faust");
 *  - programme banners and decorations ("Filmowy Klub Seniora - X", "Oficjalna premiera: X"),
 *    bracketed re-release years ("X (2026)"), shouted titles;
 *  - films the database has no entry for (a broadcast), venues publishing a random subset of
 *    year, director and runtime — some only on a detail page;
 *  - GAPS: a deterministic share of queries and film records the source cannot answer, as the
 *    recorded trees cannot.
 *
 * `truth` maps each listing key to the film it really is — a test label, never an input.
 */
final case class GeneratedIdentityCorpus(listings: Seq[Listing], lookups: IdentityLookups, truth: Map[ListingKey, Int])

object GeneratedIdentityCorpus {

  final case class Film(id: Int, title: String, year: Int, director: String, runtime: Int, tmdbId: Option[Int], popularity: Double)

  val Venues: Seq[Cinema] = Seq(Multikino, Helios, KinoApollo, KinoMuza, Rialto, CinemaCityKinepolis, CinemaCityPoznanPlaza)

  private val Bases = Seq("Lalka", "Belle", "Samson i Dalila", "Opętanie", "Faust", "Zärtlich kreist die Faust", "Matilda",
    "Pressure", "Toy Story", "It Ends", "It Ends with Us", "A Star Is Born", "Happy Together", "Digger")
  private val Directors = Seq("Wojciech Has", "Amma Asante", "Mamoru Hosoda", "Cecil B. DeMille", "Darko Tresnjak",
    "Andrzej Żuławski", "F.W. Murnau", "Danny DeVito", "Wong Kar Wai", "Justin Baldoni", "Alexander Ullom", "George Cukor")
  private val Years = Seq(1926, 1954, 1976, 1990, 2013, 2018, 2021, 2025, 2026)

  def generate(seed: Long, normalizer: TitleNormalizer, films: Int = 10, listings: Int = 40): GeneratedIdentityCorpus = {
    val rnd = new Random(seed)
    val universe = (1 to films).map { i =>
      val base  = Bases(rnd.nextInt(Bases.size))
      val title = if (rnd.nextDouble() < 0.12) s"$base 2" else base
      Film(i, title, Years(rnd.nextInt(Years.size)), Directors(rnd.nextInt(Directors.size)), 80 + rnd.nextInt(100),
        Option.when(rnd.nextDouble() < 0.85)(1000 + i), math.round(rnd.nextDouble() * 1000) / 10.0)
    }.distinctBy(f => (f.title, f.year))
    val detailVenues = Venues.zipWithIndex.collect { case (v, i) if i % 3 == 0 => v.displayName }.toSet
    val made = (1 to listings).map { _ =>
      val film  = universe(rnd.nextInt(universe.size))
      val venue = Venues(rnd.nextInt(Venues.size))
      val spelling = rnd.nextInt(7) match {
        case 0 => s"Filmowy Klub Seniora - ${film.title}"
        case 1 => s"${film.title} (${film.year})"
        case 2 => film.title.toUpperCase(java.util.Locale.ROOT)
        case 3 => s"${film.title} (2026)"
        case 4 => s"Oficjalna premiera: ${film.title}"
        case _ => film.title
      }
      val page      = Some(s"${venue.displayName}/${film.id}/${spelling.hashCode}")
      val year      = Option.when(rnd.nextDouble() < 0.4)(film.year)
      val directors = if (rnd.nextDouble() < 0.4 && !detailVenues(venue.displayName)) Seq(film.director) else Nil
      val runtime   = Option.when(rnd.nextDouble() < 0.4)(film.runtime)
      val key       = ListingKey.Native(venue.displayName, page.get, spelling)
      Listing(venue, key, spelling, spelling, spelling, year, directors, runtime, page, None) -> film
    }.distinctBy(_._1.key)
    val truth  = made.map { case (l, f) => l.key -> f.id }.toMap
    val byKey  = made.map { case (l, f) => l.key -> f }.toMap
    val lookups = new SyntheticLookups(universe, normalizer) {
      override def hasDetail(l: Listing): Boolean = detailVenues(l.venue) && l.page.isDefined
      override def detail(l: Listing): Answer[Option[DetailFacts]] =
        Answer.Known(byKey.get(l.key).map(f => DetailFacts(None, Seq(f.director), None, None)))
    }
    GeneratedIdentityCorpus(made.map(_._1), lookups, truth)
  }

  /** A film database over `universe`: TMDB's search (every film whose title holds all the query's
   *  words, most popular first), filmographies and records.
   *  One query in eleven and one record in seven are GAPS. */
  class SyntheticLookups(universe: Seq[Film], normalizer: TitleNormalizer) extends IdentityLookups {
    private val known = universe.filter(_.tmdbId.isDefined)
    private def words(s: String) = services.movies.TitleContainment.tokens(normalizer.searchQuery(s)).toSet
    private def hit(f: Film) = Hit(f.tmdbId.get, f.title, None, Some(f.year), f.popularity)
    override def hasDetail(l: Listing): Boolean = false
    override def detail(l: Listing): Answer[Option[DetailFacts]] = Answer.Known(None)
    override def candidates(q: CandidateQuery): Answer[Seq[Hit]] =
      if (math.abs(q.sortKey.hashCode) % 11 == 0) Answer.Unknown
      else q match {
        case CandidateQuery.Title(text) =>
          val want = words(text)
          Answer.Known(known.filter(f => want.nonEmpty && want.subsetOf(words(f.title)))
            .sortBy(f => (-f.popularity, f.tmdbId.get)).map(hit))
        case CandidateQuery.Director(name) =>
          Answer.Known(known.filter(_.director == name).sortBy(_.tmdbId.get).map(hit))
      }
    override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] =
      if (tmdbId % 7 == 0) Answer.Unknown
      else Answer.Known(known.find(_.tmdbId.contains(tmdbId)).map(f =>
        IdentityMeasures.Film(f.title, None, Nil, Some(f.year), Some(f.runtime), Some(Seq(f.director)), None, Some(f.popularity))))
  }
}
