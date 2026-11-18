package services.identity
package adapters

import clients.TmdbClient
import models.{KinoMuza, Kinoteka}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** An operator's "these TMDB films were just edited" (the `tmdb-films` source of a `RefreshSource` task): each film's record fetched again into
 *  the model's store, then the questions of exactly the families the films reach — a decision naming the film, or a
 *  listing billing a title the fetched record now carries — asked again, every moved answer filed and announced to the
 *  model; nothing else asked. */
class TmdbFilmRefreshSpec extends AnyFlatSpec with Matchers {
  import TmdbChangesSweepSpec.Tmdb

  private val ghana = 1525208   // "The Eyes of Ghana", given its Polish title "Oczy Ghany" on TMDB
  private val heat  = 1658185   // given "Żar"; a family already leaning to it

  /** TMDB after the edit: the films' records carry their Polish titles, and a search for one finds its film. */
  private final class Edited extends Tmdb {
    def answer(path: String, url: String): String =
      if (path == s"/3/movie/$ghana") s"""{"id":$ghana,"title":"Oczy Ghany","original_title":"The Eyes of Ghana","credits":{"crew":[]},"alternative_titles":{"titles":[]}}"""
      else if (path == s"/3/movie/$heat") s"""{"id":$heat,"title":"Żar","original_title":"Heat","credits":{"crew":[]},"alternative_titles":{"titles":[]}}"""
      else if (path == "/3/search/movie" && url.contains("Oczy"))
        s"""{"results":[{"id":$ghana,"title":"Oczy Ghany","original_title":"The Eyes of Ghana","release_date":"2025-04-01","popularity":1.5}],"total_pages":1}"""
      else """{"results":[],"total_pages":1}"""
    def searched: Seq[String] = asked.toSeq.filter(_.contains("/3/search/movie")).map(u => java.net.URLDecoder.decode(u.split("query=")(1).takeWhile(_ != '&'), "UTF-8"))
  }

  private def listing(title: String, venue: models.Cinema) =
    Listing(venue, services.movies.ListingKey.Published(venue.displayName, title, None, Nil), title, title, title, None, Nil, None, None, None)
  private def unmatched(l: Listing) = ResolverDecision(Seq(l.key), None, 0.0, ResolverDecision.Basis.OwnMatch, Nil)()

  private val oczy  = listing("Oczy Ghany", Kinoteka)
  private val zar   = listing("Zar", KinoMuza)
  private val lalka = listing("Lalka", KinoMuza)
  private val banner = listing("AfryKamera: Oczy Ghany", KinoMuza)   // billed under a banner, searched as its peeled title
  private val questions = Seq(
    Set[CandidateQuery](CandidateQuery.Title("Oczy Ghany"))     -> Seq(unmatched(oczy)),
    Set[CandidateQuery](CandidateQuery.Title("Zar"))            -> Seq(ResolverDecision(Seq(zar.key), None, 0.0, ResolverDecision.Basis.OwnMatch, Nil, leaning = Some(ResolverDecision.Leaning(heat, 0)))()),
    Set[CandidateQuery](CandidateQuery.Title("Lalka"))          -> Seq(unmatched(lalka)),
    Set[CandidateQuery](CandidateQuery.Title("AfryKamera: Oczy Ghany"), CandidateQuery.Title("OCZY GHANY")) -> Seq(unmatched(banner)))

  private final class World(modelUp: Boolean = true) {
    val tmdb    = new Edited
    val store   = new TmdbStore(new InMemoryTmdbDocuments, tools.SpecClock.Pinned)
    val changed = mutable.Buffer.empty[String]
    store.onChanged(changed += _)
    val refresh = new TmdbFilmRefresh(new NormalizingHttpFetch(tmdb, new TmdbNormalizer(store)),
      TmdbFilmRefresh.liveLookups(new TmdbClient(_, apiKey = Some(settings.TmdbApiKey("k")))),
      families = () => Option.when(modelUp)(TmdbFilmRefresh.Family.of(questions, Seq(oczy, zar, lalka, banner))),
      fold = services.movies.TitleNormalizer.forCountry(models.Country.Poland).sanitize)
    def filed(query: CandidateQuery): Boolean = store.get(TmdbKind.Query, Seq(TmdbStore.questionId("pl-PL", query))).nonEmpty
  }

  "a refresh" should "fetch the films' records, then ask again the questions of the families they reach — and only those" in {
    val w = new World
    val r = w.refresh.refreshFilms(Seq(ghana, heat, ghana))
    w.tmdb.asked.map(u => new java.net.URI(u).getPath).filter(_.startsWith("/3/movie/")).toSet shouldBe Set(s"/3/movie/$ghana", s"/3/movie/$heat")
    // "Oczy Ghany" bills the record's new Polish title; "Zar" leans to Heat; the banner's billing searches the title;
    // "Lalka" is reached by none
    w.tmdb.searched.toSet shouldBe Set("Oczy Ghany", "Zar", "AfryKamera: Oczy Ghany", "OCZY GHANY")
    r shouldBe TmdbFilmRefresh.Refreshed(films = 2, records = 2, modelUp = true, families = 3, questions = 4, answered = 4)
  }

  it should "file what moved, so the model re-resolves the families that read it" in {
    val w = new World
    w.refresh.refreshFilms(Seq(ghana))
    w.filed(CandidateQuery.Title("Oczy Ghany")) shouldBe true
    w.changed should contain(TmdbStore.keyOf(TmdbKind.Query, TmdbStore.questionId("pl-PL", CandidateQuery.Title("Oczy Ghany"))))
    w.changed should contain(TmdbStore.keyOf(TmdbKind.Film, ghana.toString))
  }

  it should "fetch the records but reach no family while the model is not taken up, and say so" in {
    val w = new World(modelUp = false)
    val r = w.refresh.refreshFilms(Seq(ghana))
    w.tmdb.searched shouldBe Nil
    r.modelUp shouldBe false
    r.records shouldBe 1
    r.message should include("no model taken up")
  }

  it should "be the tmdb-films source, its keys the films' ids" in {
    val w = new World
    w.refresh.name shouldBe services.tasks.RefreshableSources.TmdbFilms
    w.refresh.refresh(Seq(ghana.toString, "not-an-id")).message should startWith("1 of 1 records fetched; 2 families reached")
  }
}
