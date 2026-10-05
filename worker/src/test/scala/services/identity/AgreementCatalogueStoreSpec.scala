package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.FlicksClient
import services.cinemas.pl.KinotekaClient
import services.enrichment.{LetterboxdClient, WikidataClient}
import services.tasks.{HandlerOutcome, InMemoryTaskQueue, Task, TaskType}
import tools.{MutableClock, RoutingHttpFetch}

import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger

/** The catalogue take's answers: the links a venue's film page states (recorded Flicks and Kinoteka pages), the ids
 *  Wikidata's SPARQL endpoint maps (recorded answers for DE Webedia ids and Letterboxd slugs), filed among the families'
 *  answers and read back, and asked for on the queue. */
class AgreementCatalogueStoreSpec extends AnyFlatSpec with Matchers {
  private val clock    = new MutableClock(Instant.parse("2026-10-05T00:00:00Z"))
  private val fixtures = Path.of("test", "resources", "fixtures")
  private def recorded(path: String) = Files.readString(fixtures.resolve(path))

  private val empire   = "https://www.flicks.co.uk/movie/empire-of-the-sun/"
  private val goldenBoy = "https://www.flicks.co.uk/movie/nt-live-golden-boy/"
  private val wongKarWai = "https://kinoteka.pl/film/2046/"
  private val pages = Seq(FlicksClient.CatalogueLinkPages, KinotekaClient.CatalogueLinkPages)

  private def venues = new RoutingHttpFetch(Seq(
    "/movie/empire-of-the-sun/" -> recorded("flicks/www.flicks.co.uk/movie/empire-of-the-sun.html"),
    "/movie/nt-live-golden-boy/" -> recorded("flicks/www.flicks.co.uk/movie/nt-live-golden-boy.html"),
    "/film/2046/"                -> recorded("kinoteka/kinoteka.pl/film/2046")), unroutedIsNotFound = true)
  private def reader(fetch: RoutingHttpFetch = venues) = new CatalogueLinkReader(pages.map(_ -> fetch))
  private def mapping(letterboxdPages: Seq[(String, String)] = Nil) = new WikidataCatalogueMapping(
    new WikidataClient(new clients.tools.UrlFragmentHttpFetch(Seq(
      "P1265" -> recorded("wikidata/sparql_webedia_items.json"), "P6127" -> recorded("wikidata/sparql_letterboxd_items.json")))),
    new LetterboxdClient(new RoutingHttpFetch(letterboxdPages, unroutedIsNotFound = true)))
  private def store() = new CatalogueAnswerStore(new FamilyAnswerStore(new InMemoryTmdbDocuments, clock), clock, pages)

  "a venue's film page" should "link the one film it names in each catalogue its client declares, none it links two of" in {
    reader().links(empire) shouldBe Seq(CatalogueId("letterboxd", "empire-of-the-sun"), CatalogueId("rt", "empire_of_the_sun"))
    reader().links(goldenBoy) shouldBe Nil
    reader().links(wongKarWai) shouldBe Seq(CatalogueId("imdb", "tt0212712"))
    CatalogueLinks.of("""<a href="https://www.imdb.com/title/tt1/">a</a> <a href="https://www.imdb.com/title/tt2/">b</a>""", Set("imdb")) shouldBe Nil
  }

  it should "link none when gone for good, and be no question at all on a host no client declares" in {
    reader().links("https://www.flicks.us/movie/gone/") shouldBe Nil
    val s = store()
    s.linked("https://www.kinoprogramm.com/film/x") shouldBe Answer.Known(Nil)
    s.linked(empire) shouldBe Answer.Unknown
  }

  "a catalogue id" should "map to the item Wikidata states it on, by which property, a Letterboxd slug on none by its own page" in {
    mapping().map(Seq(CatalogueId("webedia", "325279"), CatalogueId("webedia", "1000032825"), CatalogueId("webedia", "144766"))) shouldBe Map(
      CatalogueId("webedia", "325279")     -> Seq(CatalogueHit(Some("Q135441923"), Some(1318829), Some("tt36112899"), "Wikidata P1265")),
      CatalogueId("webedia", "1000032825") -> Seq(CatalogueHit(Some("Q138644118"), None, None, "Wikidata P8531")))
    val lb = mapping(Seq("/film/kinowo-no-such-slug/" -> recorded("letterboxd/film_inception.html")))
      .map(Seq(CatalogueId("letterboxd", "empire-of-the-sun"), CatalogueId("letterboxd", "kinowo-no-such-slug")))
    lb(CatalogueId("letterboxd", "empire-of-the-sun")).map(_.via) shouldBe Seq("Wikidata P6127")
    lb(CatalogueId("letterboxd", "kinowo-no-such-slug")) shouldBe Seq(CatalogueHit(None, Some(27205), Some("tt1375666"), "Letterboxd"))
  }

  "the catalogue store" should "be a gap until filed, hold what was filed, and count among the families' answers" in {
    val families = new FamilyAnswerStore(new InMemoryTmdbDocuments, clock)
    val s        = new CatalogueAnswerStore(families, clock, pages)
    val fantasy  = CatalogueId("webedia", "325279")
    s.mapped(fantasy) shouldBe Answer.Unknown
    s.fileMapped(fantasy, Seq(CatalogueHit(Some("Q135441923"), Some(1318829), Some("tt36112899"), "Wikidata P1265")))
    s.filePage(empire, Seq(CatalogueId("letterboxd", "empire-of-the-sun")))
    s.mapped(fantasy) shouldBe Answer.Known(Seq(CatalogueHit(Some("Q135441923"), Some(1318829), Some("tt36112899"), "Wikidata P1265")))
    s.linked(empire) shouldBe Answer.Known(Seq(CatalogueId("letterboxd", "empire-of-the-sun")))
    families.version shouldBe 2
  }

  it should "ask an id no item states again after a month, a mapped one after a year" in {
    val s = store()
    val (mapped, unmapped) = (CatalogueId("webedia", "1"), CatalogueId("webedia", "2"))
    s.fileMapped(mapped, Seq(CatalogueHit(Some("Q1"), Some(1), None, "Wikidata P1265")))
    s.fileMapped(unmapped, Nil)
    Seq(mapped, unmapped).map(id => s.wanted(CatalogueQuestion.Id(id))) shouldBe Seq(false, false)
    clock.advanceSeconds(CatalogueAnswerStore.UnmappedAge.toSeconds + 1)
    Seq(mapped, unmapped).map(id => s.wanted(CatalogueQuestion.Id(id))) shouldBe Seq(false, true)
  }

  "catalogue questions" should "go on the queue a batch of ids per source and a page each, once however often met" in {
    val queue = new InMemoryTaskQueue
    val questions = (1 to AgreementCatalogueQuestions.Batch + 1).map(i => CatalogueQuestion.Id(CatalogueId("webedia", i.toString))).toSet[CatalogueQuestion] +
      CatalogueQuestion.Id(CatalogueId("letterboxd", "x")) + CatalogueQuestion.Page(empire)
    AgreementQuestions.enqueueOpen(queue, Set.empty, Set.empty, clock, catalogue = questions)
    AgreementQuestions.enqueueOpen(queue, Set.empty, Set.empty, clock, catalogue = questions)
    queue.monitor().counts.values.sum shouldBe 4
  }

  it should "be mapped and read and filed, a projection asked for, and skipped once filed" in {
    val s           = store()
    val projections = new AtomicInteger
    val fetch       = venues
    val handler     = new AgreementCatalogueHandler(s, mapping(), reader(fetch), () => { projections.incrementAndGet(); () }, clock)
    val ids         = Task("t1", TaskType.AgreementCatalogue, "agreement-catalogue|webedia|325279,144766", Map("source" -> "webedia", "ids" -> "325279,144766"), 1)
    handler.handle(ids) shouldBe HandlerOutcome.Done
    (s.mapped(CatalogueId("webedia", "325279")).toOption.map(_.flatMap(_.tmdb)), s.mapped(CatalogueId("webedia", "144766"))) shouldBe
      ((Some(Seq(1318829)), Answer.Known(Nil)))
    val page = Task("t2", TaskType.AgreementCatalogue, s"agreement-catalogue-page|$empire", Map("page" -> empire), 1)
    handler.handle(page) shouldBe HandlerOutcome.Done
    s.linked(empire).toOption.map(_.size) shouldBe Some(2)
    handler.handle(page) shouldBe HandlerOutcome.Skipped
    fetch.calls.size shouldBe 1
    projections.get shouldBe 2
  }
}
