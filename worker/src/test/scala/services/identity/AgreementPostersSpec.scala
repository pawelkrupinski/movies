package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.AgreementStage.PosterQuestion
import services.sharecards.{PosterDownload, PosterFailure, VipsPosterShrinker}
import services.tasks.{HandlerOutcome, InMemoryTaskQueue, Task, TaskType}
import tools.MutableClock

import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger

/** The posters the agreement's poster evidence reads: hashed from the real images (recorded in
 *  `test/resources/fixtures/identity-posters`: Kino Patria's "Dyrygent" poster, and TMDB's `/images` answers and 185-wide
 *  prints for Provazník's 2025 "Broken Voices" and Wajda's 1980 "The Conductor"), filed and read back, and asked for on
 *  the queue. */
class AgreementPostersSpec extends AnyFlatSpec with Matchers {
  private val clock    = new MutableClock(Instant.parse("2026-10-05T00:00:00Z"))
  private val fixtures = Path.of("test", "resources", "fixtures", "identity-posters")
  private val patria   = "https://kinopatria.com/wp-content/uploads/2026/10/8232401.3-264x396_c.jpg"

  /** The recorded images by URL, each handed over as a copy (the hashing deletes what it is handed). */
  private final class Recorded(failure: Option[String] = None) extends PosterDownload {
    val fetched = new AtomicInteger
    def fetch(url: String): Either[String, Path] = failure.toLeft {
      fetched.incrementAndGet()
      val name = if (url == patria) "venue-patria-dyrygent.jpg" else "tmdb-w185" + url.stripPrefix(clients.TmdbClient.PosterHashBase).replace('/', '-')
      val copy = Files.createTempFile("poster-", ".img")
      Files.copy(fixtures.resolve(name), copy, StandardCopyOption.REPLACE_EXISTING)
      copy
    }
  }
  private val tmdb = new clients.TmdbClient(http = tools.RoutingHttpFetch.getOnly(Seq(1483477, 95269).map(id =>
    s"/movie/$id/images?include_image_language=pl,en,null" -> Files.readString(fixtures.resolve(s"images-$id.json"))).toMap),
    apiKey = Some(settings.TmdbApiKey("fake")), language = java.util.Locale.forLanguageTag("pl-PL"))
  private def hashing(download: PosterDownload = new Recorded()) =
    new PosterHashing(download, new VipsPosterShrinker(binary = None), id => tmdb.posters(id, also = Seq("en")), "pl")

  "a venue's poster" should "match the film it is of, and name it against the namesake the families took" in {
    val h       = hashing()
    val venue   = h.venue(patria).toSeq
    val broken  = h.film(1483477)
    val wajda   = h.film(95269)
    (broken.size, wajda.size) shouldBe ((3, PosterEvidence.FilmPosters))
    PosterEvidence.nearest(venue, broken).get should be <= PosterEvidence.VetoMatchBits
    PosterEvidence.nearest(venue, wajda).get should be > PosterEvidence.VetoBits
    PosterEvidence.veto(Some(95269), Seq(Map(95269 -> PosterEvidence.nearest(venue, wajda), 1483477 -> PosterEvidence.nearest(venue, broken))))
      .map(_._1) shouldBe Some(1483477)
  }

  "a film's posters" should "be its country's language first, then English, then none, by votes" in {
    def image(path: String, language: Option[String], votes: Int) = clients.TmdbClient.PosterImage(path, language, 0.667, 0, 0, votes)
    PosterHashing.chosen(Seq(image("/none", None, 9), image("/en", Some("en"), 1), image("/pl-low", Some("pl"), 0), image("/pl", Some("pl"), 3),
      image("/de", Some("de"), 9)), "pl") shouldBe Seq("/pl", "/pl-low", "/en", "/none", "/de")
  }

  "the poster store" should "be a gap until a poster is filed, and hold what was hashed, none for an unreadable one" in {
    val families = new FamilyAnswerStore(new InMemoryTmdbDocuments, clock)
    val store    = new PosterAnswerStore(families, clock)
    store.venue(patria) shouldBe Answer.Unknown
    store.film(1) shouldBe Answer.Unknown
    store.file(PosterQuestion.Venue(patria), Seq(PosterHash(-7L)))
    store.file(PosterQuestion.Film(1), Seq(PosterHash(1L), PosterHash(2L)))
    store.file(PosterQuestion.Venue("https://gone"), Nil)
    store.file(PosterQuestion.Film(2), Nil)
    (store.venue(patria), store.film(1), store.venue("https://gone"), store.film(2)) shouldBe
      ((Answer.Known(Some(PosterHash(-7L))), Answer.Known(Seq(PosterHash(1L), PosterHash(2L))), Answer.Known(None), Answer.Known(Nil)))
    // counted as the families' answers are: one version covers all the agreement reads
    families.version shouldBe 4
    families.changedSince(0).map(_.size) shouldBe Some(4)
  }

  it should "want a poster hashed again only once its hash is a year old" in {
    val store = new PosterAnswerStore(new FamilyAnswerStore(new InMemoryTmdbDocuments, clock), clock)
    store.wanted(PosterQuestion.Film(1)) shouldBe true
    store.file(PosterQuestion.Film(1), Seq(PosterHash(1L)))
    store.wanted(PosterQuestion.Film(1)) shouldBe false
    clock.advanceSeconds(PosterAnswerStore.Age.toSeconds + 1)
    store.wanted(PosterQuestion.Film(1)) shouldBe true
  }

  "a poster to hash" should "go on the queue once, however often the stage meets it" in {
    val queue = new InMemoryTaskQueue
    val posters = Set[PosterQuestion](PosterQuestion.Venue(patria), PosterQuestion.Film(95269))
    AgreementQuestions.enqueueOpen(queue, Set.empty, Set.empty, clock, posters = posters)
    AgreementQuestions.enqueueOpen(queue, Set.empty, Set.empty, clock, posters = posters)
    queue.monitor().counts.values.sum shouldBe 2
  }

  it should "be hashed and filed, a projection asked for, and skipped once filed" in {
    val store       = new PosterAnswerStore(new FamilyAnswerStore(new InMemoryTmdbDocuments, clock), clock)
    val download    = new Recorded()
    val projections = new AtomicInteger
    val handler     = new AgreementPosterHandler(store, hashing(download), () => { projections.incrementAndGet(); () }, clock)
    val task        = Task("t1", TaskType.AgreementPoster, s"agreement-poster|venue|$patria", Map("venuePoster" -> patria), 1)
    handler.handle(task) shouldBe HandlerOutcome.Done
    store.venue(patria).toOption.flatten shouldBe defined
    projections.get shouldBe 1
    handler.handle(task) shouldBe HandlerOutcome.Skipped
    download.fetched.get shouldBe 1
    handler.handle(Task("t2", TaskType.AgreementPoster, "agreement-poster|film|1483477", Map("filmPoster" -> "1483477"), 1)) shouldBe HandlerOutcome.Done
    store.film(1483477).toOption.map(_.size) shouldBe Some(3)
  }

  it should "be filed as no poster when the origin refuses it, and asked again when the failure may pass" in {
    val store = new PosterAnswerStore(new FamilyAnswerStore(new InMemoryTmdbDocuments, clock), clock)
    val task  = Task("t1", TaskType.AgreementPoster, s"agreement-poster|venue|$patria", Map("venuePoster" -> patria), 1)
    new AgreementPosterHandler(store, hashing(new Recorded(Some(PosterFailure.Timeout))), () => (), clock).handle(task) shouldBe a[HandlerOutcome.Reschedule]
    store.venue(patria) shouldBe Answer.Unknown
    new AgreementPosterHandler(store, hashing(new Recorded(Some(PosterFailure.Http4xx))), () => (), clock).handle(task) shouldBe HandlerOutcome.Done
    store.venue(patria) shouldBe Answer.Known(None)
  }
}
