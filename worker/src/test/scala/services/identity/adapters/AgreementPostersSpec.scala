package services.identity
package adapters

import tools.ScratchDirectories
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.AgreementStage.PosterQuestion
import services.sharecards.{PosterDownload, PosterFailure, VipsPosterShrinker}
import services.tasks.{HandlerOutcome, InMemoryTaskQueue}
import tools.MutableClock

import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger

/** The posters the agreement's poster evidence reads: hashed from the real images (recorded in
 *  `test/resources/fixtures/identity-posters`: Kino Patria's "Dyrygent" poster, and TMDB's `/images` answers and 185-wide
 *  prints for Provazník's 2025 "Broken Voices" and Wajda's 1980 "The Conductor"), filed and read back, and asked for on
 *  the queue. */
class AgreementPostersSpec extends AnyFlatSpec with Matchers with ScratchDirectories {
  private def asker(store: PosterAnswerStore, hashing: PosterHashing, filed: () => Unit = () => (),
                    memory: QuestionMemory = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", clock)) =
    AgreementAskers.over(new FamilyAnswerStore(new InMemoryTmdbDocuments, clock), clock)(posters = store, hashing = hashing, filed = filed,
      memory = memory)
  private def venueTask(attempts: Int = 1) = AgreementAskers.task(Question.Poster(PosterQuestion.Venue(patria)))(attempts)
  private def filmTask(tmdbId: Int)        = AgreementAskers.task(Question.Poster(PosterQuestion.Film(tmdbId)))()
  private val clock    = new MutableClock(Instant.parse("2026-10-05T00:00:00Z"))
  private val fixtures = Path.of("test", "resources", "fixtures", "identity-posters")
  private val patria   = "https://kinopatria.com/wp-content/uploads/2026/10/8232401.3-264x396_c.jpg"

  /** The recorded images by URL, each handed over as a copy (the hashing deletes what it is handed). */
  private final class Recorded(failure: Option[String] = None) extends PosterDownload {
    val fetched = new AtomicInteger
    def fetch(url: String): Either[String, Path] = failure.toLeft {
      fetched.incrementAndGet()
      val name = if (url == patria) "venue-patria-dyrygent.jpg" else "tmdb-w185" + url.stripPrefix(PosterAnswers.FilmPosterBase).replace('/', '-')
      val copy = tempFile("poster-", ".img")
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
    val broken  = h.film(1483477).map(_.hash)
    val wajda   = h.film(95269).map(_.hash)
    (broken.size, wajda.size) shouldBe ((3, PosterEvidence.FilmPosters))
    PosterEvidence.nearest(venue, broken).get should be <= PosterEvidence.VetoMatchBits
    PosterEvidence.nearest(venue, wajda).get should be > PosterEvidence.VetoBits
    PosterEvidence.veto(Some(95269), Seq(Map(95269 -> PosterEvidence.nearest(venue, wajda), 1483477 -> PosterEvidence.nearest(venue, broken))))
      .map(_._1) shouldBe Some(1483477)
  }

  // prod 2026-10-05: TMDB films since deleted answered /images with 404 and their tasks were asked again 11 times
  "a film TMDB no longer has" should "have no posters, not a failure asked again" in {
    val gone = new PosterHashing(new Recorded(), new VipsPosterShrinker(binary = None),
      id => throw new tools.HttpStatusException(404, "GET", s"https://api.themoviedb.org/3/movie/$id/images", None), "pl")
    gone.film(1575247) shouldBe Nil
    val store = new PosterAnswerStore(new FamilyAnswerStore(new InMemoryTmdbDocuments, clock), clock)
    asker(store, gone).handle(filmTask(1575247)) shouldBe HandlerOutcome.Done
    store.film(1575247) shouldBe Answer.Known(Nil)
    val down = new PosterHashing(new Recorded(), new VipsPosterShrinker(binary = None),
      id => throw new tools.HttpStatusException(503, "GET", s"https://api.themoviedb.org/3/movie/$id/images", None), "pl")
    a[tools.HttpStatusException] should be thrownBy down.film(1)
  }

  // prod 2026-10-05: OCK's "…/event//Vincent. Legenda oceanu PLAKAT_M.jpg" — raw spaces no fetch takes, so "network",
  // asked again 11 times
  "a venue's poster" should "be fetched by its link escaped, as a card serves it" in {
    val asked    = new java.util.concurrent.ConcurrentLinkedQueue[String]
    val download = new PosterDownload {
      def fetch(url: String): Either[String, Path] = { asked.add(url); Left(PosterFailure.Http4xx) }
    }
    new PosterHashing(download, new VipsPosterShrinker(binary = None), _ => Nil, "pl")
      .venue("https://ock.systembiletowy.pl/uploads/event//Vincent. Legenda oceanu PLAKAT_M.jpg") shouldBe None
    asked.toArray.toSeq shouldBe Seq("https://ock.systembiletowy.pl/uploads/event//Vincent.%20Legenda%20oceanu%20PLAKAT_M.jpg")
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
    store.file(PosterQuestion.Venue(patria), Seq(HashedPoster(patria, PosterHash(-7L))))
    store.file(PosterQuestion.Film(1), Seq(HashedPoster("/1.jpg", PosterHash(1L)), HashedPoster("/2.jpg", PosterHash(2L))))
    store.file(PosterQuestion.Venue("https://gone"), Nil)
    store.file(PosterQuestion.Film(2), Nil)
    (store.venue(patria), store.film(1), store.venue("https://gone"), store.film(2)) shouldBe
      ((Answer.Known(Some(PosterHash(-7L))), Answer.Known(Seq(PosterHash(1L), PosterHash(2L))), Answer.Known(None), Answer.Known(Nil)))
    (store.unread(PosterQuestion.Venue("https://gone")), store.unread(PosterQuestion.Film(2))) shouldBe ((true, false))
    // counted as the families' answers are: one version covers all the agreement reads
    families.version shouldBe 4
    families.changedSince(0).map(_.size) shouldBe Some(4)
  }

  it should "want a poster hashed again only once its hash is a year old" in {
    val store = new PosterAnswerStore(new FamilyAnswerStore(new InMemoryTmdbDocuments, clock), clock)
    store.wanted(PosterQuestion.Film(1)) shouldBe true
    store.file(PosterQuestion.Film(1), Seq(HashedPoster("/1.jpg", PosterHash(1L))))
    store.wanted(PosterQuestion.Film(1)) shouldBe false
    clock.advanceSeconds(QuestionPolicies.PosterAge.toSeconds + 1)
    store.wanted(PosterQuestion.Film(1)) shouldBe true
  }

  "a poster to hash" should "go on the queue once, however often the stage meets it" in {
    val queue = new InMemoryTaskQueue
    val posters = Set[PosterQuestion](PosterQuestion.Venue(patria), PosterQuestion.Film(95269))
    val memory = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    AgreementQuestions.enqueue(queue, posters.map(Question.Poster(_)), memory, clock)
    AgreementQuestions.enqueue(queue, posters.map(Question.Poster(_)), memory, clock)
    queue.monitor().counts.values.sum shouldBe 2
  }

  it should "be hashed and filed, a projection asked for, and skipped once filed" in {
    val store       = new PosterAnswerStore(new FamilyAnswerStore(new InMemoryTmdbDocuments, clock), clock)
    val download    = new Recorded()
    val projections = new AtomicInteger
    val handler     = asker(store, hashing(download), () => { projections.incrementAndGet(); () })
    val task        = venueTask()
    handler.handle(task) shouldBe HandlerOutcome.Done
    store.venue(patria).answered.flatten shouldBe defined
    projections.get shouldBe 1
    handler.handle(task) shouldBe HandlerOutcome.Skipped
    download.fetched.get shouldBe 1
    handler.handle(filmTask(1483477)) shouldBe HandlerOutcome.Done
    store.film(1483477).answered.map(_.size) shouldBe Some(3)
  }

  // the review pages show a candidate's TMDB poster: the paths hashed, filed beside their hashes, in their order — one
  // filing, counted once, as a filing without them was
  it should "file a film's poster paths beside the hashes read from them" in {
    val families = new FamilyAnswerStore(new InMemoryTmdbDocuments, clock)
    val store    = new PosterAnswerStore(families, clock)
    asker(store, hashing()).handle(filmTask(1483477)) shouldBe HandlerOutcome.Done
    val filed = families.document(PosterAnswers.idOf(PosterQuestion.Film(1483477))).get
    val paths = PosterHashing.chosen(tmdb.posters(1483477, also = Seq("en")), "pl")
    PosterAnswers.pathsOf(filed) shouldBe paths
    paths should have size 3
    store.film(1483477).answered.map(_.size) shouldBe Some(3)
    families.version shouldBe 1
    // a venue's poster is filed under its own URL: no path beside it
    store.file(PosterQuestion.Venue(patria), Seq(HashedPoster(patria, PosterHash(-7L))))
    PosterAnswers.pathsOf(families.document(PosterAnswers.idOf(PosterQuestion.Venue(patria))).get) shouldBe Nil
  }

  // prod PL 2026-10-06: biletyna.pl refused all 226 of its posters (403) and each was filed as "no poster" for a year —
  // a failed read taken for data, the poster evidence for Binti, Fritzi and the rest gone with it
  it should "be filed as unread when the origin refuses it, asked again a week on, and asked again minutes on when the failure may pass" in {
    val store  = new PosterAnswerStore(new FamilyAnswerStore(new InMemoryTmdbDocuments, clock), clock)
    val memory = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    val task   = venueTask()
    asker(store, hashing(new Recorded(Some(PosterFailure.Timeout))), memory = memory).handle(task) shouldBe HandlerOutcome.Done
    store.venue(patria) shouldBe Answer.Unknown
    clock.advanceSeconds(QuestionPolicies.FirstFailureWait.toSeconds + 1)
    asker(store, hashing(new Recorded(Some(PosterFailure.Http4xx))), memory = memory).handle(task) shouldBe HandlerOutcome.Done
    (store.venue(patria), store.unread(PosterQuestion.Venue(patria))) shouldBe ((Answer.Known(None), true))
    store.fresh(PosterQuestion.Venue(patria)) shouldBe true
    clock.advanceSeconds(QuestionPolicies.PosterUnreadAge.toSeconds + 1)
    store.fresh(PosterQuestion.Venue(patria)) shouldBe false
  }

  // the 226 biletyna.pl posters as prod filed them on 2026-10-06: no hash, not marked unread
  it should "want a venue poster filed as none before a failed read was told from data a week on, as an unread one" in {
    val families = new FamilyAnswerStore(new InMemoryTmdbDocuments, clock)
    val store    = new PosterAnswerStore(families, clock)
    val biletyna = "https://biletyna.pl/file/get/id/394113"
    families.put(PosterAnswers.idOf(PosterQuestion.Venue(biletyna)), new org.bson.BsonDocument("hashes", org.bson.BsonNull.VALUE))
    store.file(PosterQuestion.Film(2), Nil)
    store.venue(biletyna) shouldBe Answer.Known(None)
    store.fresh(PosterQuestion.Venue(biletyna)) shouldBe true
    clock.advanceSeconds(QuestionPolicies.PosterUnreadAge.toSeconds + 1)
    (store.wanted(PosterQuestion.Venue(biletyna)), store.fresh(PosterQuestion.Venue(biletyna))) shouldBe ((true, false))
    // a film TMDB keeps no poster of is data, not a failed read: read for a year
    store.wanted(PosterQuestion.Film(2)) shouldBe false
  }

  // prod PL 2026-10-06: Kino Kryterium's poster host times out the worker, so 17 of its posters were asked again 10
  // times and more, and the correction waiting on one never decided
  it should "be given up on once its fetch has failed the policy's attempts in a row, filed as unread and remembered so" in {
    val store    = new PosterAnswerStore(new FamilyAnswerStore(new InMemoryTmdbDocuments, clock), clock)
    val memory   = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    val download = new Recorded(Some(PosterFailure.Timeout))
    val timeouts = asker(store, hashing(download), memory = memory)
    // each failure waited out before the next ask: one asked inside its wait is not asked
    def attempt() = { val outcome = timeouts.handle(venueTask()); clock.advanceSeconds(QuestionPolicies.MaxFailureWait.toSeconds + 1); outcome }
    (1 until QuestionPolicies.PosterGiveUpAttempts).foreach(_ => attempt() shouldBe HandlerOutcome.Done)
    store.venue(patria) shouldBe Answer.Unknown
    attempt() shouldBe HandlerOutcome.Done
    (store.venue(patria), store.unread(PosterQuestion.Venue(patria))) shouldBe ((Answer.Known(None), true))
    // hashed again sooner than a read poster: the host may answer again
    store.wanted(PosterQuestion.Venue(patria)) shouldBe false
    memory.due(Set[Question](Question.Poster(PosterQuestion.Venue(patria)))) shouldBe empty
    clock.advanceSeconds(QuestionPolicies.PosterUnreadAge.toSeconds + 1)
    store.wanted(PosterQuestion.Venue(patria)) shouldBe true
    memory.due(Set[Question](Question.Poster(PosterQuestion.Venue(patria)))) should not be empty
  }
  // A turned-away ask (its host's breaker open, or the fleet's pace refusing it) counted as a failed attempt: a flapping
  // poster host, HostRecovery releasing the wait each time its breaker closed, spent the poster's eight attempts in
  // minutes and it was given up on for a week, its evidence lost. A turn-away is a wait now; but a host blocked for a day
  // still ends the ask (the model's input gate waits on every ask terminating), and the run's start is kept with the
  // question, so a worker killed in between neither restarts the day nor ends it early.
  it should "wait out a host that turns its asks away, however often, yet give up once it has been blocked a day, across a restart" in {
    val docs     = new InMemoryTmdbDocuments   // what Mongo keeps: the answers and the question memory's markers
    val store    = new PosterAnswerStore(new FamilyAnswerStore(docs, clock), clock)
    val question = Question.Poster(PosterQuestion.Venue(patria))
    val blocked  = new PosterDownload { def fetch(url: String) = throw new tools.CircuitOpenException("kinopatria.com", 60000L) }
    val memory   = new QuestionMemory(docs, "pl-PL", clock)
    val flapping = asker(store, hashing(blocked), memory = memory)
    (1 to 3 * QuestionPolicies.PosterGiveUpAttempts).foreach { _ =>
      flapping.handle(venueTask()) shouldBe HandlerOutcome.Done
      memory.recovered("kinopatria.com")   // its breaker closes: HostRecovery releases the wait at once
      clock.advanceSeconds(60)
    }
    store.venue(patria) shouldBe Answer.Unknown
    memory.concluded(question) shouldBe false
    // killed (no shutdown hook); a new worker over the same stores, the host still blocked a day after the first turn-away
    clock.advanceSeconds(QuestionPolicies.TurnAwayGiveUp.toSeconds - 24 * 60)
    val restarted = new QuestionMemory(docs, "pl-PL", clock)
    asker(store, hashing(blocked), memory = restarted).handle(venueTask()) shouldBe HandlerOutcome.Done
    (store.venue(patria), store.unread(PosterQuestion.Venue(patria))) shouldBe ((Answer.Known(None), true))
    restarted.concluded(question) shouldBe true
  }
}
