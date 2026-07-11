package services.staging

import models.{Cinema, CinemaShowing, Helios, Multikino, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{DetailEnricher, FilmDetail, FilmwebShowtimesClient}
import services.freshness.InMemoryFreshnessStore
import services.movies.{MovieService, TitleNormalizer}
import services.titlerules.{TitleRules, TitleRuleSet}

/** Unit specs for the staging enrichment steps factored out of the old
 *  `StagingPromoter` — the same scenarios, now exercised per discrete step (the
 *  shape the queue handlers + reaper consume). */
class StagingStepsSpec extends AnyFlatSpec with Matchers {

  private class FakeEnricher(val cinema: Cinema, detail: Option[FilmDetail], defer: Boolean = true)
    extends DetailEnricher {
    def detailGroup = "fake"
    override def defersTmdbResolution: Boolean = defer
    def fetchFilmDetail(ref: String): Option[FilmDetail] = detail
  }

  // Slot keyed per shown title (`CinemaShowing`), exactly as the scrape-divert path
  // writes a newcomer's slot — so the staging detail step (which targets the same
  // key) merges into it.
  private def listingRow(cinema: Cinema, title: String, year: Option[Int]): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](
      CinemaShowing.keyFor(cinema, title) -> SourceData(title = Some(title), releaseYear = year, filmUrl = Some(s"https://x/$title"))))

  private def seeded(cinema: Cinema, title: String, year: Option[Int]): (InMemoryStagingRepository, String) = {
    val repository = new InMemoryStagingRepository
    repository.upsert(cinema, title, year, listingRow(cinema, title, year))
    (repository, TitleNormalizer.sanitize(title))
  }

  private def steps(repository: InMemoryStagingRepository, enrichers: Seq[DetailEnricher],
                    resolve: (String, Option[Int], MovieRecord) => Option[MovieRecord],
                    recover: (String, Option[Int]) => Option[String] = (_, _) => None) =
    new StagingSteps(repository, enrichers, resolve, recover, new InMemoryFreshnessStore)

  "fetchDetailFor" should "fetch + merge a deferred cinema's detail into its row" in {
    val (repository, anchor) = seeded(Helios, "Newcomer", Some(2026))
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("A plot"), director = Seq("Jane Doe"))))
    val s = steps(repository, Seq(enricher), (_, _, r) => Some(r))

    s.fetchDetailFor(Helios, anchor) shouldBe true
    repository.findAll().head.record.cinemaData(Helios).synopsis shouldBe Some("A plot")
  }

  it should "owe no native detail for a Filmweb-fallback row (filmweb.pl filmUrl) — ready at once, no native fetch, no loop" in {
    // Root cause of the hot-loop: a cinema in Filmweb fallback stores Filmweb data
    // under its OWN source slot with a filmweb.pl filmUrl. Its native enricher
    // parses its own event page and can never fetch that URL, so the row owes no
    // native detail — it must graduate on the listing/Filmweb data immediately
    // rather than rescheduling until the give-up budget burns.
    val repository = new InMemoryStagingRepository
    repository.upsert(Helios, "Fallback", Some(2026), MovieRecord(data = Map[Source, SourceData](
      CinemaShowing.keyFor(Helios, "Fallback") -> SourceData(title = Some("Fallback"), filmUrl = Some(FilmwebShowtimesClient.filmPageUrl(1089))))))
    val anchor = TitleNormalizer.sanitize("Fallback")
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("native")))) // would merge if pointed at the URL
    val s = steps(repository, Seq(enricher), (_, _, r) => Some(r))

    s.fetchDetailFor(Helios, anchor) shouldBe true                    // ready at once — no loop, no give-up needed
    s.detailReady(repository.findAll().head) shouldBe true
    repository.findAll().head.record.cinemaData(Helios).synopsis shouldBe None  // native enricher never fetched the filmweb URL
  }

  it should "give up (degrade to listing-only) when told to, so a permanently-failing deferred fetch stops blocking the film" in {
    // The safety net for a genuinely-dead NATIVE event page (a cinema's own
    // filmUrl that 404s every pass): without a give-up the staging-detail step
    // reschedules forever; with it the film graduates on listing-only data.
    val (repository, anchor) = seeded(Helios, "Stuck", Some(2026))
    val enricher = new FakeEnricher(Helios, detail = None)            // deferred fetch never lands
    val s = steps(repository, Seq(enricher), (_, _, r) => Some(r))

    s.fetchDetailFor(Helios, anchor) shouldBe false                  // not ready yet — keep retrying
    s.detailReady(repository.findAll().head) shouldBe false
    s.fetchDetailFor(Helios, anchor, giveUp = true) shouldBe true    // budget exhausted — degrade
    s.detailReady(repository.findAll().head) shouldBe true           // marked fresh → resolve can proceed
  }

  "resolveAndStamp" should "resolve with the merged detail hints and stamp the hit on every row" in {
    val (repository, anchor) = seeded(Helios, "Newcomer", Some(2026))
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(director = Seq("Jane Doe"))))
    var sawDirector = false
    val s = steps(repository, Seq(enricher), (_, _, record) => {
      sawDirector = record.cinemaData.get(Helios).exists(_.director.contains("Jane Doe"))
      Some(record.copy(tmdbId = Some(1275779)))
    })

    s.fetchDetailFor(Helios, anchor) shouldBe true                    // detail FIRST
    s.resolveAndStamp(anchor) shouldBe StagingSteps.Resolved
    sawDirector shouldBe true                                          // resolve saw the detail hint
    repository.findAll().head.record.tmdbId shouldBe Some(1275779)
  }

  it should "stamp a definitive no-match (tmdbNoMatch)" in {
    val (repository, anchor) = seeded(Helios, "Obscure", Some(2026))
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("x"))))
    val s = steps(repository, Seq(enricher), (_, _, r) => Some(r.copy(tmdbNoMatch = true)))

    s.fetchDetailFor(Helios, anchor)
    s.resolveAndStamp(anchor) shouldBe StagingSteps.Resolved
    repository.findAll().head.record.tmdbNoMatch shouldBe true
  }

  it should "conclude a row in place when its display title re-keys its sanitize (drift-proof, no duplicate)" in {
    // Regression for the infinite staging loop: a row is keyed at creation under
    // one sanitize ("GWIEZDNE WOJNY:" — the case-sensitive prefix strip rule
    // leaves all-caps alone), but `recase` re-cases its display title to
    // "Gwiezdne Wojny:" which sanitize DOES strip. So `sanitize(displayTitle)` no
    // longer matches the persisted `_id` prefix. Stamping by a recomputed id then
    // wrote a DUPLICATE under the new key and left the original unconcluded — the
    // reaper re-resolved it forever (320k× observed). Stamping by the row's `id`
    // updates it in place. Fails before, passes now.
    //
    // Pinned to the DEFAULTS-only rule set so the case-sensitive `canonical-gwiezdne-wojny`
    // strip creates the all-caps-vs-title-case drift this guards. The full prod set adds
    // the case-INSENSITIVE `xtra-canonical-gwiezdne-wojny-ci`, which collapses both
    // spellings to one key — so this exact title no longer drifts in prod; the stamp-by-`id`
    // invariant is what we exercise, with a rule set where the drift still exists.
    TitleNormalizer.withRules(TitleRuleSet(TitleRules.all)) {
      val repository = new InMemoryStagingRepository
      val slotTitle  = "Gwiezdne Wojny: Mandalorian i Grogu"            // recase output (capital W → sanitize strips)
      // Persist under the NON-stripping all-caps spelling so the `_id` prefix and
      // the re-derived display title sanitize to DIFFERENT keys.
      repository.upsert(Helios, "GWIEZDNE WOJNY: Mandalorian i Grogu", None,
        MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(title = Some(slotTitle)))))

      val row    = repository.findAll().head
      val anchor = TitleNormalizer.sanitize(row.title)
      anchor should not be row.id.split('|')(1)                          // precondition: the drift exists

      val s = steps(repository, Seq.empty, (_, _, r) => Some(r.copy(tmdbNoMatch = true)))
      s.resolveAndStamp(anchor) shouldBe StagingSteps.Resolved
      s.resolveAndStamp(anchor) shouldBe StagingSteps.AlreadyDone        // concluded in place — would LOOP if duplicated

      val rows = repository.findAll()
      rows.size                      shouldBe 1                          // no duplicate spawned under the re-keyed sanitize
      rows.head.record.tmdbNoMatch   shouldBe true
    }
  }

  it should "report DetailNotReady (and NOT call resolve) for a deferred cinema whose detail hasn't landed" in {
    val (repository, anchor) = seeded(Helios, "Pending", Some(2026))
    val enricher = new FakeEnricher(Helios, detail = None)            // detail keeps failing
    var resolveCalled = false
    val s = steps(repository, Seq(enricher), (_, _, _) => { resolveCalled = true; None })

    s.resolveAndStamp(anchor) shouldBe StagingSteps.DetailNotReady
    resolveCalled shouldBe false
  }

  it should "report TransientFailure on a transient TMDB miss (retry next pass)" in {
    val (repository, anchor) = seeded(Helios, "Flaky", Some(2026))
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("x"))))
    val s = steps(repository, Seq(enricher), (_, _, _) => None)

    s.fetchDetailFor(Helios, anchor)
    s.resolveAndStamp(anchor) shouldBe StagingSteps.TransientFailure
    repository.findAll().head.record.tmdbConcluded shouldBe false
  }

  it should "resolve a non-deferred cinema immediately (no detail enricher)" in {
    val (repository, anchor) = seeded(Multikino, "Inline", Some(2026))
    val s = steps(repository, Seq.empty, (_, _, r) => Some(r.copy(tmdbId = Some(42))))

    s.resolveAndStamp(anchor) shouldBe StagingSteps.Resolved
    repository.findAll().head.record.tmdbId shouldBe Some(42)
  }

  it should "wait for a NON-deferring enricher's detail to be fetched before resolving (so display synopsis lands before the fold)" in {
    // Kino Muza-style: display-only detail (defersTmdbResolution = false). The
    // promoter fetched it before folding; the queue path must too, or the folded
    // movies row loses its synopsis.
    val (repository, anchor) = seeded(Helios, "Display", Some(2026))
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("syn"))), defer = false)
    val s = steps(repository, Seq(enricher), (_, _, r) => Some(r.copy(tmdbId = Some(1))))

    s.resolveAndStamp(anchor) shouldBe StagingSteps.DetailNotReady          // detail step hasn't run yet
    s.fetchDetailFor(Helios, anchor) shouldBe true
    repository.findAll().head.record.cinemaData(Helios).synopsis shouldBe Some("syn")
    s.resolveAndStamp(anchor) shouldBe StagingSteps.Resolved               // now it resolves
  }

  it should "report AlreadyDone for an already-concluded film" in {
    val (repository, anchor) = seeded(Multikino, "Done", Some(2026))
    val s = steps(repository, Seq.empty, (_, _, r) => Some(r.copy(tmdbId = Some(1))))
    s.resolveAndStamp(anchor) shouldBe StagingSteps.Resolved          // first pass concludes it
    s.resolveAndStamp(anchor) shouldBe StagingSteps.AlreadyDone       // second pass is a no-op
  }

  "recoverImdbFor" should "recover a missing imdbId when TMDB resolved without a cross-reference" in {
    val (repository, anchor) = seeded(Helios, "Pucio", Some(2026))
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("x"))))
    var searchedFor = Option.empty[String]
    val s = steps(repository, Seq(enricher),
      resolve = (_, _, r) => Some(r.copy(tmdbId = Some(1645035))),    // hit, but imdbId empty
      recover = (search, _) => { searchedFor = Some(search); Some("tt42003604") })

    s.fetchDetailFor(Helios, anchor); s.resolveAndStamp(anchor)
    s.recoverImdbFor(anchor)
    searchedFor shouldBe Some(MovieService.apiQuery("Pucio"))
    repository.findAll().head.record.imdbId shouldBe Some("tt42003604")
  }

  it should "recover from a NON-head row and mark done one-shot when the _id-first row has no tmdbId" in {
    // Regression: a film's cinemas disagree on year, and a late-arriving variant
    // with NO tmdbId sorts ahead (by `_id`) of the resolved one. Keying recovery
    // off `head` skipped both the recovery AND the freshness mark, so the reaper
    // re-enqueued StagingResolveImdbId forever (8790× observed in prod).
    val repository = new InMemoryStagingRepository
    val title = "Chłopiec na krańcach świata"
    val anchor = TitleNormalizer.sanitize(title)
    // Helios row (`_id` sorts first) has NO tmdbId; Multikino row carries tmdbId, no imdb.
    repository.upsert(Helios, title, Some(2025), MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(title = Some(title)))))
    repository.upsert(Multikino, title, None, MovieRecord(tmdbId = Some(1277047), data = Map[Source, SourceData](Multikino -> SourceData(title = Some(title)))))
    repository.findAll().head.record.tmdbId shouldBe None             // precondition: head lacks tmdbId

    var searched = Option.empty[String]
    val s = steps(repository, Seq.empty, (_, _, r) => Some(r), (search, _) => { searched = Some(search); Some("tt1277047") })
    s.imdbRecoveryDone(anchor) shouldBe false
    s.recoverImdbFor(anchor)
    searched should not be empty                                       // recovered from the needy (non-head) row
    s.imdbRecoveryDone(anchor) shouldBe true                          // one-shot mark set → reaper can't re-loop
    // Per-combination: only the needy group (Multikino, year=None) is stamped.
    // The Helios row is a different hint-combination (year 2025, no tmdbId) — it
    // recovers its own id once it resolves; never cross-stamped here.
    val rows = repository.findAll()
    rows.find(_.cinema == Multikino).flatMap(_.record.imdbId) shouldBe Some("tt1277047")
    rows.find(_.cinema == Helios).flatMap(_.record.imdbId)    shouldBe None
  }

  it should "resolve each cinema's hint-combination independently (no merge before settle)" in {
    // Two cinemas show the same title+year but report DIFFERENT directors, so they
    // are two hint-combinations. The OLD code unioned both directors and resolved
    // ONCE, stamping a single tmdbId on both rows; per-combination resolution
    // stamps each row with the film ITS hints resolve to. Fails before, passes now.
    val repository = new InMemoryStagingRepository
    val title = "Twins"; val year = Some(2026)
    val anchor = TitleNormalizer.sanitize(title)
    repository.upsert(Helios, title, year,
      MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(title = Some(title), releaseYear = year, director = Seq("Dir A")))))
    repository.upsert(Multikino, title, year,
      MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some(title), releaseYear = year, director = Seq("Dir B")))))

    val s = steps(repository, Seq.empty,
      (_, _, r) => Some(r.copy(tmdbId = Some(if (r.director.contains("Dir A")) 111 else 222))))
    s.resolveAndStamp(anchor) shouldBe StagingSteps.Resolved

    val rows = repository.findAll()
    rows.find(_.cinema == Helios).flatMap(_.record.tmdbId)    shouldBe Some(111)
    rows.find(_.cinema == Multikino).flatMap(_.record.tmdbId) shouldBe Some(222)
  }

  it should "mark recovery done even when there is nothing to recover (no needy row)" in {
    // No row has tmdbId-without-imdb, so recovery is a no-op — but it must still
    // mark done so the reaper doesn't treat the step as outstanding.
    val (repository, anchor) = seeded(Multikino, "NoNeed", Some(2026))
    val s = steps(repository, Seq.empty, (_, _, r) => Some(r), (_, _) => Some("x"))
    s.recoverImdbFor(anchor)
    s.imdbRecoveryDone(anchor) shouldBe true
  }

  it should "not call recovery when TMDB already shipped a cross-reference" in {
    val (repository, anchor) = seeded(Helios, "HasImdb", Some(2026))
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("x"))))
    var recoverCalled = false
    val s = steps(repository, Seq(enricher),
      resolve = (_, _, r) => Some(r.copy(tmdbId = Some(7), imdbId = Some("tt0000007"))),
      recover = (_, _) => { recoverCalled = true; None })

    s.fetchDetailFor(Helios, anchor); s.resolveAndStamp(anchor)
    s.recoverImdbFor(anchor)
    recoverCalled shouldBe false
    repository.findAll().head.record.imdbId shouldBe Some("tt0000007")
  }
}
