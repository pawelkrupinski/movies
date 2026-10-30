package scripts

import org.bson.{BsonArray, BsonDocument, BsonInt64}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, AnswersChanged, CandidateQuery, ForwardingIdentityLookups, Hit, IdentityLookups, IdentityMeasures,
  IdentityResolver, IdentityTraceSink, InMemoryIdentityModelStore, TakeFlips}
import services.identity.adapters.{FamilyAnswerStore, InMemoryTmdbDocuments, TmdbKind, TmdbStore}
import services.identity.agreement.VoterFamily
import tools.MutableClock

import java.nio.file.Path
import java.time.{Clock, Duration, Instant, ZoneOffset}

/** The drift replay's queue asks what production's asker asks: a family question whose answer was filed and is fresh is
 *  not asked of the recording's sites again — read again, the recording's answer replaced the one filed, which the whole
 *  resolve reads (PL "LALKA / DOLLY", seed 2, run 3154). */
class IdentityDriftSpec extends AnyFlatSpec with Matchers {
  private val question = VoterFamily.RottenTomatoes -> "title|Lalka Dolly"
  private val id       = FamilyAnswerStore.questionId(question._1, question._2)

  /** prod's filed answer to `question`, held back by a gate that holds every question (seed 2's held it). */
  private def filed(clock: MutableClock) = {
    val inner = new InMemoryTmdbDocuments
    inner.put(TmdbKind.Family, Seq(id -> new BsonDocument("hits", new BsonArray()).append(TmdbStore.FetchedAt, BsonInt64(clock.millis()))))
    val docs = new IdentityDrift.GatedDocuments(inner, new IdentityDrift.Share(2, 100))
    docs -> new FamilyAnswerStore(docs, clock)
  }

  "asks" should "release a held answer rather than ask for it" in {
    val clock         = new MutableClock(Instant.parse("2026-10-20T13:26:32Z"))
    val (docs, store) = filed(clock)
    IdentityDrift.asks(store, Seq(question), id => docs.held(id) && { docs.release(id); true }) shouldBe empty
    store.wanted(id) shouldBe false
  }

  it should "not ask again a gap a wait still lists once its answer was filed, the gate opened first (seed 2's order)" in {
    val clock         = new MutableClock(Instant.parse("2026-10-20T13:26:32Z"))
    val (docs, store) = filed(clock)
    // the cluster met the question while its answer was held: a gap its wait lists
    store.wanted(id) shouldBe true
    // every answer filed, then the wait's gaps handed to the queue again
    docs.openAll(store.noteFiled)
    IdentityDrift.asks(store, Seq(question), _ => false) shouldBe empty
  }

  it should "ask a question not filed, and one filed longer ago than its kind keeps it" in {
    val clock         = new MutableClock(Instant.parse("2026-10-20T13:26:32Z"))
    val (docs, store) = filed(clock)
    docs.openAll(store.noteFiled)
    val unfiled = VoterFamily.RottenTomatoes -> "title|Halloween '26: Lalka ( Dolly)"
    IdentityDrift.asks(store, Seq(question, unfiled), _ => false) shouldBe Seq(unfiled)
    clock.advance(Duration.ofDays(91))
    IdentityDrift.asks(store, Seq(question, unfiled), _ => false) shouldBe Seq(question, unfiled)
  }

  "a listing's inputs" should "move with its model cluster's members, and at a step that moved listings with the model's take of it" in {
    val lalka = IdentityDrift.ModelCluster(members = 17, film = "tmdb:1321666")
    IdentityDrift.inputsMoved(Some(lalka), Some(lalka.copy(members = 18)), listingsMoved = false) shouldBe true
    // PL Kino Eva's "LALKA", run 3509: new venues' listings came, its cluster kept its members, the model took it as none
    IdentityDrift.inputsMoved(Some(lalka), Some(lalka.copy(film = "none")), listingsMoved = true) shouldBe true
    // a deploy under the same rules moved nothing: the model's take moving there is a flip of the model's own
    IdentityDrift.inputsMoved(Some(lalka), Some(lalka.copy(film = "none")), listingsMoved = false) shouldBe false
    IdentityDrift.inputsMoved(Some(lalka), Some(lalka), listingsMoved = true) shouldBe false
    IdentityDrift.inputsMoved(None, Some(lalka), listingsMoved = false) shouldBe true
    // a listing no model cluster held at either step: nothing to tell its inputs apart by, read as moved as the members'
    // hash alone read it — the model take's clause only ever removes a flag
    IdentityDrift.inputsMoved(None, None, listingsMoved = false) shouldBe true
  }

  // A flip the leg finds is fixed, never listed (the owner, 2026-10-27). The one exception is a check's birth (the ratchet,
  // scripts/ci/allow_list_ratchet.py; convergence-failures-2026-10-29 recommendation 2): what a new or tightened check
  // finds at birth is listed TEMPORARY, with an owner and when it goes, so the check lands green -- and the list only shrinks.
  "the reviewed flips" should "hold nothing but a check's TEMPORARY birth rows, each with an owner and when it goes" in {
    val path = "test/resources/fixtures/identity-flips-allow.tsv"
    deploy.RepoFile.exists(path) shouldBe true
    val temporary = """(?i)^TEMPORARY\b.*\bowner:\s*\S+.*\bremove when\b\s*\S+.*""".r
    IdentityDrift.flipAllowList(java.nio.file.Path.of(path)).filterNot(row => temporary.matches(row.reason.trim)) shouldBe empty
  }

  // THE KNOWN-FLIPS BASELINE (the owner, 2026-10-29: "fail build only on new flips, focus in eliminating all old flips and
  // then fail on all flips"): a baselined listing's flip passes, counted; any other fails; a row whose seed ran without
  // its listing flipping is fixed; an empty baseline fails every flip, the strict gate.
  private def flip(venue: String, title: String) = {
    def taken(step: String, film: String) = TakeFlips.Taken(step, TakeFlips.Served(film, "OwnMatch"), moved = true)
    TakeFlips.Flip(services.movies.ListingKey.Published(venue, title, None, Nil),
      Vector(taken("1 boot", "tmdb:1"), taken("6 gone", "tmdb:2"), taken("10 scrape", "tmdb:1")))
  }
  private def known(venue: String, title: String, seed: Int) =
    IdentityDrift.BaselineFlip("de", seed, venue, title, "tmdb:1 -> tmdb:2 -> tmdb:1", "2026-10-29", "red fixer of convergence #2527", "")

  "the known-flips baseline" should "pass a baselined flip as known and fail a new one" in {
    val flips   = Seq(1 -> flip("Werkstattkino e.V.", "CAMOUFLAGE"), 2 -> flip("CinemaxX Dammtor", "Titans"))
    val verdict = IdentityDrift.flipVerdict("de", flips, Seq(known("Werkstattkino e.V.", "CAMOUFLAGE", 2)), Seq(1, 2))
    verdict.known.map(_._2.key.rawTitle) shouldBe Seq("CAMOUFLAGE")
    verdict.fresh.map(_._2.key.rawTitle) shouldBe Seq("Titans")
    verdict.fixed shouldBe empty
  }

  it should "name a row fixed when its seed ran and its listing flipped in none, and judge no seed the run did not replay" in {
    val baseline = Seq(known("Werkstattkino e.V.", "CAMOUFLAGE", 1), known("CinemaxX Dammtor", "Titans", 2))
    IdentityDrift.flipVerdict("de", Nil, baseline, Seq(1)).fixed.map(_.rawTitle) shouldBe Seq("CAMOUFLAGE")
    IdentityDrift.flipVerdict("de", Nil, baseline, Seq(1, 2)).fixed.map(_.rawTitle) shouldBe Seq("CAMOUFLAGE", "Titans")
  }

  it should "cover only its own country, and fail every flip once empty: the strict gate" in {
    val flips = Seq(1 -> flip("Werkstattkino e.V.", "CAMOUFLAGE"))
    IdentityDrift.flipVerdict("pl", flips, Seq(known("Werkstattkino e.V.", "CAMOUFLAGE", 1)), Seq(1)).fresh should have size 1
    IdentityDrift.flipVerdict("de", flips, Nil, Seq(1)).fresh should have size 1
  }

  it should "read the checked-in baseline, every row with its seed, first sighting and owner" in {
    val path = java.nio.file.Path.of("test/resources/fixtures/identity-flips-baseline.tsv")
    deploy.RepoFile.exists(path.toString) shouldBe true
    noException should be thrownBy IdentityDrift.flipBaseline(path)
  }

  /** `inner`'s answers but those `held` names (`q:<sortKey>`, `f:<film>`, as [[IdentityDrift.Gate]] names them): what the
   *  fill has not filed yet. */
  private final class Withheld(inner: IdentityLookups) extends ForwardingIdentityLookups(inner) {
    var held = Set.empty[String]
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]] =
      if (held(s"q:${query.sortKey}")) Answer.Unknown else inner.candidates(query)
    override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] =
      if (held(s"f:$tmdbId")) Answer.Unknown else inner.film(tmdbId)
  }

  /** One slice through the incremental model, its answers filed as `held` shrinks: at each step the model must be the
   *  whole resolve of the listings seen and the answers filed, and its takes are recorded as the drift replay's are. */
  private final class SliceReplay(dir: String) {
    private val slice     = IdentitySlice.readSlice(Path.of(dir))
    private val replaying = new tools.SliceHttp.Replaying(slice.http)
    private val reads     = new IdentitySlice.Reads(slice.country, replaying, replaying.gaps, Clock.fixed(slice.recordedAt, ZoneOffset.UTC),
      slice.filed, tools.ProdState.none)
    private val inputs    = IdentityDrift.inputsOf(reads)
    val lookups           = new Withheld(reads.lookups)
    val listings          = slice.core ++ slice.context
    val model             = inputs.incremental(lookups, reads.normalizer, new InMemoryIdentityModelStore, "rules", IdentityTraceSink.Discard)
    val flips             = new TakeFlips
    private var seen      = Seq.empty[services.identity.Listing]
    private def takesOf(decisions: Seq[services.identity.ResolverDecision]) =
      decisions.flatMap(d => d.members.map(_ -> IdentitySlice.filmOf(d))).toMap
    def takes(title: String): Map[String, String] =
      takesOf(model.decisions).collect { case (key, film) if key.rawTitle == title => key.venue -> film }
    def scraped(step: String, venues: String => Boolean): Unit = {
      val arriving = listings.filter(listing => venues(listing.venue))
      seen ++= arriving
      model.batch(seen = arriving)
      check(step)
    }
    def gone(step: String, venues: String => Boolean): Unit = {
      val leaving = seen.filter(listing => venues(listing.venue))
      seen = seen.filterNot(listing => venues(listing.venue))
      model.batch(gone = leaving.map(_.key))
      check(step)
    }
    def filed(step: String, queries: Set[CandidateQuery], films: Set[Int]): Unit = {
      lookups.held = lookups.held -- queries.map(query => s"q:${query.sortKey}") -- films.map(film => s"f:$film")
      model.batch(answered = AnswersChanged(queries, films))
      check(step)
    }
    private def check(step: String): Unit = {
      val now     = takesOf(model.decisions)
      val pending = IdentityDrift.pendingOf(model.decisions)
      now shouldBe takesOf(IdentityResolver.resolve(seen, lookups, reads.normalizer, inputs).decisions)
      flips.record(step, now.map { case (key, film) => key -> TakeFlips.Served(film, "", pending(key)) }, _ => true)
    }
  }

  // AGREE OR NONE (the owner, 2026-10-29): while a film's record is pending, a take only where the pool with it and the
  // pool without it agree, else none until it is filed. Each case is a convergence flip, replayed as its seed filed it.

  "a search filed before the records of the films it names" should "move no take back and forth" in {
    // Convergence #2527 (run 3757), takes-drift-1 (united-kingdom): seed 1's gate held flicks.co.uk's "Rose (2023)" search
    // and the records of the films it names. The fill filed the search at step 2 and "Rose Parade 2023"'s record at step 8;
    // between them the listing took the parade, which only its record (a 90-minute US broadcast) denies, then went back to
    // Oplev's "Rose" (2022).
    val replay = new SliceReplay("test/resources/fixtures/identity-slices/uk-queen-s-film-theatre-belfast-rose-2023--4f64ad1a")
    val search = CandidateQuery.Title("Rose (2023)")
    replay.lookups.held = Set(s"q:${search.sortKey}", "f:1069112")
    replay.scraped("boot", _ => true)
    replay.filed("the search filed", Set(search), Set.empty)
    replay.takes("Rose (2023)").values.toSet should not contain "tmdb:1069112"
    replay.filed("its record filed", Set.empty, Set(1069112))
    replay.flips.flips.map(_.line) shouldBe empty
    replay.takes("Rose (2023)").values.toSet shouldBe Set("tmdb:901251")
  }

  "a bare title whose rival's record is pending" should "take no film either reading alone would guess" in {
    // Drift 3810, takes-drift-1 (germany), with the rival's record left out of the pool: Metropol-Kino-Center's bare "Madoka
    // Magica - Der Film: Rebellion" took the 2013 film (212162) at boot, the 1962 "Rebellion" (42997) once its record was
    // filed, and 212162 again once Filmhaus (Langerringen)'s listing joined it.
    val replay = new SliceReplay("test/resources/fixtures/drift-slices/de/metropol-madoka-6a9b3bcc")
    replay.lookups.held = Set("f:42997")
    replay.scraped("boot", _ == "Metropol-Kino-Center")
    // both readings agree since the pooled tie goes to the title covering more of the listing's (the next case)
    replay.takes("Madoka Magica - Der Film: Rebellion") shouldBe Map("Metropol-Kino-Center" -> "tmdb:212162")
    replay.filed("the rival's record filed", Set.empty, Set(42997))
    replay.scraped("Filmhaus's listing", _ => true)
    replay.flips.flips.map(_.line) shouldBe empty
    replay.takes("Madoka Magica - Der Film: Rebellion").values.toSet shouldBe Set("tmdb:212162")
  }

  "a namesake whose record is pending" should "not stand in for the film the model takes once it is filed" in {
    // Drift 3810, takes-drift-2 (united-kingdom), with the namesake's record left out of the pool: Chichester's "The Ties
    // That Bind Us" took the 2025 film (1096642) at boot, the model's take 1460982 once its record was filed at step 8, and
    // 1096642 again when the agreement corrected it at settle. The model's take, every answer filed, is 1460982. Main's
    // reading (the pending film weighed on its hits) passes this too: it holds the agree-or-none rule's other half.
    val replay = new SliceReplay("test/resources/fixtures/drift-slices/uk/chichester-ties-1231860f")
    replay.lookups.held = Set("f:1460982")
    replay.scraped("boot", _ => true)
    replay.takes("The Ties That Bind Us").values.toSet should not contain "tmdb:1096642"
    replay.filed("its record filed", Set.empty, Set(1460982))
    replay.flips.flips.map(_.line) shouldBe empty
    replay.takes("The Ties That Bind Us").values.toSet shouldBe Set("tmdb:1460982")
  }

  "a bare title its pooled scoring ties between two films" should "take the one whose title covers more of its own" in {
    // The drift slice of DE Metropol-Kino-Center's bare "Madoka Magica - Der Film: Rebellion" (11 of 12 seeds): alone it
    // measured 1962's "Rebellion" (42997) and 2013's Madoka film (212162) `segment` alike, 46.3% each, and the pooled vote
    // took 42997 by its lower id -- 212162 -> 42997 -> 212162 as Filmhaus (Langerringen)'s dated listing left and came
    // back. The owner's decision (identity design): an exact top tie goes to the title covering more of the listing's.
    val replay = new SliceReplay("test/resources/fixtures/drift-slices/de/metropol-madoka-6a9b3bcc")
    val title  = "Madoka Magica - Der Film: Rebellion"
    replay.scraped("boot", _ == "Metropol-Kino-Center")
    replay.takes(title) shouldBe Map("Metropol-Kino-Center" -> "tmdb:212162")
    replay.scraped("Filmhaus's listing", _ => true)
    replay.gone("Filmhaus's listing gone", _ == "Filmhaus (Langerringen)")
    replay.takes(title) shouldBe Map("Metropol-Kino-Center" -> "tmdb:212162")
    replay.scraped("Filmhaus's listing back", _ == "Filmhaus (Langerringen)")
    replay.flips.flips.map(_.line) shouldBe empty
    replay.takes(title).values.toSet shouldBe Set("tmdb:212162")
  }

  "a cluster whose fallback's record is pending" should "take no film, saying what it waits on, and its fallback once filed" in {
    // Drift 3991 (the precise none, a229501a7): US "Parkway Drive: Home" x123 went none -> tt47657111 -> none -> tt47657111.
    // Every TMDB candidate is denied it; it falls back to IMDb's tt47657111, whose record (fallback id -181874839) the
    // seeds' gate held. Held, the cluster takes no film -- agree-or-none, the fallback scope alike -- but its decision told
    // nothing pending (Vetoed, no lookup unanswerable), so the flip check read a take given up. It now names the records
    // its pools await.
    val replay = new SliceReplay("test/resources/fixtures/drift-slices/us/parkway-drive-home-2abd6734")
    val title  = "Parkway Drive: Home"
    replay.lookups.held = Set("f:-181874839")
    replay.scraped("boot", _ => true)
    replay.takes(title).values.toSet shouldBe Set("none")
    IdentityDrift.pendingOf(replay.model.decisions).map(_.rawTitle) should contain (title)
    replay.filed("the fallback's record filed", Set.empty, Set(-181874839))
    replay.flips.flips.map(_.line) shouldBe empty
    replay.takes(title).values.toSet shouldBe Set("imdb:tt47657111")
  }
}
