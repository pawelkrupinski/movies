package services.identity
package adapters

import org.bson.{BsonDocument, BsonInt64, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.FilmDetail
import services.venuepages.{HistoryVenuePageStore, InMemoryVenuePageStore, VenuePage, VenuePageKey}
import tools.{MutableClock, TestWiring}

/** The history keeps CHANGES ([[HistoryTmdbDocuments]], [[HistoryVenuePageStore]]): an answer whose value moved, before and
 *  after as an answer reads it; a deleted one; never a re-stamp of the same value — but every marker, whose stamps are
 *  what it says. And a value at a moment is the one the first change after it replaced ([[IdentityHistory.valueAt]]). */
class HistoryTmdbDocumentsSpec extends AnyFlatSpec with Matchers {
  private final class World {
    val clock   = new MutableClock(TestWiring.FixedInstant)
    val history = new InMemoryIdentityHistory
    val inner   = new InMemoryTmdbDocuments
    val docs    = new HistoryTmdbDocuments(inner, history, clock, "b1")
    def later(): Unit = clock.advance(java.time.Duration.ofMinutes(1))
  }
  private def query(ids: String, fetchedAt: Long) =
    new BsonDocument("ids", BsonString(ids)).append(TmdbStore.FetchedAt, BsonInt64(fetchedAt))

  private def ids(d: Option[BsonDocument]) = d.map(x => BsonFields.intsOf(x.get("ids")))

  "an answer the TMDB store files" should "be kept when its value moves, before and after, and not when only its stamp does" in {
    val w     = new World
    val store = new TmdbStore(w.docs, w.clock, history = w.history, build = "b1")
    store.people("q", Seq(1, 2))
    w.clock.advance(java.time.Duration.ofDays(2)); store.people("q", Seq(1, 2))   // re-stamped, the same value
    w.later(); store.people("q", Seq(3, 1, 2))
    val kept = w.history.all
    kept.map(e => (e.kind, e.id, ids(e.before), ids(e.after), e.build)) shouldBe Seq(
      ("answer:tmdb_queries", "q", None, Some(Seq(1, 2)), "b1"), ("answer:tmdb_queries", "q", Some(Seq(1, 2)), Some(Seq(3, 1, 2)), "b1"))
    kept.flatMap(e => e.after.toSeq ++ e.before).foreach(_.containsKey(TmdbStore.FetchedAt) shouldBe false)
  }

  it should "be kept as a film's answer reads it, the record's partials aside" in {
    val film = new BsonDocument("record", new BsonDocument("title", BsonString("Lalka"))).append("local", new BsonDocument("imdb_id", BsonString("tt1"))
      .append("overview", BsonString("…"))).append(TmdbStore.FetchedAt, BsonInt64(1))
    HistoryTmdbDocuments.valueOf(TmdbKind.Film, film).getDocument("local").keySet shouldBe java.util.Set.of("imdb_id")
  }

  "a family answer" should "be kept as filed, its re-file alike no entry, and read nothing to tell" in {
    val w = new World
    val fam = (v: String, at: Long) => new BsonDocument("hits", BsonString(v)).append(TmdbStore.FetchedAt, BsonInt64(at))
    w.docs.put(TmdbKind.Family, Seq("imdb|title|Lalka" -> fam("a", 1)))
    w.docs.put(TmdbKind.Family, Seq("imdb|title|Lalka" -> fam("a", 2)))
    w.docs.put(TmdbKind.Family, Seq("imdb|title|Lalka" -> fam("b", 3)))
    w.history.all.map(e => (e.after.map(_.getString("hits").getValue), e.beforeUnknown)) shouldBe Seq((Some("a"), true), (Some("b"), true))
  }

  it should "be kept as deleted when the retention sweep lets it go, and only when it did" in {
    val w = new World
    w.docs.put(TmdbKind.Query, Seq("q" -> query("1", 5), "r" -> query("2", 5)))
    w.later()
    w.docs.deleteIfStill(TmdbKind.Query, Seq("q" -> 5L, "r" -> 4L)) shouldBe 1
    w.history.all.filter(_.after.isEmpty).map(e => (e.id, e.before.map(_.getString("ids").getValue))) shouldBe Seq(("q", Some("1")))
  }

  it should "read at a moment as the first change after it found it, else as it is now" in {
    val w     = new World
    val store = new TmdbStore(w.docs, w.clock, history = w.history, build = "b1")
    store.people("q", Seq(1))
    val first = w.clock.millis()
    w.later(); store.people("q", Seq(2))
    val second = w.clock.millis()
    def at(t: Long) = new IdentityHistoryReplay(w.history.all, t).documents(w.inner).get(TmdbKind.Query, Seq("q")).get("q").map(d => BsonFields.intsOf(d.get("ids")))
    at(first) shouldBe Some(Seq(1))
    at(second) shouldBe Some(Seq(2))
    at(first - 1) shouldBe None
  }

  "a marker" should "be kept at every write, its stamps and all: each is an attempt" in {
    val w = new World
    val marker = (at: Long) => new BsonDocument(TmdbStore.FetchedAt, BsonInt64(at)).append("failures", BsonInt64(1))
    w.docs.put(TmdbKind.Marker, Seq("unanswered|q" -> marker(1)))
    w.later(); w.docs.put(TmdbKind.Marker, Seq("unanswered|q" -> marker(2)))
    w.history.all.map(_.after.map(_.getInt64(TmdbStore.FetchedAt).getValue)) shouldBe Seq(Some(1L), Some(2L))
  }

  "a venue page" should "be kept when what it says moves, not when it is only read again" in {
    val w     = new World
    val pages = new HistoryVenuePageStore(new InMemoryVenuePageStore, w.history, w.clock, "b1")
    val key   = VenuePageKey("group", "https://kino.example/lalka")
    val read  = (at: Long, director: String) => VenuePage(key, VenuePage.Read(FilmDetail(director = Seq(director))), java.time.Instant.ofEpochMilli(at))
    pages.put(read(1, "Wojciech Has"))
    pages.put(read(2, "Wojciech Has"))
    pages.put(VenuePage(key, VenuePage.Gone(404), java.time.Instant.ofEpochMilli(3)))
    w.history.all.map(e => (e.kind, e.id, e.before.isDefined, e.after.exists(_.containsKey("gone")))) shouldBe Seq(
      ("answer:venue_pages", key.id, false, false), ("answer:venue_pages", key.id, true, true))
    w.history.all.map(_.beforeUnknown) shouldBe Seq(true, false)
  }
}
