package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import services.identity._
import services.movies.{ListingKey, ScrapeListing, ScreeningTokens}
import tools._

import java.nio.file.Files
import scala.collection.mutable
import scala.util.Try

/**
 * ID seeding, measured (docs/design/identity-resolver.md §8, phase 4): [[IdSeeding]] with
 * TODAY'S production films as the previous assignment — each film the set of its slots' listing
 * keys, exported read-only by `scripts.ListingKeyBackfill --export` — against the resolver's
 * clusters on the recorded full corpus of the same day. Nothing is written anywhere but the
 * report; no FilmId changes.
 *
 * The review list the migration needs, per country: films no cluster overlaps, films merged
 * away (a larger film's id won their cluster), films the resolver splits, and the clusters that
 * would get a fresh id — counts and 20 examples each, in `seeding-<cc>.txt`. Beside it: how many
 * of today's listing keys the corpus holds at all, and how many raw listings today's slot fold
 * hides behind another listing's slot (the listings a `listingKey` read cannot find yet).
 *
 * Opt-in: runs when `KINOWO_IDENTITY_SEED_FILMS` (the export directory), `KINOWO_IDENTITY_FULL`,
 * `KINOWO_IDENTITY_CORPUS_DIR` and `KINOWO_FIXTURE_ROOT` are set, as for
 * `IdentityShadowIntegrationSpec`'s full corpora. No pipeline boot — the resolver alone.
 */
class IdentitySeedingIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with IntegrationMongoSuite {

  import IdentityShadow._

  private val storages = mutable.ListBuffer.empty[ConvergenceStorage]

  private val out = configuration.identityShadowOutput.value
  private val seeded: Seq[(Corpus, java.nio.file.Path)] = for {
    films  <- configuration.identitySeedFilms.toSeq
    dir    <- configuration.identityCorpusDirectory.toSeq
    corpus <- IdentityShadow.full(configuration.identityFullCorpora.value, dir.value, configuration.fixtureRoot)
    file    = films.value.resolve(s"films-${corpus.country.code}.json") if Files.exists(file)
  } yield corpus -> file

  private val summary = mutable.ListBuffer.empty[String]

  private def filmsFrom(file: java.nio.file.Path): Seq[IdSeeding.Film] =
    (Json.parse(Files.readString(file)) \ "films").as[Map[String, Seq[String]]].toSeq.map { case (id, keys) =>
      IdSeeding.Film(id, keys.flatMap(ListingKey.parse).toSet)
    }

  private def show(k: ListingKey): String = k match {
    case ListingKey.Native(v, _, raw)             => s"$v: '$raw'"
    case ListingKey.Published(v, raw, year, dirs) => s"$v: '$raw'${year.fold("")(y => s" ($y)")}${if (dirs.isEmpty) "" else dirs.mkString(" [", ", ", "]")}"
  }
  private def cluster(c: Set[ListingKey]): String = s"${c.size} listing(s), e.g. ${c.toSeq.sorted.take(2).map(show).mkString("; ")}"

  seeded.foreach { case (c, file) =>
    "ID seeding" should s"review today's films against the resolver's clusters on ${c.label}" in {
      val films = filmsFrom(file)
      val w = wiring(mongoTarget, c, storages, configuration.fixtureRoot, configuration.env)
      val listings = listingsOf(w, c.normalizer)
      val source = new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)),
        language = c.country.language, retrySleep = (_: Long) => ()), w.detailEnrichers, c.misses)
      val (resolution, seconds) = timed(IdentityResolver.resolve(listings, new Memo(source), c.normalizer, IdentityCalibration.default))
      val clusters = resolution.decisions.map(_.listings)
      val review   = IdSeeding.review(films, clusters)

      // How much of today's corpus the recording holds, and what the slot fold hides.
      val corpusKeys = listings.map(_.key).toSet
      val todayKeys  = films.flatMap(_.listings).toSet
      val tokens     = ScreeningTokens.of(c.country)
      val hidden     = w.archivedListings.toSeq.map { case (cinema, rows) =>
        rows.map(ListingKey.of(cinema, _)).distinct.size - ScrapeListing.prepare(cinema, rows, c.normalizer, tokens).movies.size
      }.sum

      val lines = mutable.ListBuffer.empty[String]
      def section[A](title: String, items: Seq[A])(line: A => String): Unit = {
        lines += s"\n## $title: ${items.size}"
        items.take(20).foreach(a => lines += s"  - ${line(a)}")
      }
      lines += s"# ID seeding review, ${c.label}: ${films.size} production films (${todayKeys.size} listing keys), " +
        s"${clusters.size} resolver clusters over ${listings.size} listings (resolve ${seconds.round}s)"
      lines += s"today's listing keys found in the corpus: ${todayKeys.count(corpusKeys)} of ${todayKeys.size} (${pct(todayKeys.count(corpusKeys).toLong, todayKeys.size.toLong)}); " +
        s"raw listings the slot fold hides behind another listing's slot: $hidden"
      lines += s"films that keep their id: ${review.keeps.size}"
      section("films no cluster overlaps (their listings are not in the corpus)", review.unmatched)(f =>
        s"${f.id}: ${cluster(f.listings)}")
      section("films merged away (a larger film's id won their cluster)", review.mergedAway) { case (f, winner) =>
        s"${f.id} → ${winner}: ${cluster(f.listings)}" }
      section("films the resolver splits over two or more clusters", review.split) { case (f, cs) =>
        s"${f.id} (${f.listings.size}) → ${cs.map(k => s"[${cluster(k.intersect(f.listings))}]").mkString(" + ")}" }
      val (freshSeen, freshUnseen) = review.fresh.partition(_._2.nonEmpty)
      section("clusters that get a fresh id, overlapping a film that went elsewhere", freshSeen) { case (k, fs) =>
        s"${cluster(k)} — overlaps ${fs.mkString(", ")}" }
      section("clusters that get a fresh id, overlapping no production film", freshUnseen) { case (k, _) => cluster(k) }
      Files.createDirectories(out)
      Files.writeString(out.resolve(s"seeding-${c.country.code}.txt"), lines.mkString("\n") + "\n")
      summary.synchronized(summary += f"| ${c.country.code} | ${films.size} | ${clusters.size} | ${review.keeps.size} | ${review.unmatched.size} | " +
        s"${review.mergedAway.size} | ${review.split.size} | ${freshSeen.size} | ${freshUnseen.size} | " +
        s"${pct(todayKeys.count(corpusKeys).toLong, todayKeys.size.toLong)} | $hidden |")
      println(lines.take(3).mkString("\n"))

      // The gate the doc sets for this phase (§10): every film maps to one cluster or is on this list.
      (review.keeps.keySet ++ review.unmatched.map(_.id) ++ review.mergedAway.map(_._1.id)) shouldBe films.filter(_.listings.nonEmpty).map(_.id).toSet
    }
  }

  override protected def afterAll(): Unit = {
    if (summary.nonEmpty) {
      val table = ("| country | films | clusters | keep id | no cluster | merged away | split | fresh (film elsewhere) | fresh (unseen) | keys in corpus | fold-hidden listings |" +:
        "|---|---|---|---|---|---|---|---|---|---|---|" +: summary.sorted.toSeq).mkString("\n")
      Files.writeString(out.resolve("seeding-summary.md"), table + "\n")
      println(table)
    }
    storages.foreach(s => Try(s.close()))
    super.afterAll()
  }
}
