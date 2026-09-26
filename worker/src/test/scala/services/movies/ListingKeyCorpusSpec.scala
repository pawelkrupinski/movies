package services.movies

import models.{Cinema, CinemaMovie, Country}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.CorpusFixture

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/**
 * `ListingKey` is unique per distinct listing on every recorded corpus, and each naive key a
 * listing could be (and has been) identified by is not.
 *
 * A DISTINCT LISTING is what the venue published about a film: its raw title, its own year and
 * its directors. A key is sound when it never gives two distinct listings of one venue the same
 * value — otherwise the two films' showtimes share one slot, and one film is served the other's
 * (Belle 2013/2021, Sinn und Sinnlichkeit 1995/2026).
 *
 * Corpora read:
 *  - every corpus checked in under `test/resources/fixtures/corpus/` (the hard clusters of all
 *    five countries, the PL and DE convergence corpora, the PL sample);
 *  - `listing-key-collisions-de.json.gz`: the three DE venues' colliding rows, cut verbatim from
 *    recorder run 36153174348's DE corpus (the full corpora are too big to check in);
 *  - `listing-key-collisions-us.json.gz`: Marion Theatre Ocala's "Planet of the Apes" (Schaffner,
 *    no year) beside "Planet of the Apes (2001)" (Burton), cut from the same run's US corpus;
 *  - the five full recorded corpora, when `KINOWO_IDENTITY_CORPUS_DIR` names a directory
 *    holding `cinema-scrapes-<cc>.json.gz` (the recorder's `scrape-fixtures-<cc>` artifacts).
 */
class ListingKeyCorpusSpec extends AnyFlatSpec with Matchers with tools.SuiteConfiguration {

  private val corpusDir = Paths.get("test", "resources", "fixtures", "corpus")

  private final case class Corpus(label: String, country: Option[Country], path: Path) {
    lazy val listings: Seq[(Cinema, CinemaMovie)] =
      CorpusFixture.readFrom(path).flatMap(row => row.films.map(row.cinema -> _))
  }

  private def countryOf(file: String): Option[Country] =
    Country.all.find(c => file.endsWith(s"-${c.code}.json.gz"))

  private val checkedIn: Seq[Corpus] = {
    val stream = Files.list(corpusDir)
    try stream.iterator().asScala.filter { p =>
      val n = p.getFileName.toString
      (n.startsWith("cinema-scrapes-") || n.startsWith("listing-key-collisions-")) && n.endsWith(".json.gz")
    }.toSeq.sortBy(_.getFileName.toString).map(p => Corpus(p.getFileName.toString, countryOf(p.getFileName.toString), p))
    finally stream.close()
  }

  private val full: Seq[Corpus] = configuration.identityCorpusDirectory.toSeq.flatMap { dir =>
    Country.all.map(c => dir.value.resolve(s"cinema-scrapes-${c.code}.json.gz")).filter(Files.exists(_))
      .map(p => Corpus(s"full ${p.getFileName}", countryOf(p.getFileName.toString), p))
  }

  private val corpora = checkedIn ++ full

  /** What the venue published that names ONE film. */
  private def published(cm: CinemaMovie): (String, Option[Int], Seq[String]) =
    (cm.movie.rawTitle.getOrElse(cm.movie.title), cm.movie.releaseYear,
      cm.director.map(_.trim).filter(_.nonEmpty).distinct.sorted)

  /** Keys that hold two distinct listings of one venue, with what they hold. */
  private def collisions[K](c: Corpus, key: (Cinema, CinemaMovie) => K): Seq[String] =
    c.listings.groupBy { case (cinema, cm) => key(cinema, cm) }.toSeq.flatMap { case (k, rows) =>
      val distinct = rows.map { case (_, cm) => published(cm) }.distinct
      Option.when(distinct.sizeIs > 1)(s"$k -> ${distinct.sortBy(_.toString).mkString(" | ")}")
    }.sorted

  private type Key = (Cinema, CinemaMovie) => Any

  /** The keys a listing has been identified by, none of them sound. A page-less listing's
   *  "page" is its raw title, so "venue + page" is exactly the key a native id alone gives. */
  private def naiveKeys(c: Corpus): Seq[(String, Key)] = Seq[(String, Key)](
    "venue + raw title" -> ((cin, cm) => (cin.displayName, published(cm)._1)),
    "venue + page"      -> ((cin, cm) => (cin.displayName, cm.filmUrl.getOrElse(published(cm)._1))),
    "venue + page + raw title (the shadow prototype's ListingId)" ->
      ((cin, cm) => (cin.displayName, cm.filmUrl.getOrElse(""), published(cm)._1))
  ) ++ c.country.map(TitleNormalizer.forCountry).toSeq.map(n =>
    "the production slot key" -> ((cin: Cinema, cm: CinemaMovie) => (cin.displayName, ScrapeListing.slotKey(cin, cm.movie.title, n))))

  "ListingKey" should "give every distinct listing of a venue its own key, on every recorded corpus" in {
    corpora.map(_.label) should contain allOf ("cinema-scrapes-hard-clusters-pl.json.gz", "listing-key-collisions-de.json.gz")
    info(s"${corpora.size} corpora, ${corpora.map(_.listings.size).sum} listings: ${corpora.map(_.label).mkString(", ")}")
    val found = corpora.flatMap(c => collisions(c, ListingKey.of).map(line => s"${c.label}: $line"))
    found shouldBe empty
  }

  it should "tell apart the two films a page-less venue lists under one raw title" in {
    val de   = corpora.find(_.label == "listing-key-collisions-de.json.gz").get
    val keys = de.listings.map { case (cinema, cm) => ListingKey.of(cinema, cm) }
    keys.distinct should have size 6
    keys.collect { case k: ListingKey.Published => (k.venue, k.rawTitle, k.year) }.sorted shouldBe Seq(
      ("Cinema-Arthouse", "Sinn und Sinnlichkeit", Some(1995)), ("Cinema-Arthouse", "Sinn und Sinnlichkeit", Some(2026)),
      ("Club Manufaktur", "Bad Apples", Some(2018)), ("Club Manufaktur", "Bad Apples", Some(2025)),
      ("Schauburg Karlsruhe", "Sinn und Sinnlichkeit", Some(1995)), ("Schauburg Karlsruhe", "Sinn und Sinnlichkeit", Some(2026)))
  }

  it should "tell apart the films a venue links to one shared programme page" in {
    val pl = corpora.find(_.label == "cinema-scrapes-hard-clusters-pl.json.gz").get
    val kinoPort = pl.listings.collect { case (cinema, cm) if cinema.displayName == "KinoPort" => ListingKey.of(cinema, cm) }
    val shared   = kinoPort.collect { case k: ListingKey.Native => k }.groupBy(_.nativeId).filter(_._2.sizeIs > 1)
    shared should not be empty
    shared.values.foreach(ks => ks.distinct.size shouldBe ks.size)
  }

  /** Two listings the venue itself published as DIFFERENT films: years a production-vs-release
   *  gap apart, or credited to disjoint directors. The oracle is the published evidence
   *  `ListingKey.Published` keys a page-less listing by, never a year or film the pipeline derived. */
  private def publishedAsDifferentFilms(a: CinemaMovie, b: CinemaMovie): Boolean = {
    val (ya, yb) = (ScrapeListing.yearOf(a), ScrapeListing.yearOf(b))
    ya.zip(yb).exists { case (x, y) => (x - y).abs > services.resolution.YearWindow.ProductionToRelease } ||
      (directors(a).nonEmpty && directors(b).nonEmpty && (directors(a) & directors(b)).isEmpty)
  }

  private def directors(cm: CinemaMovie) = cm.director.map(_.trim.toLowerCase).filter(_.nonEmpty).toSet

  "The production slot fold" should "never put two films a venue lists under one title on one slot, on every recorded corpus" in {
    corpora.map(_.label) should contain ("listing-key-collisions-us.json.gz")
    val found = corpora.filter(_.country.isDefined).flatMap { c =>
      val normalizer = TitleNormalizer.forCountry(c.country.get)
      val tokens     = ScreeningTokens.of(c.country.get)
      c.listings.groupMap(_._1)(_._2).toSeq.flatMap { case (cinema, raw) =>
        val slots = ScrapeListing.prepare(cinema, raw, normalizer, tokens).movies
          .groupBy(slot => ScrapeListing.slotKey(cinema, slot.movie.title, normalizer))
        // A listing's own slot carries what it published: its directors and its year. Two
        // listings of different films each need a slot of their own — not one representative
        // standing in for both (showtimes can't tell them apart: a double bill shares them).
        def carries(slot: CinemaMovie, cm: CinemaMovie) =
          directors(slot) == directors(cm) && ScrapeListing.yearOf(slot) == ScrapeListing.yearOf(cm)
        raw.groupBy(cm => ScrapeListing.slotKey(cinema, cm.movie.title, normalizer)).toSeq.flatMap { case (key, rows) =>
          val held = slots.getOrElse(key, Nil)
          rows.combinations(2).collectFirst {
            case Seq(a, b) if publishedAsDifferentFilms(a, b) &&
              !(held.exists(s => carries(s, a) && !carries(s, b)) && held.exists(s => carries(s, b) && !carries(s, a))) =>
              s"${c.label}: ${cinema.displayName} [$key] folds \"${a.movie.title}\" " +
                s"(${ScrapeListing.yearOf(a).getOrElse("—")}, ${a.director.mkString("/")}) with \"${b.movie.title}\" " +
                s"(${ScrapeListing.yearOf(b).getOrElse("—")}, ${b.director.mkString("/")})"
          }
        }
      }
    }
    found shouldBe empty
  }

  /** The listings the fold HIDES — a raw listing whose key no stored slot carries, because
   *  `ScrapeListing.prepare` unioned it into a slot whose representative is another listing — each
   *  with that slot and what tells the two keys apart (docs/design/identity-resolver.md §16.4 item 3). */
  private def hiddenListings(c: Corpus): Seq[(Cinema, CinemaMovie, CinemaMovie, String)] = {
    val normalizer = TitleNormalizer.forCountry(c.country.get)
    val tokens     = ScreeningTokens.of(c.country.get)
    c.listings.groupMap(_._1)(_._2).toSeq.flatMap { case (cinema, raw) =>
      val slots = ScrapeListing.prepare(cinema, raw, normalizer, tokens).movies
      val held  = slots.map(ListingKey.of(cinema, _)).toSet
      raw.distinctBy(ListingKey.of(cinema, _)).filterNot(cm => held(ListingKey.of(cinema, cm))).map { cm =>
        val key  = ScrapeListing.slotKey(cinema, cm.movie.title, normalizer)
        val slot = slots.find(s => ScrapeListing.slotKey(cinema, s.movie.title, normalizer) == key).get
        val page = cm.filmUrl.map(_.trim).filter(_.nonEmpty) != slot.filmUrl.map(_.trim).filter(_.nonEmpty)
        val rawTitle = published(cm)._1 != published(slot)._1
        val facts    = published(cm)._2 != published(slot)._2 || published(cm)._3 != published(slot)._3
        (cinema, cm, slot, Seq("page" -> page, "raw title" -> rawTitle, "year/directors" -> facts).collect { case (n, true) => n }.mkString(" + "))
      }
    }
  }

  it should "hide only listings of the film whose slot holds them, and say what their keys differ in" in {
    // Measured, not a gate: the listings a read by listingKey cannot find until slots are one per
    // listing (phase 5). The claim is that each is the SAME film as its slot's representative (the
    // fold's own contract), so hiding it loses a key, never a film's showtimes.
    corpora.filter(_.country.isDefined).foreach { c =>
      val hidden = hiddenListings(c)
      hidden.foreach { case (cinema, cm, slot, _) =>
        withClue(s"${c.label}: ${cinema.displayName} hides '${cm.movie.title}' behind '${slot.movie.title}': ") {
          publishedAsDifferentFilms(cm, slot) shouldBe false
        }
      }
      if (hidden.nonEmpty)
        info(s"${c.label}: ${hidden.size} hidden listing(s) at ${hidden.map(_._1).distinct.size} venue(s) — " +
          hidden.groupBy(_._4).toSeq.sortBy(-_._2.size).map { case (k, hs) => s"$k ${hs.size}" }.mkString(", ") +
          "; e.g. " + hidden.take(3).map { case (cin, cm, slot, _) =>
            s"${cin.displayName}: '${published(cm)._1}' ${cm.filmUrl.getOrElse("-")} behind '${published(slot)._1}' ${slot.filmUrl.getOrElse("-")}" }.mkString("; "))
    }
  }

  "Every naive listing key" should "fold two distinct listings of one venue together on some recorded corpus" in {
    // The key each of these names is not unique per listing — the evidence the design doc's
    // "Listing identity" section cites. Each must fail SOMEWHERE in the corpora read, or it
    // would be a candidate key and this proof would be incomplete.
    val labels = corpora.flatMap(naiveKeys).map(_._1).distinct
    val failing = labels.filter(label => corpora.exists(c => naiveKeys(c).find(_._1 == label).exists { case (_, k) => collisions(c, k).nonEmpty }))
    failing shouldBe labels
  }
}
