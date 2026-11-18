package services.identity

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titles.{FormatTags, LegacyFormatTags, LegacyTitleContainment, TitleContainment}
import services.titles.billing.{BillingPieces, LegacyBillingPieces}
import services.titles.rules.{LegacyTitleRuleTable, TitleRuleTable}
import tools.UnmatchedClusters

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** The billing and title parsers split into named steps (coupling/complexity phase 3, lane D) read every billing of the
 *  fixtures exactly as their copies from before the split ([[LegacyListingShape]], [[LegacyBilledPiece]],
 *  [[LegacyBillingPieces]], [[LegacyTitleContainment]], [[LegacyTitleRuleTable]], [[LegacyFormatTags]] and the cast
 *  walkers below): every listing of the five unmatched-cluster captures, every raw title the judgements name, the DE and
 *  PL corpus recordings' titles, synopses and casts, and the title-rule table with each of its rows broken every way the
 *  parser refuses. Temporary: removed with the copies once the split has landed. */
class BillingParseEquivalenceSpec extends AnyFlatSpec with Matchers {

  private lazy val captures = Country.all.filter(c => Files.exists(UnmatchedClusters.fixturePath(c))).map(c => UnmatchedClusters.read(UnmatchedClusters.fixturePath(c)))

  /** The corpus recordings' films: (country, title, synopsis, cast) — every country's newest corpus the identity tools'
   *  recording cache holds, or the checked-in one, extracted to `target/billing-equivalence/corpus.tsv` (one film a line,
   *  the cast's entries split by U+001F): parsed whole, the recordings' JSON outgrows the test JVM's heap. */
  private lazy val corpusFilms: Seq[(String, String, String, Seq[String])] =
    Files.readAllLines(Path.of("target", "billing-equivalence", "corpus.tsv"), StandardCharsets.UTF_8).asScala.toSeq.map { line =>
      val Array(cc, title, synopsis, cast) = line.split("\t", -1): @unchecked
      (cc, title, synopsis.replace("\\n", "\n"), cast.split('\u001f').toSeq.filter(_.nonEmpty))
    }

  private lazy val judgedTitles: Seq[(String, String)] =
    Files.readAllLines(Path.of("test", "resources", "fixtures", "identity-judgements.tsv"), StandardCharsets.UTF_8).asScala.toSeq
      .filterNot(line => line.startsWith("#") || line.startsWith("country\t")).map(_.split("\t", -1)).filter(_.length > 2)
      .map(cells => cells(0) -> cells(2))

  private lazy val ruleLines: Seq[String] =
    scala.io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(TitleRuleTable.Resource), "UTF-8").getLines().toSeq

  private lazy val ruleExamples: Seq[String] = TitleRuleTable.rows.flatMap(_.examples.flatMap { case (a, b) => Seq(a, b) })

  private lazy val allTitles: Seq[String] =
    (captures.flatMap(_.listings.flatMap(l => Seq(l.rawTitle, l.title, l.cleanTitle))) ++ judgedTitles.map(_._2) ++
      corpusFilms.map(_._2) ++ ruleExamples).distinct

  "The title parsers" should "read every title of the fixtures as before the split" in {
    allTitles.size should be > 8000
    allTitles.foreach { t =>
      withClue(t) {
        BillingPieces.workSpan(t) shouldBe LegacyBillingPieces.workSpan(t)
        BillingPieces.work(t) shouldBe LegacyBillingPieces.work(t)
        val peeled = FormatTags.peel(t)
        val legacy = LegacyFormatTags.peel(t)
        (peeled.title, peeled.tokens, peeled.certificate, peeled.runtimeMinutes) shouldBe
          (legacy.title, legacy.tokens, legacy.certificate, legacy.runtimeMinutes)
      }
    }
  }

  it should "find every token run as before, over lists, indexed sequences and the two mixed" in {
    val tokenised = allTitles.map(TitleContainment.tokens).filter(_.nonEmpty)
    var compared = 0
    tokenised.sliding(2).foreach { pair =>
      val whole = pair.head
      val bases = (1 to math.min(3, whole.size)).flatMap(n => Seq(whole.take(n), whole.takeRight(n), whole.slice(1, 1 + n))) ++ pair.lift(1).toSeq
      for (base <- bases; w <- Seq(whole.toList, whole.toVector, whole.toArray.toSeq); b <- Seq(base.toList, base.toVector, base.toArray.toSeq)) {
        withClue(s"$w / $b") { TitleContainment.isTokenRun(b, w) shouldBe LegacyTitleContainment.isTokenRun(b, w) }
        compared += 1
      }
    }
    compared should be > 100000
  }

  "The title-rule table" should "parse every row, and refuse every broken one, as before the split" in {
    def rows(lines: Seq[String]) = Try(TitleRuleTable.parse(lines.iterator).map(r => (r.rule, r.effect, r.season, r.seed, r.examples)))
    def legacyRows(lines: Seq[String]) = Try(LegacyTitleRuleTable.parse(lines.iterator).map(r => (r.rule, r.effect, r.season, r.seed, r.examples)))
    def same(lines: Seq[String]) = (rows(lines), legacyRows(lines)) match {
      case (scala.util.Success(a), scala.util.Success(b)) => a shouldBe b
      case (scala.util.Failure(a), scala.util.Failure(b)) => (a.getClass, a.getMessage) shouldBe (b.getClass, b.getMessage)
      case (a, b)                                         => fail(s"$a vs $b for ${lines.mkString("\n")}")
    }
    same(ruleLines)
    val data = ruleLines.filter(line => line.trim.nonEmpty && !line.startsWith("#"))
    data.size should be > 50
    def broken(cells: Array[String]): Seq[Array[String]] = {
      def set(i: Int, v: String) = cells.updated(i, v)
      Seq(cells :+ "extra", set(0, ""), set(7, ""), set(4, "all,bogus"), set(4, "seed"), set(5, "nope"), set(6, "jan"), set(6, "january"),
        set(1, "nope"), set(1, "venue"), set(2, "Some Cinema"), set(1, "venue").updated(2, "Kino"), set(3, "pl,xx"), set(3, "de"),
        set(10, ""), set(10, "not json"), set(10, """[["one"]]"""), set(10, "[]"), set(10, """[["a","b"],["c"]]"""))
    }
    data.zipWithIndex.foreach { case (line, i) =>
      val cells = line.split("\t", -1).padTo(11, "")
      (Seq(cells) ++ broken(cells)).foreach(variant => same(Seq("# a comment", "", variant.mkString("\t"))))
      if (i % 7 == 0) same(data.slice(i, i + 3))
    }
  }

  "The cast walkers" should "hash every synopsis and cast field of the corpus as before the split" in {
    val texts = corpusFilms.flatMap { case (_, title, synopsis, cast) => Seq(title, synopsis) ++ cast } ++ allTitles
    val casts = corpusFilms.map(_._4).filter(_.nonEmpty) ++ allTitles.map(t => t.split("[,:|-]").toSeq) ++ allTitles.map(Seq(_)) ++
      corpusFilms.map { case (_, _, synopsis, _) => synopsis.split("[.,]").toSeq }
    casts.size should be > 10000
    texts.foreach(text => withClue(text) {
      VenueNames.of(Seq(text)).hashes shouldBe LegacyCastWalkers.venueNames(Seq(text), Nil).toSeq
      ListedCast.wordsOf(text).toSeq shouldBe LegacyCastWalkers.wordsOf(text).toSeq
    })
    casts.foreach(cast => withClue(cast) {
      VenueNames.of(Nil, cast).hashes shouldBe LegacyCastWalkers.venueNames(Nil, cast).toSeq
      ListedCast.of(cast).hashes shouldBe LegacyCastWalkers.listedCast(cast).toSeq
    })
  }

  "A piece billed beside the film" should "be read of every captured billing and its namesakes as before the split" in {
    var compared = 0
    var beside   = 0
    captures.foreach { capture =>
      val country = Some(capture.country.code)
      val films   = capture.films.collect { case (id, Some(film)) => id -> film }.toSeq
      val byFirst = films.flatMap { case (id, film) => TitleContainment.tokens(film.title).headOption.map(_ -> (id, film)) }.groupMap(_._1)(_._2)
      val template = capture.listings.head
      val billed   = judgedTitles.collect { case (cc, raw) if cc == capture.country.code => raw } ++
        corpusFilms.collect { case (cc, raw, _, _) if cc == capture.country.code => raw }
      val judged   = billed.distinct.map(raw => template.copy(rawTitle = raw, title = raw, cleanTitle = raw))
      val listings = (capture.listings ++ judged).distinctBy(l => (l.cinema.displayName, l.rawTitle, l.title, l.originalTitle))
      listings.foreach { listing =>
        val words      = TitleContainment.tokens(listing.rawTitle).toSet
        val namesakes  = words.toSeq.flatMap(byFirst.getOrElse(_, Nil))
          .filter { case (_, film) => TitleContainment.tokens(film.title).forall(words) }.distinctBy(_._1).take(12)
        val pieces     = BillingPieces.segments(listing.rawTitle).parts.map(_.trim).filter(_.nonEmpty)
          .map(piece => None -> IdentityMeasures.Film(piece, None, directors = Some(Seq(firstWord(listing)))))
        val candidates = namesakes.map { case (id, film) => Some(id) -> film } ++ pieces
        val evidence   = Evidence.of(listing, None)
        candidates.foreach { case (tmdbId, film) =>
          for ((venues, bannerSlot) <- Seq[(Seq[String] => Boolean, Boolean)]((_ => false, true), (_.sizeIs == 1, true), (_ => false, false))) {
            val now = ListingShape.billedBeside(listing, Seq(film), venues, bannerSlot)
            withClue(s"${listing.rawTitle} / ${film.title}") { now shouldBe LegacyListingShape.billedBeside(listing, Seq(film), venues, bannerSlot) }
            if (now.isDefined) beside += 1
            compared += 1
          }
          val measures = BilledPiece.measuresOf(evidence, film, country)
          val answered = (query: CandidateQuery) => capture.queries.getOrElse(query, Nil)
          val recurs   = (shape: String) => shape.length % 3 == 0
          for (keeps <- BilledPiece.Keeps.values.toSeq; corroborated <- Seq(0.0, IdentityResolver.PieceCorroboration.toDouble)) {
            withClue(s"${listing.rawTitle} / ${film.title} / $keeps / $corroborated") {
              BilledPiece.of(evidence, Seq(listing), country, film, tmdbId, measures, corroborated, recurs, answered, keeps) shouldBe
                LegacyBilledPiece.of(evidence, Seq(listing), country, film, tmdbId, measures, corroborated, recurs, answered, keeps)
            }
          }
          for (keeps <- BilledPiece.Keeps.values.toSeq)
            BilledPiece.ofFamilies(Seq(listing), film, tmdbId, keeps) shouldBe LegacyBilledPiece.ofFamilies(Seq(listing), film, tmdbId, keeps)
        }
        // two films at once, as the agreement reads a billing against several
        if (namesakes.sizeIs >= 2) {
          val two = namesakes.take(2).map(_._2)
          ListingShape.billedBeside(listing, two) shouldBe LegacyListingShape.billedBeside(listing, two)
        }
      }
    }
    info(s"$compared billings against a film compared, $beside of them a piece beside it")
    compared should be > 50000
    beside should be > 1000
  }

  /** A director's name for a piece's film: the billing's first word, so the credit rule reads too. */
  private def firstWord(listing: Listing): String = TitleContainment.tokens(listing.rawTitle).headOption.getOrElse("x")
}

/** The cast walkers as they read before the split: `VenueNames.collect`, `ListedCast.of` and `ListedCast.wordsOf`. */
private object LegacyCastWalkers {
  import scala.util.hashing.MurmurHash3

  private def pairHash(a: Int, b: Int): Int = MurmurHash3.finalizeHash(MurmurHash3.mixLast(MurmurHash3.mix(0x4e616d65, a), b), 2)
  private def joins(c: Char): Boolean = Character.isWhitespace(c) && c != '\n' && c != '\r' || c == '-' || c == '\'' || c == '’' || c == '.'
  private def pair(a: Int, b: Int): Int =
    MurmurHash3.finalizeHash(MurmurHash3.mixLast(MurmurHash3.mix(0x4c697374, math.min(a, b)), math.max(a, b)), 2)

  def venueNames(texts: Iterable[String], cast: Iterable[String]): Array[Int] = {
    val found = scala.collection.mutable.ArrayBuilder.make[Int]
    texts.foreach(text => collect(text, capitalisedOnly = true, found))
    cast.foreach(name => collect(name, capitalisedOnly = false, found))
    val all = found.result()
    if (all.isEmpty) Array.emptyIntArray else SortedHashes.sortedDistinct(all, all.length)
  }

  private def collect(text: String, capitalisedOnly: Boolean, into: scala.collection.mutable.ArrayBuilder[Int]): Unit = {
    var previous = 0
    var inRun    = false
    var hyphened = false
    var i        = 0
    val n        = text.length
    while (i < n) {
      val c = text.charAt(i)
      if (Character.isLetterOrDigit(c)) {
        var j = i
        if (capitalisedOnly && !Character.isUpperCase(c) && !(inRun && hyphened)) {
          while (j < n && Character.isLetterOrDigit(text.charAt(j))) j += 1
          inRun = false
        } else {
          var h = 0
          while (j < n && Character.isLetterOrDigit(text.charAt(j))) { h = 31 * h + VenueNames.fold(text.charAt(j)); j += 1 }
          if (inRun) into += pairHash(previous, h)
          previous = h; inRun = true
        }
        i = j
      } else {
        if (!joins(c)) inRun = false
        hyphened = c == '-'
        i += 1
      }
    }
  }

  def listedCast(cast: Seq[String]): Array[Int] = {
    var found   = new Array[Int](16)
    var count   = 0
    var names   = 0
    var oneWord = false
    val it = cast.iterator
    while (it.hasNext && !oneWord) {
      val text = it.next()
      var words = 0; var previous = 0
      var i = 0
      val n = text.length
      while (i <= n && !oneWord) {
        val c = if (i < n) text.charAt(i) else '\n'
        if (Character.isLetterOrDigit(c)) {
          var h = 0
          while (i < n && Character.isLetterOrDigit(text.charAt(i))) { h = 31 * h + VenueNames.fold(text.charAt(i)); i += 1 }
          if (words > 0) {
            if (count == found.length) found = java.util.Arrays.copyOf(found, count * 2)
            found(count) = pair(previous, h); count += 1
          }
          previous = h; words += 1
        } else {
          if (",;/|()[]\r\n".indexOf(c) >= 0) {
            if (words == 1) oneWord = true else if (words >= 2) names += 1
            words = 0
          }
          i += 1
        }
      }
    }
    if (oneWord || names < ListedCast.Names) Array.emptyIntArray else SortedHashes.sortedDistinct(found, count)
  }

  def wordsOf(name: String): Array[Int] = {
    val found = scala.collection.mutable.ArrayBuilder.make[Int]
    var i = 0
    val n = name.length
    while (i < n) {
      if (Character.isLetterOrDigit(name.charAt(i))) {
        var h = 0
        while (i < n && Character.isLetterOrDigit(name.charAt(i))) { h = 31 * h + VenueNames.fold(name.charAt(i)); i += 1 }
        found += h
      } else i += 1
    }
    found.result()
  }
}
