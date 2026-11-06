package services.review

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

import java.nio.file.{Files, Path, Paths}
import java.time.Instant

/** The review digest's items: which judgements and candidates await the user, and what an answer to one does to the
 *  judgements file. */
class ReviewDigestSpec extends AnyFlatSpec with Matchers with tools.ScratchDirectories {

  private val at = Instant.parse("2026-10-23T08:00:00Z")

  private def row(venue: String, raw: String, film: String, verdict: String, evidence: String, filmTitle: String = "") =
    LabelRow("de", venue, raw, film, verdict, evidence, filmTitle = filmTitle, judged = "2026-10-22")

  private val agentRose   = row("KiKuBi", "Rose", "tmdb:846864", "right", "agent judged 2026-10-22 (prodreg rose): kinoheld credits Aurélie Saada")
  private val agentRoseNo = row("KiKuBi", "Rose", "tmdb:1176962", "wrong", "agent judged 2026-10-22 (prodreg rose): Schleinzer's film")
  private val blade       = row("Metropolis Bochum", "Blade", "tmdb:36647", "right",
    "judged 10-22 (premiere bound, AGENT-JUDGED, flagged for the user): the venue's Cineamo id", "Blade (1998)")
  private val hayFever    = LabelRow("uk", "*", "Screening: NTL - Hay Fever", "tmdb:1", "right", "judged 10-22 (agent, parser-query): NT Live's relay")
  private val confirmed   = row("Patrick", "Patrick Monahan", "tmdb:2", "wrong",
    "review page: wrong: Patrick Monahan: Live (2011) (was: wrong: agent judged 2026-10-22: a stand-up night)")
  private val plain       = row("Abaton", "Enjô", "tmdb:97149", "right", "judged 10-13: Ichikawa's Enjō (1958)")

  private def candidate(cc: String, venue: String, raw: String, film: Int) =
    DigestItem(cc, venue, raw, FilmRef.tmdb(film), Some("Short (2026)"), None, "2026-10-23", "looks wrong: a short's record")

  private val dads      = candidate("us", "Rialto Cinemas Sebastopol", "The Dads", 1073659)
  private val latcho    = candidate("de", "Raststätte", "Latcho Drom", 1247239)
  private val judgedOne = candidate("de", "Abaton", "Enjô", 97149)      // a row judges it already
  private val agentsOwn = candidate("de", "KiKuBi", "Rose", 846864)     // the agent's row stands for it

  "the digest" should "list every agent judgement the user has not confirmed, by each spelling of the marker, and the candidates no row judges" in {
    val items = ReviewDigest.pending(Seq(agentRose, agentRoseNo, blade, hayFever, confirmed, plain), Seq(dads, latcho, judgedOne, agentsOwn), Set.empty)
    items.map(i => (i.country, i.rawTitle, i.film.render, i.agentVerdict)) shouldBe Seq(
      ("de", "Blade", "tmdb:36647", Some("right")),
      ("de", "Latcho Drom", "tmdb:1247239", None),
      ("de", "Rose", "tmdb:1176962", Some("wrong")),
      ("de", "Rose", "tmdb:846864", Some("right")),
      ("uk", "Screening: NTL - Hay Fever", "tmdb:1", Some("right")),
      ("us", "The Dads", "tmdb:1073659", None))
    items.find(_.rawTitle == "Blade").get.shown shouldBe FilmFacts(FilmRef.tmdb(36647), Some("Blade"), Some(1998))
  }

  it should "give one listing's two films two items, each answered on its own" in {
    val items = ReviewDigest.pending(Seq(agentRose, agentRoseNo), Nil, Set.empty)
    items.map(_.id).distinct should have size 2
    val first = answerTo(items.head, ReviewVerdict.Right, at)
    val second = answerTo(items(1), ReviewVerdict.Wrong, at.plusSeconds(1))
    ReviewAnswers.current(Seq(first, second)) shouldBe Seq(first, second)
    // an Undo of one leaves the other standing
    ReviewAnswers.current(Seq(first, second, answerTo(items.head, ReviewVerdict.Undo, at.plusSeconds(2)))) shouldBe Seq(second)
    ReviewDigest.pending(Seq(agentRose, agentRoseNo), Nil, ReviewDigest.answered(Seq(second))).map(_.film) shouldBe Seq(FilmRef.tmdb(1176962))
  }

  "an answer on the digest" should "make a confirmed agent judgement the user's, flip an overruled one, and add a candidate's row" in {
    withLabels(Seq(agentRose, agentRoseNo, plain)) { path =>
      val items  = ReviewDigest.pending(LabelsTsv.read(path), Seq(latcho), Set.empty)
      def item(film: Int) = items.find(_.film == FilmRef.tmdb(film)).get
      val history = Seq(answerTo(item(846864), ReviewVerdict.Right, at), answerTo(item(1176962), ReviewVerdict.Right, at.plusSeconds(1)),
        answerTo(item(1247239), ReviewVerdict.Event, at.plusSeconds(2)))
      val summary = LabelsExport.exportTo(path, history)
      (summary.confirmed, summary.flipped, summary.added) shouldBe ((1, 1, 1))
      val rows = LabelsTsv.read(path)
      rows.map(r => (r.rawTitle, r.film, r.verdict, r.note)) shouldBe Seq(
        ("Rose", "tmdb:846864", "right", s"review page: right: tmdb:846864 (was: right: ${agentRose.note})"),
        ("Rose", "tmdb:1176962", "right", s"review page: right: tmdb:1176962 (was: wrong: ${agentRoseNo.note})"),
        ("Enjô", "tmdb:97149", "right", plain.note),
        ("Latcho Drom", "tmdb:1247239", "wrong", "review page: not a film: Short (2026)"))
      ReviewDigest.pending(rows, Seq(latcho), Set.empty) shouldBe empty
      // the confirmation undone: the agent's row is the agent's again, and back on the digest
      LabelsExport.exportTo(path, history :+ answerTo(item(846864), ReviewVerdict.Undo, at.plusSeconds(3)))
      LabelsTsv.read(path).find(_.film == "tmdb:846864").get.note shouldBe agentRose.note
      ReviewDigest.pending(LabelsTsv.read(path), Nil, Set.empty).map(_.film) shouldBe Seq(FilmRef.tmdb(846864))
    }
  }

  it should "touch the row at its own venue before another venue's of the same film" in {
    val star = row("*", "Rose", "tmdb:1712584", "wrong", "agent judged 2026-10-22 (prodreg rose): Babayan's French Rose")
    val own  = row("Metropol Düsseldorf", "Rose", "tmdb:1712584", "right", "agent judged 2026-10-22 (prodreg rose): kinoheld credits Babayan")
    withLabels(Seq(own, star)) { path =>
      val item = ReviewDigest.pending(LabelsTsv.read(path), Nil, Set.empty).find(_.venue == "*").get
      LabelsExport.exportTo(path, Seq(answerTo(item, ReviewVerdict.Wrong, at))).confirmed shouldBe 1
      LabelsTsv.read(path).map(r => (r.venue, r.verdict, ReviewDigest.userJudged(r.note))) shouldBe
        Seq(("Metropol Düsseldorf", "right", false), ("*", "wrong", true))
    }
  }

  it should "tell agent judgements from candidates, and name the other judgements of a billing" in {
    val items = ReviewDigest.pending(Seq(agentRose, agentRoseNo, blade), Seq(latcho), Set.empty)
    ReviewDigest.ofKind(items, Some("agent")).map(_.rawTitle) shouldBe Seq("Blade", "Rose", "Rose")
    ReviewDigest.ofKind(items, Some("candidate")).map(_.rawTitle) shouldBe Seq("Latcho Drom")
    ReviewDigest.ofKind(items, None) shouldBe items
    ReviewDigest.otherJudgements(Seq(agentRose, agentRoseNo, blade), items.find(_.film == FilmRef.tmdb(846864)).get) shouldBe Seq(agentRoseNo)
  }

  it should "take from the judge cards the blocks naming its billing and film" in {
    val cards = "# cards\n\n## Rose x tmdb:846864\nright\n\n## Rose x tmdb:1176962\nwrong\n\n## Blade x tmdb:846864\nother billing"
    val item  = ReviewDigest.pending(Seq(agentRose), Nil, Set.empty).head
    ReviewDigest.judgeCard(cards, item) shouldBe "Rose x tmdb:846864\nright"
    ReviewDigest.judgeCard("", item) shouldBe ""
    ReviewDigest.judgeCardsPath(Paths.get("/w/test/resources/fixtures/identity-judgements.tsv")) shouldBe Paths.get("/w/target/identity-judge/cards.md")
    // a labels file near the filesystem root has no checkout above it: no failure, only no cards there
    ReviewDigest.judgeCardsPath(Paths.get("/tmp/identity-judgements.tsv")) shouldBe Paths.get("/target/identity-judge/cards.md")
  }

  it should "reopen a known-wrong row the user overrules to right, so the judgements lint still holds" in {
    val known = agentRoseNo.copy(state = "known-wrong", scope = "rows", reason = "kept")
    withLabels(Seq(known)) { path =>
      val item = ReviewDigest.pending(LabelsTsv.read(path), Nil, Set.empty).head
      LabelsExport.exportTo(path, Seq(answerTo(item, ReviewVerdict.Right, at)))
      LabelsTsv.read(path).map(r => (r.verdict, r.state, r.scope, r.reason)) shouldBe Seq(("right", "open", "", ""))
    }
  }

  it should "put each item on a card of its own listing, the film judged put forward and the film the model took among its candidates" in {
    import services.identity.ResolverDecision
    import services.movies.ListingKey
    val hall    = ListingKey.Published("KiKuBi", "Rose", None, Nil)
    val sibling = ListingKey.Published("Other Kino", "Rose", None, Nil)
    val decision = ResolverDecision(Seq(hall, sibling), Some(1176962), 0.9, ResolverDecision.Basis.OwnMatch, Seq("own match 1176962 at 90.0%"))()
    val item = ReviewDigest.pending(Seq(agentRose), Nil, Set.empty).head   // judged right: tmdb:846864
    val card = ReviewDigest.clusterOf(models.Country.Germany, item, Seq(decision))
    card.members shouldBe Seq(hall)                       // that listing alone: an answer labels no sibling
    card.id shouldBe item.id
    card.shown shouldBe Some(846864)
    card.candidates.map(_.film) shouldBe Seq(846864, 1176962)  // the film judged, then the one the model took
    card.explanation shouldBe decision.explanation
    // the model's own take is the film judged: it stays the match
    ReviewDigest.clusterOf(models.Country.Germany, item.copy(film = FilmRef.tmdb(1176962)), Seq(decision)).film shouldBe Some(1176962)
    // a listing the model holds no decision for still gets a card
    val bare = ReviewDigest.clusterOf(models.Country.Germany, item, Nil)
    (bare.members.map(_.venue), bare.shown) shouldBe ((Seq("KiKuBi"), Some(846864)))
  }

  "the checked-in digest" should "read every candidate, and list the agent judgements and candidates awaiting the user" in {
    val labels = Paths.get("test/resources/fixtures/identity-judgements.tsv")
    val root   = Iterator.iterate(Paths.get("").toAbsolutePath)(_.getParent).takeWhile(_ != null)
      .find(d => Files.exists(d.resolve(labels))).get
    val candidates = ReviewDigest.readCandidates(ReviewDigest.candidatesPath(root.resolve(labels)))
    candidates.map(c => (c.country, c.rawTitle)) should contain allOf (("de", "Latcho Drom"), ("de", "Huis clos"), ("de", "Snake Eyes"), ("us", "The Dads"))
    candidates.count(_.rawTitle == "The Dads") shouldBe 3
    ReviewDigest.pending(LabelsTsv.read(root.resolve(labels)), candidates, Set.empty).count(_.agentVerdict.isDefined) should be > 0
  }

  /** The answer the digest page posts for `item`: its payload, as the answer route parses it. */
  private def answerTo(item: DigestItem, verdict: ReviewVerdict, when: Instant): ReviewAnswer =
    AnswerRequest.parse(Json.obj("card" -> item.payload, "verdict" -> verdict.code), "dev", when).toOption.get

  private def withLabels(rows: Seq[LabelRow])(body: Path => Any): Unit = {
    val dir  = tempDirectory("digest")
    val path = dir.resolve("identity-judgements.tsv")
    LabelsTsv.write(path, rows)
    try body(path): Unit
    finally { Files.list(dir).forEach(Files.delete(_)); Files.delete(dir) }
  }
}
