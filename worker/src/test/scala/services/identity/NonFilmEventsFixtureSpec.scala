package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.{FamilyAnswers, NonFilmEvents, SourceHit, SourceRecord}
import tools.UnmatchedClusters

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap

/**
 * [[NonFilmEvents]] over the clusters the model leaves unmatched on the recorded full corpora ([[UnmatchedClusters]]'
 * fixture): its precision against `labels.tsv` and the ratchet's right takes — no listing a film is right for is an
 * event — what it finds per country, and what the agreement stage no longer spends on what it finds: the families'
 * questions and the posters it would hash. Writes the events it finds to `target/identity-unmatched/non-film-events.tsv`
 * for review.
 */
class NonFilmEventsFixtureSpec extends AnyFlatSpec with Matchers {

  private val labels   = UnmatchedClusters.readLabels(UnmatchedClusters.Directory.resolve("labels.tsv"))
  private val takes    = UnmatchedClusters.readTakeLines(UnmatchedClusters.Directory.resolve("expected-matches.tsv"))
  private val captures = models.Country.all.map(UnmatchedClusters.fixturePath).filter(Files.exists(_)).map(UnmatchedClusters.read)

  /** Each capture's model decisions whose cluster is a non-film event, with why. */
  private lazy val events: Seq[(UnmatchedClusters.Capture, ResolverDecision, String)] = captures.flatMap { capture =>
    val byKey = capture.listings.map(l => l.key -> l).toMap
    capture.decisions.filter(_.film.isEmpty).flatMap(d => NonFilmEvents.of(d.members.flatMap(byKey.get)).map((capture, d, _)))
  }

  "no listing a label or the ratchet names a right film for" should "be a non-film event" in {
    val right = labels.filter(_.right).map(l => (l.country, l.venue, l.rawTitle)).toSet ++ takes.map(t => (t._1, t._2, t._3))
    val lost = captures.flatMap { capture =>
      capture.listings.filter(l => right((capture.country.code, l.venue, l.rawTitle)) || right((capture.country.code, "*", l.rawTitle)))
        .flatMap(l => NonFilmEvents.of(l).map(why => s"${capture.country.code}\t${l.venue}\t${l.rawTitle}\t$why"))
    }
    info(s"${right.size} right listings checked")
    withClue(lost.mkString("films classed as events:\n", "\n", "\n")) { lost shouldBe empty }
  }

  "the non-film events among the unmatched clusters" should "be found in every country that bills them, and written for review" in {
    val lines = events.flatMap { case (capture, d, why) => d.members.map(m => s"${capture.country.code}\t${m.venue}\t${m.rawTitle}\t$why") }.distinct.sorted
    val out   = Path.of("target", "identity-unmatched", "non-film-events.tsv")
    Files.createDirectories(out.getParent)
    Files.writeString(out, lines.mkString("country\tvenue\trawTitle\twhy\n", "\n", "\n"), StandardCharsets.UTF_8)
    captures.foreach { capture =>
      val unmatched = capture.decisions.count(_.film.isEmpty)
      val found     = events.filter(_._1 eq capture)
      info(s"${capture.country.code}: ${found.size} of $unmatched unmatched clusters are events (${found.map(_._2.members.size).sum} listings): " +
        found.groupBy(_._3).view.mapValues(_.size).toSeq.sortBy(-_._2).map { case (why, n) => s"$why $n" }.mkString(", "))
    }
    info(s"wrote $out")
    events.map(_._1.country.code).toSet should contain allOf ("pl", "uk", "de", "us")
  }

  "the agreement stage" should "ask no family and hash no poster for a non-film event cluster" in {
    val asked   = new ConcurrentHashMap[String, java.lang.Boolean]()
    val hashed  = new ConcurrentHashMap[String, java.lang.Boolean]()
    def counted(answers: FamilyAnswers): FamilyAnswers = new FamilyAnswers {
      val family = answers.family
      def titled(text: String): Answer[Seq[SourceHit]]     = { asked.put(s"${family.label}|title|$text", true); answers.titled(text) }
      def directedBy(name: String): Answer[Seq[SourceHit]] = { asked.put(s"${family.label}|director|$name", true); answers.directedBy(name) }
      def record(id: String): Answer[Option[SourceRecord]] = { asked.put(s"${family.label}|record|$id", true); answers.record(id) }
      override def fresh(question: String): Boolean        = answers.fresh(question)
    }
    def postersCounted(posters: PosterAnswers): PosterAnswers = new PosterAnswers {
      def venue(url: String): Answer[Option[PosterHash]] = { hashed.put(s"venue|$url", true); posters.venue(url) }
      def film(tmdbId: Int): Answer[Seq[PosterHash]]     = { hashed.put(s"film|$tmdbId", true); posters.film(tmdbId) }
    }
    val outcomes = events.groupBy(_._1).toSeq.map { case (capture, found) =>
      capture.country.code -> UnmatchedClusters.replay(capture, found.map(_._2), counted, postersCounted)
    }
    val reported = outcomes.flatMap(_._2.agreed.decisions).count(_.basis == ResolverDecision.Basis.Event)
    val (questions, hashes) = (asked.size, hashed.size)
    info(s"${events.size} event clusters cost $questions family questions and $hashes poster hashes; $reported reported as events")
    // the whole of every capture, for what the stage costs over all its no-matches
    asked.clear(); hashed.clear()
    val allocated = tools.costs.AllocationMeter.once(captures.foreach(UnmatchedClusters.replay(_, families = counted, posters = postersCounted)))
    info(s"all ${captures.map(_.decisions.size).sum} clusters: ${asked.size} family questions, ${hashed.size} poster hashes, " +
      s"${allocated / 1000000} MB allocated replaying them")
    questions shouldBe 0
    hashes shouldBe 0
    reported shouldBe events.size
  }
}
