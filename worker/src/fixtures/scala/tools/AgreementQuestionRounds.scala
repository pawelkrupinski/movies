package tools

import services.identity.{AgreementQuestions, FamilyAnswerStore, FamilySource}
import services.identity.agreement.VoterFamily

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**
 * The agreement's open family questions asked of their live sources round after round — what the unmatched-cluster
 * capture and fill (`integration.UnmatchedClustersCapture`, `integration.UnmatchedClustersFill`) run until the
 * stage asks nothing more: each round asks what the stage wants — at most `perFamily` at a time per family, every
 * family at once ([[ConcurrentAsks]]) — files the answers in `store`, and agrees again.
 *
 * A question whose ask failed [[AgreementQuestionRounds.Attempts]] times is given up on as UNREACHABLE and asked no
 * more: a host that blackholes the asker for one URL (the GitHub runner's US capture of 2026-10-12: Wikidata's search
 * for "SANGGEUN PARK" and two Rotten Tomatoes records timed out on all twelve rounds, while answering in under two
 * seconds from elsewhere) never answers it however often it is asked. It is filed as nothing — no answer, never "no
 * film" — so the replay serves it as production meets a failed fetch: the question open, the cluster waiting on it;
 * the fixture names it ([[UnmatchedClusters.Capture.unreachable]]) so the ratchet knows it was asked, and the next
 * capture, holding no answer, asks it again.
 */
final class AgreementQuestionRounds(store: FamilyAnswerStore, sources: Map[VoterFamily, FamilySource], label: String,
                                    attempts: Int = AgreementQuestionRounds.Attempts, maxRounds: Int = AgreementQuestionRounds.MaxRounds,
                                    perFamily: Int = 4) {
  private val failures = new ConcurrentHashMap[(VoterFamily, String), Integer]()

  /** The questions given up on: asked [[attempts]] times, never answered. */
  // the keys, not `collect` over the map: a question is itself a pair, so that would build a map keyed by family and keep
  // one question of each (the PL capture of 2026-10-12 re-asked ~25 given-up questions on all 24 rounds)
  def unreachable: Set[(VoterFamily, String)] = failures.asScala.filter(_._2 >= attempts).keySet.toSet

  /** Rounds from `agree()`'s outcome until it wants no question still worth asking (`wanted`, less the unreachable)
   *  and nothing else (`pending`: posters, catalogue, finds — `beside` answers those each round), or [[maxRounds]]. */
  def run[O](agree: () => O)(wanted: O => Set[(VoterFamily, String)], pending: O => Boolean = (_: O) => false,
                             beside: O => Unit = (_: O) => ()): AgreementQuestionRounds.Asked[O] = {
    var outcome = agree()
    var rounds  = 0
    def askable = wanted(outcome) -- unreachable
    while ((askable.nonEmpty || pending(outcome)) && rounds < maxRounds) {
      rounds += 1
      val open = askable.toSeq
      println(s"[$label] round $rounds: ${open.size} open family question(s)" +
        (if (unreachable.isEmpty) "" else s", ${unreachable.size} given up as unreachable"))
      beside(outcome)
      ConcurrentAsks.inLanes(open.groupBy(_._1).values, perFamily)(question => ask(question._1, question))
      outcome = agree()
    }
    val givenUp = unreachable
    if (givenUp.nonEmpty) println(s"[$label] ${givenUp.size} question(s) unreachable after $attempts attempts, left open for the next capture: " +
      givenUp.toSeq.sortBy { case (family, question) => (family.ordinal, question) }.map { case (family, question) => s"${family.label} '$question'" }.mkString(", "))
    AgreementQuestionRounds.Asked(outcome, rounds, givenUp, wanted(outcome) -- givenUp)
  }

  private def ask(family: VoterFamily, question: (VoterFamily, String)): Unit =
    try { AgreementQuestions.file(store, family, sources(family), question._2); () }
    catch { case NonFatal(e) =>
      val n = failures.merge(question, 1, (a, b) => a + b)
      println(s"[$label] ${family.label} '${question._2}' failed (attempt $n of $attempts): ${e.getMessage}")
    }
}

object AgreementQuestionRounds {
  /** How many failed asks give a question up: one round's timeout is a slow host, three running are one that will not answer. */
  val Attempts  = 3
  /** The most rounds a capture runs: an answer can open new questions, so the stage's chain decides how many it needs. */
  val MaxRounds = 24

  /** The rounds' last outcome, how many ran, the questions given up as unreachable, and those still `open` — wanted,
   *  not given up: what stopping at [[MaxRounds]] left unasked. */
  final case class Asked[O](outcome: O, rounds: Int, unreachable: Set[(VoterFamily, String)], open: Set[(VoterFamily, String)])
}
