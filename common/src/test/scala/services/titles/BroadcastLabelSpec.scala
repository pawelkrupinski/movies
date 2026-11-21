package services.titles

import java.util.Locale

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[NonMovieEventClassifier.isBroadcastLabel]]: the label test every candidate record asks of each work it bills
 *  (`Film.billedWorksBesideLabels`), so of every film the corpus context holds at a worker's take-up. Its answers are
 *  the frozen reading's below, which split every marker into words again on each call -- 60% of a DE take-up's corpus
 *  context build (JFR 2026-11-21, docs/design/corpus-context-persistence.md). */
class BroadcastLabelSpec extends AnyFlatSpec with Matchers {

  /** The reading before the markers were split once, frozen. */
  private def frozen(piece: String): Boolean = {
    def words(s: String) = s.toLowerCase(Locale.ROOT).split("""[^\p{L}\p{N}]+""").filter(_.nonEmpty).toSeq
    val own = words(piece)
    own.nonEmpty && NonMovieEventClassifier.NeutralBroadcastVeto.exists(marker => words(marker) == own)
  }

  /** Every marker as a piece would bill it -- as written, shouted, title-cased, punctuated, padded, with a word more and a
   *  word less -- and pieces no marker reads. */
  private val pieces: Seq[String] = {
    val markers = NonMovieEventClassifier.NeutralBroadcastVeto
    markers.flatMap { m =>
      val words = m.trim.split(' ').toSeq
      Seq(m, m.toUpperCase(Locale.ROOT), words.map(_.capitalize).mkString(" "), words.mkString(": "), s"  $m  ", s"$m!",
        s"$m 2026", s"the $m", words.drop(1).mkString(" "), words.mkString("-"))
    } ++ Seq("", " ", "--", "Live", "LIVE", "The Metamorphosis", "Hamlet", "BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES",
      "Met", "Royal", "Opera", "Teatr", "Kinie", "Ölüm", "Zażółć gęślą jaźń", "2026", "NT", "N.T. Live")
  }

  "isBroadcastLabel" should "read every piece as the frozen reading did" in {
    pieces.size should be > 100
    pieces.foreach(p => withClue(s"'$p': ")(NonMovieEventClassifier.isBroadcastLabel(p) shouldBe frozen(p)))
  }

  it should "split the markers once, not on every ask" in {
    assume(tools.ThreadAllocation.supported, "this JVM cannot say what a thread allocates")
    val asks = Iterator.continually(pieces).flatten.take(20000).toSeq
    def allocated(label: String => Boolean): Long = { asks.foreach(label); tools.ThreadAllocation.of(asks.foreach(label))._2 }
    val before = allocated(frozen)
    val now    = allocated(NonMovieEventClassifier.isBroadcastLabel)
    info(s"${asks.size} asks allocated ${now / 1000} kB, the frozen reading ${before / 1000} kB")
    now should be < before / 5
  }
}
