package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.IdentitySlice

import java.nio.file.Path
import java.time.{Clock, ZoneOffset}

/**
 * A RUNNER-UP ONLY A SIBLING REACHED REFUSES NO OWN MATCH (structural fix 2, the rule burn-down:
 * docs/design/identity-structural-fixes.md). DE "Es" (drift seed 581, identity-flip-backlog rows "Cineplex Neustadt |
 * Es" and its siblings): four venues bill a bare "Es" [72'] (original "It"), the 2017 film their own search ranks first;
 * Caligari Wiesbaden bills "ES (ULRICH SCHAMONI)", whose search brings the 1966 "Es" into the family's pool. The bare
 * title names that film too, so `favoured-calibrated` weighed it as the runner-up -- and the bare listings' 72 minutes
 * fit it better -- whenever Caligari's listing was present and Uferpalast's credited "ES" {Andy Muschietti} absent: the
 * take went 2017 -> none -> 2017 as the family's venues arrived, and beside Caligari alone the bare "Es" joined its
 * cluster and took Schamoni's film. The walk over every population of the family, each resolved whole from the drift
 * slice's own answers (which a Pending take hid from the population walk: it reads a take awaiting an answer as none).
 */
class SiblingReachedRunnerUpSpec extends AnyFlatSpec with Matchers {
  private val dir   = Path.of("test/resources/fixtures/drift-slices/de/cineplex-neustadt-es-37a64e0d")
  private val slice = IdentitySlice.readSlice(dir)
  private val all   = (slice.core ++ slice.context).sorted.distinctBy(_.key)
  private val replaying = new tools.SliceHttp.Replaying(slice.http)
  private val reads     = new IdentitySlice.Reads(slice.country, replaying, replaying.gaps, Clock.fixed(slice.recordedAt, ZoneOffset.UTC),
    slice.filed, tools.ProdState.none)
  private val lookups   = scripts.IdentityDrift.memo(reads)

  private val It2017       = 346364
  private val Schamoni1966 = 133035

  "Every population of the Es family" should "take the 2017 film for every 'Es' listing, and Schamoni's for Caligari's" in {
    val wrong = for {
      mask    <- (1 until (1 << all.size)).toSeq
      present  = all.indices.filter(i => (mask & (1 << i)) != 0).map(all(_))
      resolved = IdentityResolver.resolve(present, lookups, reads.normalizer, ResolveInputs.Unpinned)
      listing <- present
      expected = if (listing.venue.startsWith("Caligari")) Schamoni1966 else It2017
      film     = resolved.decisionOf(listing.key).film
      if !film.contains(expected)
    } yield s"${listing.venue} | ${listing.rawTitle} took ${film.fold("none")("tmdb:" + _)} beside ${present.map(_.venue).mkString("{", ", ", "}")}"
    withClue(s"${wrong.size} take(s) off their film:\n${wrong.take(10).mkString("\n")}\n")(wrong shouldBe empty)
  }
}
