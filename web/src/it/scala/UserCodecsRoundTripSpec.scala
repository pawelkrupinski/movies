package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.users.UserCodecs
import tools.persistence.PersistedRoundTrip

/** `UserCodecs`' half of `PersistedCodecsRoundTripSpec` (worker/src/it): every type the
 *  registry writes, fully populated, through the real codec and a real Mongo and back
 *  unchanged. The list comes from the registry, so a new field or type needs no edit. */
class UserCodecsRoundTripSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  "UserCodecs" should "write and read back every persisted type unchanged" in {
    val (covered, findings) = PersistedRoundTrip.registry[UserCodecs.OmittingNone, UserCodecs.WritingNone](mongoTarget, UserCodecs.registry, Set.empty)
    withClue(s"round-tripped ${covered.mkString(", ")}:\n  ${findings.mkString("\n  ")}\n")(findings shouldBe empty)
  }
}
