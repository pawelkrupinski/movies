package tools

import org.scalatest.Assertions

/**
 * A suite that writes to a real Mongo: the target it resolved from its configuration, checked
 * at construction — the suite is CANCELLED when no `MONGODB_URI` is set, and refused outright
 * when the URI names a real, credentialed cluster ([[IntegrationMongo]]).
 *
 * This replaces the three lines every `it/` spec used to open with (an `assume` on the raw
 * variable, the throwaway guard, and a second raw read for the URI) with one mix-in that
 * reads the process once, through [[SuiteConfiguration]].
 */
trait IntegrationMongoSuite extends SuiteConfiguration { this: Assertions =>

  protected final val mongoTarget: IntegrationMongoTarget = {
    val target = IntegrationMongoTarget.from(configuration)
    assume(target.isDefined, "MONGODB_URI not set")
    target.get.requireThrowaway()
    target.get
  }
}
