package deploy

import models.Country
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks the ONE database the four web deployments keep their accounts in.
 *
 * `models.Country.usersDbName` reads `MONGODB_USERS_DB` and falls back to the
 * pod's own per-country database, which is the right default for a country
 * deployed alone and the wrong one here: the three Showtimes countries share an
 * origin, so a session cookie minted under `/uk` arrives at `/de`, and against a
 * per-country `users` collection its `userId` resolves to nobody. The visitor is
 * signed out with their hidden films and /plan picks apparently gone.
 *
 * That makes the value unreachable from every running-JVM layer — `Wiring`
 * captures it once at boot from an env var this repo only sets in a ConfigMap —
 * and silent when wrong: nothing errors, every page renders, and the only symptom
 * is an account that does not exist one country over. Same reasoning as
 * [[WorkerScrapeCadenceConfigSpec]], which is why this lives beside it rather
 * than in `web`: this package, and `RepoFile` with it, is where the deploy-config
 * locks are.
 *
 * The mechanism is covered by `CountrySpec` and `WiringUsersConnectionSpec`; this
 * covers the deployed VALUE.
 */
class SharedUsersDatabaseConfigSpec extends AnyFlatSpec with Matchers {

  private val WebBase  = RepoFile.read("infra/kubernetes/web/base/all.yaml")

  /** Poland's own database, which is also everyone's accounts database. A
   *  dedicated `kinowo_users` was the first choice and could not be written: the
   *  deployments' Mongo user is scoped to the four country databases, so every
   *  upsert came back `Unauthorized`. See the constraint test below. */
  private val Expected = Country.Poland.mongoDb

  /** `MONGODB_USERS_DB` as a deploy config actually SETS it — comment lines
   *  dropped, because this key spent a release commented out with its cutover
   *  instructions beside it, and a spec that reads those back would pass on a
   *  file that configures nothing. */
  private def usersDbIn(text: String): Option[String] =
    text.linesIterator
      .map(_.trim)
      .filterNot(_.startsWith("#"))
      .collectFirst { case s"MONGODB_USERS_DB:$rest" => rest.trim.replaceAll("^['\"]|['\"]$", "") }
      .filter(_.nonEmpty)

  "The web tier" should "send every deployment's accounts to one named database" in {
    usersDbIn(WebBase) shouldBe Some(Expected)
  }

  // THE CONSTRAINT THAT BIT, encoded so it cannot bite twice. The pods share one
  // Mongo credential and it has rights on the four country databases and nothing
  // else, so naming a database outside that set does not fail at boot — it fails
  // one swallowed `Unauthorized` at a time, on every account write, while the
  // site carries on rendering.
  it should "name a database the deployments' Mongo user already has rights on" in {
    Country.all.map(_.mongoDb) should contain (usersDbIn(WebBase).value)
  }

  // THE POINT OF IT BEING IN THE BASE. An overlay that set its own value would
  // split the accounts back apart exactly as if the key were missing, and would
  // do it for one country only — the hardest version of this bug to see, since
  // three countries would keep working.
  it should "let no country's overlay point somewhere else" in {
    Country.all.foreach { country =>
      val overlay = scala.util.Try(RepoFile.read(s"infra/kubernetes/web/overlays/${country.code}/patch.yaml")).toOption
      withClue(s"${country.code} overlay overrides MONGODB_USERS_DB: ") {
        overlay.flatMap(usersDbIn) shouldBe empty
      }
    }
  }

  // The rows were folded in by `tools.SharedUsersMigration` before the key was
  // set; pointing a pod at a database nobody filled shows every visitor a blank
  // account. Keeping the command next to the value is what makes the re-run —
  // to sweep up anyone who signed in during the window — findable later.
  it should "keep the migration that fills it recorded beside it" in {
    WebBase should include ("tools.SharedUsersMigration")
  }
}
