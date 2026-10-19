package models

import tools.Env

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UsersDatabaseSpec extends AnyFlatSpec with Matchers {

  /** The `users` + `userStates` collections are the ONE thing four country
   *  deployments must agree on: a person is not per country. `MONGODB_USERS_DB`
   *  names the database holding them, and the fallback is what keeps the change
   *  a no-op everywhere it is not set. */
  "UsersDatabase.nameFrom" should "send the user collections to MONGODB_USERS_DB when it names one" in {
    UsersDatabase.nameFrom(Some("kinowo_users"), "kinowo_uk") shouldBe "kinowo_users"
    UsersDatabase.nameFrom(Some("kinowo_users"), "kinowo_de") shouldBe "kinowo_users"
  }

  it should "give every country the SAME users database, whatever its own is" in {
    val dbs = Country.all.map(c => UsersDatabase.nameFrom(Some("kinowo_users"), c.mongoDb))
    dbs.distinct shouldBe List("kinowo_users")
  }

  it should "leave them in the deployment's own database when it is unset" in {
    UsersDatabase.nameFrom(None, "kinowo_uk") shouldBe "kinowo_uk"
    UsersDatabase.nameFrom(None, "kinowo")    shouldBe "kinowo"
  }

  // Blank is how the setting gets switched OFF in a ConfigMap — the key stays,
  // the value empties. Treating it as a database literally called "" would defer
  // the failure to the first query instead of never happening at all.
  it should "treat a blank or whitespace-only value as unset" in {
    UsersDatabase.nameFrom(Some(""),    "kinowo_de") shouldBe "kinowo_de"
    UsersDatabase.nameFrom(Some("   "), "kinowo_de") shouldBe "kinowo_de"
  }

  it should "trim a value that arrived with stray whitespace" in {
    UsersDatabase.nameFrom(Some(" kinowo_users "), "kinowo_de") shouldBe "kinowo_users"
  }

  "UsersDatabase.name" should "read MONGODB_USERS_DB from the environment" in {
    UsersDatabase.name(new settings.ProcessConfiguration(Env.of("MONGODB_USERS_DB" -> "kinowo_users_probe")).usersDatabase,
      settings.MongoDatabaseName(Country.Germany.mongoDb)) shouldBe settings.MongoDatabaseName("kinowo_users_probe")
  }

  // The deployment's own database is handed in, never re-derived from KINOWO_COUNTRY: the
  // serving country is the wiring's to know, and a second read of the environment is how a
  // test wiring booted as one country used to reach another's database.
  it should "fall back to the deployment's own database it is handed when it is unset" in {
    UsersDatabase.name(new settings.ProcessConfiguration(Env.of("KINOWO_COUNTRY" -> "uk")).usersDatabase,
      settings.MongoDatabaseName(Country.Germany.mongoDb)) shouldBe settings.MongoDatabaseName(Country.Germany.mongoDb)
  }
}
