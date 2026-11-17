package deploy

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.FleetDatabase

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** `scripts/mongo-grants.tsv` is the list `scripts/mongo-grants --check` holds prod's users to. A country added
 *  to the model without regenerating it would pass the check while its database had no grant, which fails
 *  silently in prod: both tiers boot and every read comes back `not authorized` (adding-a-country.md, phase 3). */
class MongoGrantsListSpec extends AnyFlatSpec with Matchers {

  "scripts/mongo-grants.tsv" should "be what MongoGrantsList generates from the model today" in {
    withClue(s"stale: run sbt 'worker/Test/runMain deploy.MongoGrantsList' and commit ${MongoGrantsList.Path}\n") {
      RepoFile.read(MongoGrantsList.Path) shouldBe MongoGrantsList.rendered
    }
  }

  "infra/nix/files/country-databases.json" should "be what MongoGrantsList generates from the model today" in {
    withClue(s"stale: run sbt 'worker/Test/runMain deploy.MongoGrantsList' and commit ${MongoGrantsList.CountryDatabasesPath}\n") {
      RepoFile.read(MongoGrantsList.CountryDatabasesPath) shouldBe MongoGrantsList.renderedCountryDatabases
    }
  }

  /** The fleet's per-country lists come from that file, never a hand-written copy: the exporter's collstats list and
   *  the CI reader's grant command each named the five databases literally (2026-11-15 review), the shape that left
   *  the US out of the reader's grants. */
  "the NixOS modules" should "name no country database but through country-databases.json" in {
    val nix = Using.resource(Files.walk(Paths.get("infra/nix")))(_.iterator.asScala.toList)
      .filter(_.toString.endsWith(".nix")).sorted.map(path => path.toString -> RepoFile.read(path.toString))
    nix should not be empty
    // Poland's bare `kinowo` is also the cluster namespace's name (k8s-deploy.nix); every other corpus is `kinowo_<cc>`,
    // so a hand-written list is still caught by the rest of it.
    val corpora = MongoGrantsList.countryDatabases.filter(_.contains("_")).toSet
    val literal = "\"(kinowo_[a-z]{2})\"".r
    val named = for {
      (path, text) <- nix
      found        <- literal.findAllMatchIn(text).map(_.group(1)) if corpora.contains(found)
    } yield s"$path: \"$found\""
    withClue("read the list with `(lib.importJSON ../../files/country-databases.json).databases`: ") {
      named shouldBe empty
    }
    Seq("infra/nix/modules/roles/mongodb-exporter.nix", "infra/nix/modules/roles/mongo-ci-read.nix").foreach { path =>
      RepoFile.read(path) should include("country-databases.json")
    }
  }

  private def databases(who: String, role: String) =
    MongoGrantsList.grants.collect { case (`who`, `role`, db) => db }.toSet

  it should "give the app readWrite on every country's corpus and on the fleet's own database" in {
    databases("app", "readWrite") shouldBe Country.all.map(_.mongoDb).toSet + FleetDatabase.Dedicated.value
  }

  it should "give the CI reader read on the country corpora and nothing else" in {
    databases("ci-read", "read") shouldBe Country.all.map(_.mongoDb).toSet
    MongoGrantsList.grants.filter(_._1 == "ci-read").map(_._2).toSet shouldBe Set("read")
  }
}
