package tools

import services.MongoConnection
import services.movies.MongoMovieRepository

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

/**
 * Dumps the distinct cinema + display titles from the prod `movies` collection
 * into a checked-in fixture, so `ProdTitlesNormalizationSpec` can replay the
 * real long-tail of production titles through the rule engine — the "tests
 * generated from prod db" requirement.
 *
 * Run against prod via the flyctl proxy (read-only):
 *   flyctl proxy 27017:27017 --app kinowo-mongo   # in one shell
 *   set -a; source .env.local; set +a              # MONGODB_URI → 127.0.0.1
 *   sbt 'common/Test/runMain tools.SnapshotProdTitlesToFixture'
 *
 * Re-run whenever you want fresh coverage; commit the regenerated fixture.
 */
object SnapshotProdTitlesToFixture {
  private val Out = Paths.get("common/src/test/resources/fixtures/prod-movies/titles.txt")

  def main(args: Array[String]): Unit = {
    val conn = MongoConnection.fromEnv(required = true)
    val repository = new MongoMovieRepository(conn.database, fallbackToOwnInit = false)
    try {
      val records = repository.findAll()
      val titles = records
        .flatMap(r => r.record.cinemaTitles + r.title)
        .map(_.trim).filter(_.nonEmpty).distinct.sorted
      Files.createDirectories(Out.getParent)
      Files.write(Out, titles.asJava)
      println(s"Wrote ${titles.size} distinct prod titles from ${records.size} records to $Out")
    } finally {
      repository.close()
      conn.close()
    }
  }
}
