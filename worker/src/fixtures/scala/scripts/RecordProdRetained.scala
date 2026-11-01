package scripts

import models.Country
import org.mongodb.scala.MongoClient
import tools.{ProdLookupInputs, ProdRetained}

import java.nio.file.Paths
import java.time.Instant

/**
 * A recording's capture of the listings prod's identity model kept for their facts alone ([[ProdRetained]]), made after
 * the fact: for a recording made before the recorder captured them, read off the LOCAL prod mirror as they stood at the
 * recording's time (`--at`, its corpus read; the history's entries after it are left out), written beside the corpus
 * under `--root` (default test/resources/fixtures). What `RecordCorpusFixture` captures at record time, by the same read.
 *
 *   sbt "worker/Fixtures/runMain scripts.RecordProdRetained de --at 2026-11-01T02:28:06Z"
 */
object RecordProdRetained {
  def main(args: Array[String]): Unit = {
    val rest    = args.toList
    def opt(name: String) = rest.sliding(2).collectFirst { case List(`name`, value) => value }
    val country = rest.headOption.flatMap(cc => Country.all.find(_.code == cc)).getOrElse(sys.error("usage: <cc> --at <instant> [--root DIR] [--mongo URI]"))
    val at      = Instant.parse(opt("--at").getOrElse(sys.error("--at <instant>: the recording's time")))
    val root    = opt("--root").map(Paths.get(_)).getOrElse(ProdLookupInputs.Root)
    val client  = MongoClient(opt("--mongo").getOrElse("mongodb://127.0.0.1:28017/?directConnection=true"))
    try {
      val db   = client.getDatabase(services.MongoConnection.mirrorDbFor(country.mongoDb))
      val kept = ProdRetained.read(db, at.toEpochMilli)
      val file = ProdRetained.write(root, country.code, kept)
      println(f"[retained] wrote $file%s — ${kept.size}%d listing(s) prod's model held at $at, ${java.nio.file.Files.size(file) / 1024.0}%.0f KB")
    } finally client.close()
  }
}
