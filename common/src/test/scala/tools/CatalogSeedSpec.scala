package tools

import models.Catalog
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

/**
 * Guards the checked-in mobile catalog SEED files (iOS + Android) against the
 * live [[models.Catalog]] render. The apps ship these so a fresh install renders
 * offline AND its first `/api/catalog` fetch already carries the current ETag
 * (→ a 304 with no body whenever the build is up to date with the server).
 *
 * The seed is `{"etag":<server ETag>,"catalog":<server body>}` — the ETag lets
 * the app send `If-None-Match` before it has ever fetched, and `catalog` is the
 * exact `{countries,cities}` body a 200 would return, so the app decodes both
 * paths the same way.
 *
 * When a country or city changes, this spec rewrites the seeds and FAILS with a
 * regenerate message (same convention as the read-model / OG-card snapshots).
 * Re-run to confirm stability, then commit the updated files.
 */
class CatalogSeedSpec extends AnyFlatSpec with Matchers {

  private val expected: String = {
    val etagField = "\"" + Catalog.etag.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    s"""{"etag":$etagField,"catalog":${Catalog.json}}"""
  }

  private val seeds = Seq(
    "ios/Kinowo/catalog-seed.json",
    "android/app/src/main/assets/catalog-seed.json",
  )

  // The tests run from an unspecified working directory; walk up to the repo
  // root (the dir holding build.sbt) so the seed paths resolve regardless.
  private lazy val repoRoot: File = {
    var dir = new File(".").getCanonicalFile
    while (dir != null && !new File(dir, "build.sbt").exists()) dir = dir.getParentFile
    Option(dir).getOrElse(sys.error(s"repo root (build.sbt) not found from ${new File(".").getCanonicalPath}"))
  }

  seeds.foreach { rel =>
    s"The bundled catalog seed $rel" should "match the live Catalog render" in {
      val file    = new File(repoRoot, rel)
      val current = if (file.exists()) new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8) else null
      if (current != expected) {
        file.getParentFile.mkdirs()
        Files.write(file.toPath, expected.getBytes(StandardCharsets.UTF_8))
        fail(s"Catalog seed $rel was missing/stale — rewrote it. Re-run to confirm, then commit the updated file.")
      }
      current shouldBe expected
    }
  }
}
