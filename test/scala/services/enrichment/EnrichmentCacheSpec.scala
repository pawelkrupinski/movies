package services.enrichment

import models.Enrichment
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class EnrichmentCacheSpec extends AnyFlatSpec with Matchers {

  // Fake repo: in-memory store with the same write-through contract as the
  // real EnrichmentRepo. We bypass the Mongo connection entirely by
  // overriding the methods the cache calls.
  private class FakeRepo(seed: Seq[(String, Option[Int], Enrichment)] = Seq.empty)
      extends EnrichmentRepo {
    private val store = mutable.LinkedHashMap.empty[(String, Option[Int]), Enrichment]
    val upserts = mutable.ListBuffer.empty[(String, Option[Int], Enrichment)]
    val deletes = mutable.ListBuffer.empty[(String, Option[Int])]
    seed.foreach { case (t, y, e) => store.put((t, y), e) }
    override def enabled: Boolean = true
    override def findAll(): Seq[(String, Option[Int], Enrichment)] =
      store.iterator.map { case ((t, y), e) => (t, y, e) }.toSeq
    override def upsert(t: String, y: Option[Int], e: Enrichment): Unit = {
      store.put((t, y), e)
      upserts.append((t, y, e))
    }
    override def delete(t: String, y: Option[Int]): Unit = {
      store.remove((t, y))
      deletes.append((t, y))
    }
  }

  private def mkEnrichment(imdbId: String, rating: Option[Double] = None): Enrichment =
    Enrichment(imdbId = imdbId, imdbRating = rating, metascore = None, originalTitle = None)

  "EnrichmentCache" should "hydrate from the repo on construction" in {
    val seed = Seq(("Drzewo Magii", Some(2024), mkEnrichment("tt1")))
    val cache = new EnrichmentCache(new FakeRepo(seed))

    cache.get(cache.keyOf("Drzewo Magii", Some(2024))) shouldBe Some(mkEnrichment("tt1"))
    cache.snapshot().map { case (t, y, _) => (t, y) } shouldBe Seq(("Drzewo Magii", Some(2024)))
  }

  it should "treat case + diacritics + whitespace differences as the same key" in {
    val cache = new EnrichmentCache(new FakeRepo())
    cache.put(cache.keyOf("Drzewo Magii", Some(2024)), mkEnrichment("tt9"))

    // Different spellings should collapse to the same row.
    cache.get(cache.keyOf("drzewo magii",       Some(2024))) shouldBe Some(mkEnrichment("tt9"))
    cache.get(cache.keyOf("DRZEWO   MAGII",     Some(2024))) shouldBe Some(mkEnrichment("tt9"))
    // Different year is a different row.
    cache.get(cache.keyOf("Drzewo Magii",       Some(2025))) shouldBe None
  }

  "put" should "write through to the repo (cache + Mongo stay in lockstep)" in {
    val repo  = new FakeRepo()
    val cache = new EnrichmentCache(repo)
    cache.put(cache.keyOf("X", Some(2024)), mkEnrichment("tt1"))

    repo.upserts.toList shouldBe List(("X", Some(2024), mkEnrichment("tt1")))
  }

  "invalidate" should "remove from both positive cache and repo" in {
    val repo  = new FakeRepo()
    val cache = new EnrichmentCache(repo)
    val key   = cache.keyOf("X", Some(2024))
    cache.put(key, mkEnrichment("tt1"))
    repo.upserts.clear()

    cache.invalidate(key)

    cache.get(key) shouldBe None
    repo.deletes.toList shouldBe List(("X", Some(2024)))
  }

  "markMissing + isNegative" should "let callers track known-non-films without polluting the positive cache" in {
    val cache = new EnrichmentCache(new FakeRepo())
    val key   = cache.keyOf("not-a-real-film", Some(2099))

    cache.isNegative(key) shouldBe false
    cache.markMissing(key)
    cache.isNegative(key) shouldBe true
    cache.get(key) shouldBe None  // negative cache never returns a positive value
  }

  "snapshot" should "return rows sorted by title (case-insensitive)" in {
    val cache = new EnrichmentCache(new FakeRepo())
    cache.put(cache.keyOf("Zorro", None),   mkEnrichment("tt3"))
    cache.put(cache.keyOf("alpha", None),   mkEnrichment("tt1"))
    cache.put(cache.keyOf("Beta", None),    mkEnrichment("tt2"))

    cache.snapshot().map { case (t, _, _) => t } shouldBe Seq("alpha", "Beta", "Zorro")
  }
}
