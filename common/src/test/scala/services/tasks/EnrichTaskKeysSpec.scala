package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EnrichTaskKeysSpec extends AnyFlatSpec with Matchers {

  "bulkDedup" should "be a constant per task type so repeat triggers collapse" in {
    EnrichTaskKeys.bulkDedup(TaskType.RefreshAllImdb) shouldBe EnrichTaskKeys.bulkDedup(TaskType.RefreshAllImdb)
    EnrichTaskKeys.bulkDedup(TaskType.RefreshAllImdb) should not be
      EnrichTaskKeys.bulkDedup(TaskType.RefreshAllTmdb)
  }

  "resolveTmdbDedup" should "distinguish films by (title, year) but be stable per film" in {
    EnrichTaskKeys.resolveTmdbDedup("Dune", Some(2024)) shouldBe EnrichTaskKeys.resolveTmdbDedup("Dune", Some(2024))
    EnrichTaskKeys.resolveTmdbDedup("Dune", Some(2024)) should not be EnrichTaskKeys.resolveTmdbDedup("Dune", Some(2021))
    EnrichTaskKeys.resolveTmdbDedup("Dune", None)       should not be EnrichTaskKeys.resolveTmdbDedup("Dune", Some(2024))
  }

  "resolveTmdbPayload" should "round-trip title + year (including a yearless film)" in {
    val withYear = EnrichTaskKeys.resolveTmdbPayload("Dune", Some(2024))
    EnrichTaskKeys.titleOf(withYear) shouldBe "Dune"
    EnrichTaskKeys.yearOf(withYear)  shouldBe Some(2024)

    val noYear = EnrichTaskKeys.resolveTmdbPayload("Untitled", None)
    EnrichTaskKeys.titleOf(noYear) shouldBe "Untitled"
    EnrichTaskKeys.yearOf(noYear)  shouldBe None
  }

  it should "carry the director + originalTitle hints and the force flag when present, and omit them when absent" in {
    val full = EnrichTaskKeys.resolveTmdbPayload("Dune", Some(2024),
      director = Some("Denis Villeneuve"), originalTitle = Some("Dune: Part Two"), force = true)
    EnrichTaskKeys.directorOf(full)      shouldBe Some("Denis Villeneuve")
    EnrichTaskKeys.originalTitleOf(full) shouldBe Some("Dune: Part Two")
    EnrichTaskKeys.forceOf(full)         shouldBe true

    val bare = EnrichTaskKeys.resolveTmdbPayload("Dune", Some(2024))
    EnrichTaskKeys.directorOf(bare)      shouldBe None
    EnrichTaskKeys.originalTitleOf(bare) shouldBe None
    EnrichTaskKeys.forceOf(bare)         shouldBe false
  }

  "the queue" should "collapse a second bulk trigger while the first is active (constant dedup key)" in {
    val queue = new InMemoryTaskQueue
    val key   = EnrichTaskKeys.bulkDedup(TaskType.RefreshAllFilmweb)
    queue.enqueue(TaskType.RefreshAllFilmweb, key) shouldBe EnqueueResult.Added
    queue.enqueue(TaskType.RefreshAllFilmweb, key) shouldBe EnqueueResult.Duplicate
  }

  it should "queue two different films' re-resolves independently" in {
    val queue = new InMemoryTaskQueue
    queue.enqueue(TaskType.ResolveTmdb, EnrichTaskKeys.resolveTmdbDedup("A", None),
      EnrichTaskKeys.resolveTmdbPayload("A", None)) shouldBe EnqueueResult.Added
    queue.enqueue(TaskType.ResolveTmdb, EnrichTaskKeys.resolveTmdbDedup("B", None),
      EnrichTaskKeys.resolveTmdbPayload("B", None)) shouldBe EnqueueResult.Added
  }
}
