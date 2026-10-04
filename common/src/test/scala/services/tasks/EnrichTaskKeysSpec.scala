package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EnrichTaskKeysSpec extends AnyFlatSpec with Matchers {

  "bulkDedup" should "be a constant per task type so repeat triggers collapse" in {
    EnrichTaskKeys.bulkDedup(TaskType.RefreshAllImdb) shouldBe EnrichTaskKeys.bulkDedup(TaskType.RefreshAllImdb)
    EnrichTaskKeys.bulkDedup(TaskType.RefreshAllImdb) should not be
      EnrichTaskKeys.bulkDedup(TaskType.RefreshAllRt)
  }

  "resolveImdbIdDedup" should "distinguish films by (title, year) but be stable per film" in {
    EnrichTaskKeys.resolveImdbIdDedup("Dune", Some(2024)) shouldBe EnrichTaskKeys.resolveImdbIdDedup("Dune", Some(2024))
    EnrichTaskKeys.resolveImdbIdDedup("Dune", Some(2024)) should not be EnrichTaskKeys.resolveImdbIdDedup("Dune", Some(2021))
    EnrichTaskKeys.resolveImdbIdDedup("Dune", None)       should not be EnrichTaskKeys.resolveImdbIdDedup("Dune", Some(2024))
  }

  "resolveImdbIdPayload" should "round-trip title, year (including a yearless film) and search title" in {
    val withYear = EnrichTaskKeys.resolveImdbIdPayload("Dune", Some(2024), "Dune: Part Two")
    EnrichTaskKeys.titleOf(withYear)       shouldBe "Dune"
    EnrichTaskKeys.yearOf(withYear)        shouldBe Some(2024)
    EnrichTaskKeys.searchTitleOf(withYear) shouldBe Some("Dune: Part Two")

    val noYear = EnrichTaskKeys.resolveImdbIdPayload("Untitled", None, "")
    EnrichTaskKeys.titleOf(noYear)       shouldBe "Untitled"
    EnrichTaskKeys.yearOf(noYear)        shouldBe None
    EnrichTaskKeys.searchTitleOf(noYear) shouldBe None
  }

  "the queue" should "collapse a second bulk trigger while the first is active (constant dedup key)" in {
    val queue = new InMemoryTaskQueue
    val key   = EnrichTaskKeys.bulkDedup(TaskType.RefreshAllFilmweb)
    queue.enqueue(TaskType.RefreshAllFilmweb, key) shouldBe EnqueueResult.Added
    queue.enqueue(TaskType.RefreshAllFilmweb, key) shouldBe EnqueueResult.Duplicate
  }

  it should "queue two different films' IMDb-id resolves independently" in {
    val queue = new InMemoryTaskQueue
    queue.enqueue(TaskType.ResolveImdbId, EnrichTaskKeys.resolveImdbIdDedup("A", None),
      EnrichTaskKeys.resolveImdbIdPayload("A", None, "A")) shouldBe EnqueueResult.Added
    queue.enqueue(TaskType.ResolveImdbId, EnrichTaskKeys.resolveImdbIdDedup("B", None),
      EnrichTaskKeys.resolveImdbIdPayload("B", None, "B")) shouldBe EnqueueResult.Added
  }
}
