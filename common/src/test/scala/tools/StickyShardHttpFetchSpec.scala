package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StickyShardHttpFetchSpec extends AnyFlatSpec with Matchers {

  /** A shard that just echoes its own tag, so a routed get reveals which shard
   *  handled it. */
  private class TaggedShard(tag: String) extends GetOnlyHttpFetch {
    override def get(url: String): String = tag
  }

  private def shards(tags: String*): IndexedSeq[HttpFetch] =
    tags.map(new TaggedShard(_)).toIndexedSeq

  private def multikinoApi(cinemaId: String) =
    s"https://www.multikino.pl/api/microservice/showings/cinemas/$cinemaId/films?minEmbargoLevel=2"

  "StickyShardHttpFetch" should "route a given venue to the same shard every time (sticky)" in {
    val fetch = new StickyShardHttpFetch(shards("a", "b", "c"))
    val url   = multikinoApi("0011")
    (1 to 10).map(_ => fetch.get(url)).distinct should have size 1
  }

  it should "key on host+path only, so the same venue with a different query is the same shard" in {
    val fetch = new StickyShardHttpFetch(shards("a", "b", "c"))
    fetch.get(multikinoApi("0011")) shouldBe
      fetch.get("https://www.multikino.pl/api/microservice/showings/cinemas/0011/films?includesSession=true&x=9")
  }

  // The regression: the previous design pinned a whole client to ONE shard, so
  // every venue hit the same IP and tripped Decodo's "Limit: 3". Venues must now
  // spread across the shards.
  it should "spread distinct venues across more than one shard" in {
    val fetch = new StickyShardHttpFetch(shards("a", "b", "c"))
    val venues = (1 to 40).map(i => fetch.get(multikinoApi(f"$i%04d")))
    venues.distinct.size should be > 1
  }

  it should "route different hosts (Multikino vs biletyna) independently across the shards" in {
    val fetch = new StickyShardHttpFetch(shards("a", "b", "c", "d", "e", "f", "g"))
    val biletyna = Seq(
      "https://biletyna.pl/Turek/Kino-Tur",
      "https://biletyna.pl/Chelmno/Kinoteatr-Rondo",
      "https://biletyna.pl/Gdansk/Kino-na-Szekspirowskim"
    ).map(fetch.get)
    // each is deterministic + sticky
    biletyna shouldBe Seq("https://biletyna.pl/Turek/Kino-Tur",
                          "https://biletyna.pl/Chelmno/Kinoteatr-Rondo",
                          "https://biletyna.pl/Gdansk/Kino-na-Szekspirowskim").map(fetch.get)
  }

  it should "reject an empty shard list (a wiring bug)" in {
    an[IllegalArgumentException] should be thrownBy new StickyShardHttpFetch(IndexedSeq.empty)
  }
}
