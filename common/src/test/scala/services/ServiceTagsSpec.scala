package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `ServiceTags` without a monitor around it: no collection means the in-memory
 *  map is the whole state, and the skip-if-unchanged guard decides from it. */
class ServiceTagsSpec extends AnyFlatSpec with Matchers {

  "ServiceTags.tagService" should "report a write only when the tags changed, and snapshot the in-memory view" in {
    val tags = new ServiceTags(None)

    tags.tagService("Kino Rialto", Set("custom:RialtoClient")) shouldBe true
    tags.tagService("Kino Rialto", Set("custom:RialtoClient")) shouldBe false
    tags.tagService("Kino Rialto", Set("shared:HeliosClient")) shouldBe true

    tags.snapshot() shouldBe Map("Kino Rialto" -> Set("shared:HeliosClient"))
  }
}
