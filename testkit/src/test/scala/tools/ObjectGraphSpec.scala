package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ObjectGraphSpec extends AnyFlatSpec with Matchers {

  private final class Target
  private final class Holder(val target: Target)
  private final class Link(val next: AnyRef)
  private final class Root(val first: AnyRef, val second: AnyRef)

  // One holder reached two ways: two hops from the root, and at the end of a chain that puts it
  // exactly at the walk's depth cap (60), its target one past it. Whichever way the walk reaches
  // the holder first is where it is marked seen.
  private def rootWith(holder: Holder, shortFirst: Boolean): Root = {
    val chain = (1 to 59).foldLeft[AnyRef](holder)((next, _) => new Link(next))
    val short = new Link(holder)
    if (shortFirst) new Root(short, chain) else new Root(chain, short)
  }

  "a value two hops from the root" should "be found even when a path as long as the depth cap also leads to it" in {
    for (shortFirst <- Seq(true, false)) {
      val target = new Target
      val found  = ObjectGraph.collect(rootWith(new Holder(target), shortFirst)) { case t: Target => t }
      withClue(s"short path declared ${if (shortFirst) "first" else "second"}: ") {
        found.map(_._2) shouldBe Seq(target)
        found.head._1 shouldBe s"Root.${if (shortFirst) "first" else "second"}.next.target"
      }
    }
  }
}
