package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A `Source`'s hash is a function of what it IS, never of which JVM built it.
 *
 * A film's slots are a `Map[Source, SourceData]`, and a hash map iterates in its keys' hash order. The data-driven
 * venues (Germany, the United States, Spain) once kept Object's `hashCode` -- the JVM's identity hash -- so the same
 * film's slots iterated in a different order in every JVM, and whatever read that order decided differently on
 * identical inputs: Germany's order passes, run in two JVMs, disagreed on 6 films' TMDB title searches. A fresh
 * instance of a venue with the same names stands in for "the same venue in another JVM": it gets a new identity
 * hash, as the other JVM's does.
 */
class SourceHashDeterminismSpec extends AnyFlatSpec with Matchers {

  private val sources: Seq[Source] = (Source.all ++ Cinema.all).distinct

  "every Source" should "hash by its content, not by Object's identity hash" in {
    val identityHashed = sources.map(_.getClass).distinct
      .filter(_.getMethod("hashCode").getDeclaringClass == classOf[Object])
    identityHashed shouldBe empty
  }

  "every City" should "hash by its content, not by Object's identity hash" in {
    City.all.map(_.getClass).distinct.filter(_.getMethod("hashCode").getDeclaringClass == classOf[Object]) shouldBe empty
  }

  "a roster venue" should "hash as the same venue built in another JVM does" in {
    val other: Cinema => Option[Cinema] = {
      case c: GermanCinema  => Some(new GermanCinema(c.displayName, c.pillName))
      case c: UsCinema      => Some(new UsCinema(c.displayName, c.pillName))
      case c: SpanishCinema => Some(new SpanishCinema(c.displayName, c.pillName))
      case _                => None
    }
    val pairs = Cinema.all.flatMap(c => other(c).map(c -> _))
    pairs.size should be > 1000
    pairs.filter { case (here, there) => here.hashCode != there.hashCode }.map(_._1.displayName) shouldBe empty
  }

  "a film's slots" should "iterate in the same order in another JVM" in {
    val venues   = Cinema.all.collect { case c: GermanCinema => c }.take(40)
    val elsewhere = venues.map(c => new GermanCinema(c.displayName, c.pillName))
    val here  = venues.map(c => (c: Source) -> c.displayName).toMap
    val there = elsewhere.map(c => (c: Source) -> c.displayName).toMap
    here.values.toList shouldBe there.values.toList
  }
}
