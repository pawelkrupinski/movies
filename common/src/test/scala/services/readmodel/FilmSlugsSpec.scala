package services.readmodel

import models.{ResolvedMovie, ResolvedRatings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The prod bug this covers: `/poznan/movie/zaproszenie` rendered TWO cards —
 *  Olivia Wilde's 2026 "The Invite" and Wanda Jakubowska's 1986 war drama, both
 *  linking to the same address — so one of the two films had no URL at all. The
 *  same shape hit `lalka|1968` vs `lalka|2026` in six cities, plus
 *  `vincent-legenda-oceanu`, `sense-and-sensibility` (London) and
 *  `hard-boiled` / `emil-und-die-detektive` (Berlin).
 */
class FilmSlugsSpec extends AnyFlatSpec with Matchers {

  private def movie(id: String, title: String, year: Option[Int]): ResolvedMovie =
    ResolvedMovie(
      _id = id, title = title, originalTitle = None, posterUrl = None, fallbackPosterUrls = Seq.empty,
      runtimeMinutes = None, releaseYear = year, genres = Seq.empty, countries = Seq.empty,
      directors = Seq.empty, cast = Seq.empty, synopsis = None, trailerUrls = Seq.empty,
      ratings = ResolvedRatings(None, None, None, "", None, "", None, ""), weightedRating = 0.0
    )

  private val invite     = movie("zaproszenie|2026", "Zaproszenie", Some(2026))
  private val jakubowska = movie("zaproszenie|1986", "Zaproszenie", Some(1986))

  /** Same title, same year — separated only by the last tiebreak, which is where the
   *  film id used to sit. Since the id became an opaque `FilmId` minted from arrival
   *  order, that made the bare address flip between the two whenever a worker rebuilt
   *  its corpus in a different order: a shared link opened a different film. Caught by
   *  the Polish order-independence leg on `lalka` / `lalka-2026`, 2026-09-07. */
  private def resolved(id: String, title: String, year: Option[Int], imdb: String): ResolvedMovie =
    movie(id, title, year).copy(ratings = ResolvedRatings(None, Some(s"https://www.imdb.com/title/$imdb/"), None, "", None, "", None, ""))

  private val lalkaA = resolved("f00aaaaaaaaaaaa", "Lalka", Some(2026), "tt0000111")
  private val lalkaB = resolved("f00bbbbbbbbbbbb", "Lalka", Some(2026), "tt0000222")

  "a same-title, same-year pair" should "keep the same addresses however the corpus was ordered" in {
    val one   = FilmSlugs(Seq(lalkaA, lalkaB))
    val other = FilmSlugs(Seq(lalkaB, lalkaA))

    one.slugFor(lalkaA._id) shouldBe Some("lalka")
    one.slugFor(lalkaB._id) shouldBe Some("lalka-2026")
    other.slugFor(lalkaA._id) shouldBe one.slugFor(lalkaA._id)
    other.slugFor(lalkaB._id) shouldBe one.slugFor(lalkaB._id)
  }

  it should "not move when the ids are re-minted in the other order, as a replay assigns them" in {
    // The same two films, ids swapped — what a differently-ordered arrival produces.
    val replayA = lalkaA.copy(_id = lalkaB._id)
    val replayB = lalkaB.copy(_id = lalkaA._id)

    val replay = FilmSlugs(Seq(replayA, replayB))

    withClue("the film with the lower IMDb id keeps the bare address, whatever id it was minted: ") {
      replay.slugFor(replayA._id) shouldBe Some("lalka")
      replay.idFor("lalka")       shouldBe Some(replayA._id)
    }
  }

  it should "prefer the film that has an IMDb id to one that has none" in {
    val unresolved = movie("f00cccccccccccc", "Lalka", Some(2026))

    val slugs = FilmSlugs(Seq(unresolved, lalkaA))

    slugs.slugFor(lalkaA._id)     shouldBe Some("lalka")
    slugs.slugFor(unresolved._id) shouldBe Some("lalka-2026")   // it has a year, so it qualifies with one
  }

  "FilmSlugs" should "give every film in a same-title collision its own address" in {
    val slugs = FilmSlugs(Seq(invite, jakubowska))

    slugs.slugFor("zaproszenie|2026") shouldBe Some("zaproszenie")
    slugs.slugFor("zaproszenie|1986") shouldBe Some("zaproszenie-1986")
  }

  it should "resolve each of those addresses back to its own film" in {
    val slugs = FilmSlugs(Seq(invite, jakubowska))

    slugs.idFor("zaproszenie")      shouldBe Some("zaproszenie|2026")
    slugs.idFor("zaproszenie-1986") shouldBe Some("zaproszenie|1986")
    slugs.idFor("nie-ma-takiego")   shouldBe None
  }

  it should "leave the bare slug with the newest film regardless of corpus order" in {
    val forward = FilmSlugs(Seq(invite, jakubowska))
    val reverse = FilmSlugs(Seq(jakubowska, invite))

    reverse.slugFor("zaproszenie|2026") shouldBe forward.slugFor("zaproszenie|2026")
    reverse.slugFor("zaproszenie|1986") shouldBe forward.slugFor("zaproszenie|1986")
  }

  it should "leave an uncontested title on its bare slug" in {
    val slugs = FilmSlugs(Seq(movie("lalka|2026", "Lalka", Some(2026)), invite))

    slugs.slugFor("lalka|2026") shouldBe Some("lalka")
    slugs.slugFor("zaproszenie|2026") shouldBe Some("zaproszenie")
  }

  // The qualified form appends a year, and a title can already END in one, so
  // the ladder has to check the whole corpus rather than just the collision
  // group — otherwise the retrospective would take the address of a film whose
  // own title folds there.
  it should "not hand a qualified slug to a film that already owns it" in {
    val absolwent1967 = movie("kultowa-klasyka-absolwent-1967|1967",
      "Kultowa klasyka: Absolwent (1967)", Some(1967))
    val slugs = FilmSlugs(Seq(
      movie("kultowa-klasyka-absolwent|2026", "Kultowa klasyka: Absolwent", Some(2026)),
      movie("kultowa-klasyka-absolwent|1967", "Kultowa klasyka: Absolwent", Some(1967)),
      absolwent1967
    ))

    slugs.slugFor("kultowa-klasyka-absolwent-1967|1967") shouldBe Some("kultowa-klasyka-absolwent-1967")
    slugs.slugFor("kultowa-klasyka-absolwent|2026")      shouldBe Some("kultowa-klasyka-absolwent")
    slugs.slugFor("kultowa-klasyka-absolwent|1967")      shouldBe Some("kultowa-klasyka-absolwent-1967-2")
  }

  it should "address films whose years are unknown or equal" in {
    val slugs = FilmSlugs(Seq(
      movie("mira|a", "Mira", None),
      movie("mira|b", "Mira", None),
      movie("mira|2026", "Mira", Some(2026))
    ))

    slugs.slugFor("mira|2026") shouldBe Some("mira")
    Seq("mira|a", "mira|b").flatMap(slugs.slugFor).distinct should have size 2
  }

  it should "skip a title that folds to nothing addressable" in {
    FilmSlugs(Seq(movie("!!!|2026", "!!!", Some(2026)))).slugFor("!!!|2026") shouldBe None
  }

  // Two SPELLINGS of one film ("Rocky II" / "Rocky 2") also fold together. They
  // are separate rows, so they still get separate addresses — but the bare slug
  // must land on one of them deterministically rather than shifting per render.
  it should "stay one-to-one between slugs and films" in {
    val corpus = Seq(
      invite, jakubowska,
      movie("rocky-2|1979", "Rocky II", Some(1979)),
      movie("rocky-2|1979-b", "Rocky 2", Some(1979)),
      movie("lalka|1968", "Lalka", Some(1968)),
      movie("lalka|2026", "Lalka", Some(2026))
    )
    val slugs = FilmSlugs(corpus)

    val assigned = corpus.flatMap(m => slugs.slugFor(m._id))
    assigned should have size corpus.size
    assigned.distinct should have size corpus.size
    assigned.foreach(s => slugs.idFor(s) should not be empty)
  }

  // The prod bug: a film billed only as a cinema's event ("Kino na obcasach: Jak żyć,
  // żeby nie zwariować") was addressed by that billing, so its page lived at
  // /poznan/movie/kino-na-obcasach-jak-zyc-zeby-nie-zwariowac. The address is the
  // film's, so it folds the clean title — the banner/decoration strips the external
  // lookups already use.
  private val polish = FilmSlugs.forCountry(models.Country.Poland)

  private val obcasach = movie("jak-zyc|2026", "Kino na obcasach: Jak żyć, żeby nie zwariować", Some(2026))

  "an event-prefixed billing" should "be addressed by the film's clean title" in {
    val slugs = polish(Seq(obcasach))

    slugs.slugFor(obcasach._id)                  shouldBe Some("jak-zyc-zeby-nie-zwariowac")
    slugs.idFor("jak-zyc-zeby-nie-zwariowac")    shouldBe Some(obcasach._id)
  }

  it should "keep resolving its former, billing-derived address, as a former one" in {
    val slugs = polish(Seq(obcasach))

    slugs.idFor("kino-na-obcasach-jak-zyc-zeby-nie-zwariowac")       shouldBe None
    slugs.formerIdFor("kino-na-obcasach-jak-zyc-zeby-nie-zwariowac") shouldBe Some(obcasach._id)
    slugs.formerIdFor("jak-zyc-zeby-nie-zwariowac")                  shouldBe None
  }

  it should "keep its own billing address while the plain listing of the film holds the clean one" in {
    // The event screening is its own card next to the plain one; the plain card keeps the bare
    // address, and the event card keeps the one it already had rather than a year-qualified
    // address that would look like a different film.
    val plain = movie("jak-zyc|2026-plain", "Jak żyć, żeby nie zwariować", Some(2026))
    val slugs = polish(Seq(obcasach, plain))

    slugs.slugFor(plain._id)    shouldBe Some("jak-zyc-zeby-nie-zwariowac")
    slugs.slugFor(obcasach._id) shouldBe Some("kino-na-obcasach-jak-zyc-zeby-nie-zwariowac")
    slugs.formerIdFor("kino-na-obcasach-jak-zyc-zeby-nie-zwariowac") shouldBe None
  }

  it should "leave an undecorated corpus addressed exactly as before" in {
    val corpus = Seq(invite, jakubowska, lalkaA, lalkaB, movie("mira|2026", "Mira", Some(2026)))
    val before = FilmSlugs(corpus)
    val after  = polish(corpus)

    corpus.foreach(m => after.slugFor(m._id) shouldBe before.slugFor(m._id))
  }
}
