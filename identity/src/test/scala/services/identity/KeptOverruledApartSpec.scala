package services.identity

import models.{Helios, KinoApollo, KinoBulgarska, KinoMikro, KinoMuza, KinoOaza, KinoPalacowe, Kinoteka, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

import scala.util.Random

/**
 * A kept listing ([[Listing.kept]], [[ListingRetention]]) never joins a present listing whose feed link the PRESENT
 * sources overrule toward another film, nor counts toward that overrule (the owner, 2026-11-01, "B"; DE "Eternity"),
 * on every seed: however many venues the feed links, however many entries credit the other film, whatever the kept
 * listing states, and whichever order the model is told -- the incremental model deciding as a whole resolve does.
 */
class KeptOverruledApartSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  import FilmTable.{F, listing}

  private val films  = Seq(F(1259102, "Eternity", 2025, "David Freyne", 114, 20.0), F(1769681, "Eternity", 2026, "Susanne Regina Meures", 90, 0.4))
  private val Linked = Seq(KinoMikro, KinoOaza, KinoPalacowe, Kinoteka, Helios, Multikino)
  private def fed(l: Listing, source: String, id: String) = l.copy(catalogueIds = Seq(CatalogueId(source, id)))

  private final case class Case(linked: Seq[Listing], others: Seq[Listing], kept: Listing) {
    def all: Seq[Listing] = linked ++ others :+ kept
  }
  private def caseOf(seed: Long): Case = {
    val random  = new Random(seed)
    val linked  = Linked.take(1 + random.nextInt(Linked.size)).map(v =>
      fed(listing(v, "Eternity", Option.when(random.nextBoolean())(2025)), "cineamo", "1259102m"))
    val others  = Seq(
      listing(KinoApollo, "Eternity", Some(2026), Some("Susanne Regina Meures"), Some(90)).copy(page = Some("https://www.kinoheld.de/film/eternity-2026")),
      fed(listing(KinoMuza, "Eternity", Some(2026), Some("Susanne Regina Meures"), Some(90)), "webedia", "1000052001"))
    val kept    = ListingRetention.kept(listing(KinoBulgarska, "Eternity", Some(2025),
      Option.when(random.nextBoolean())("David Freyne"), Option.when(random.nextBoolean())(114)).copy(page = Some("https://www.kinoheld.de/film/eternity")))
    Case(linked, others, kept)
  }

  private def whole(listings: Seq[Listing]): Resolution =
    IdentityResolver.resolve(listings, new FilmTable(films, normalizer), normalizer, ResolveInputs.Unpinned)

  "a present listing whose feed link the present sources overrule" should "never take the film a kept listing pulls it to, on every seed" in {
    val wrong = (1L to 32L).flatMap { seed =>
      val c = caseOf(seed)
      val r = whole(c.all)
      c.linked.flatMap(l => Option.when(r.decisionOf(l.key).film.contains(1259102))(s"seed $seed: ${l.venue} took 1259102\n${c.all.map(x => r.decisionOf(x.key).render).distinct.mkString("\n")}"))
    }
    wrong shouldBe empty
  }

  it should "be decided by the incremental model, told in any order, as a whole resolve decides it" in {
    val apart = (1L to 16L).flatMap { seed =>
      val c     = caseOf(seed)
      val model = new IncrementalResolver(new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver, decorations = TitleDecorations.None)
      new Random(seed).shuffle(c.all).grouped(2).foreach(batch => model.batch(seen = batch))
      val incremental = model.decisions.flatMap(d => d.members.map(_ -> d.film)).toMap
      val r = IdentityResolver.resolve(c.all, new FilmTable(films, normalizer), normalizer,
        ResolveInputs.Unpinned.copy(decorations = TitleDecorations.None))
      c.all.flatMap(l => Option.when(incremental.get(l.key).flatten != r.decisionOf(l.key).film)(s"seed $seed: ${l.venue} ${incremental.get(l.key)} vs ${r.decisionOf(l.key).film}"))
    }
    apart shouldBe empty
  }
}
