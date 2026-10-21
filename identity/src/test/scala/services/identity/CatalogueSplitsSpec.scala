package services.identity

import models.{Cinema, Helios, KinoMuza, Rialto}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.SuspectTakes.Take

/** One catalogue id whose listings were taken as two films ([[CatalogueSplits]]). */
class CatalogueSplitsSpec extends AnyFlatSpec with Matchers {
  import FilmTable.listing

  private val emil = CatalogueId(CatalogueSources.Webedia.source, "1234")
  private def take(venue: Cinema, title: String, tmdb: Int, year: Int, ids: Seq[CatalogueId] = Seq(emil)) =
    Take(listing(venue, title).copy(catalogueIds = ids), s"tmdb:$tmdb", Some(tmdb), IdentityMeasures.Film(title, year = Some(year)),
      ResolverDecision(Nil, Some(tmdb), 0.9, ResolverDecision.Basis.OwnMatch, Nil)())

  "a catalogue id" should "split where its listings were taken as two films, each film's side naming the other" in {
    // Webedia's one id taken as Kästner's 1931 film at two venues and as the 2026 film at a third (DE, 2026-10-21)
    val old = Seq(take(KinoMuza, "Emil und die Detektive", 1931, 1931), take(Helios, "Emil und die Detektive", 1931, 1931))
    val now = take(Rialto, "EMIL UND DIE DETEKTIVE", 2026, 2026)
    CatalogueSplits.of(old :+ now).map(s => (s.film, s.title, s.venues, s.rawTitles, s.why)) shouldBe Seq(
      ("tmdb:1931", "Emil und die Detektive (1931)", Seq(Helios.displayName, KinoMuza.displayName).sorted, Seq("Emil und die Detektive"),
        "webedia 1234 also names tmdb:2026 (1 listing); this film 2 listings"),
      ("tmdb:2026", "EMIL UND DIE DETEKTIVE (2026)", Seq(Rialto.displayName), Seq("EMIL UND DIE DETEKTIVE"),
        "webedia 1234 also names tmdb:1931 (2 listings); this film 1 listing"))
  }

  it should "not split where every listing carrying it was taken as one film, nor across two ids" in {
    CatalogueSplits.of(Seq(take(KinoMuza, "Emil", 1931, 1931), take(Helios, "Emil", 1931, 1931))) shouldBe empty
    CatalogueSplits.of(Seq(take(KinoMuza, "Emil", 1931, 1931),
      take(Rialto, "Emil", 2026, 2026, Seq(CatalogueId(CatalogueSources.Webedia.source, "99"))))) shouldBe empty
  }
}
