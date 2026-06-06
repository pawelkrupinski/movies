package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `MovieRecord.fallbackPosterUrls` — every poster URL we know about
 * except the primary, in source-priority order. Used as a `data-fallbacks`
 * chain by the view: when one URL 404s the client pops the next, all the
 * way down to TMDB, before falling through to the `.no-poster` placeholder.
 *
 * Why a chain and not just a TMDB safety-net: cinema CDNs intermittently
 * 403/404 (Multikino's CDN refuses cross-origin fetches today; Cinema City
 * has shipped posterLinks to images they hadn't uploaded). When the
 * primary cinema fails we'd rather try another cinema's poster than jump
 * straight to a Hollywood-style TMDB master crop — and the data is right
 * there in `data: Map[Source, SourceData]`, we just weren't using it.
 */
class MovieRecordPosterSpec extends AnyFlatSpec with Matchers {

  "fallbackPosterUrls" should "list every non-primary poster URL in source-priority order" in {
    // Werdykt-style row: Multikino wins the primary slot (currently 403),
    // Cinema City has a working poster, TMDB has its own. The fallback
    // chain should offer CinemaCity before TMDB.
    val multikino   = "https://www.multikino.pl/.../werdykt.jpg"
    val cinemaCity  = "https://www.cinema-city.pl/.../8194S2R.jpg"
    val tmdb        = "https://image.tmdb.org/t/p/w500/x.jpg"
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Multikino           -> SourceData(posterUrl = Some(multikino)),
        CinemaCityKinepolis -> SourceData(posterUrl = Some(cinemaCity)),
        Tmdb                -> SourceData(posterUrl = Some(tmdb))
      )
    )
    rec.posterUrl          shouldBe Some(multikino)
    rec.fallbackPosterUrls shouldBe Seq(cinemaCity, tmdb)
  }

  it should "include IMDb after every cinema and before / after TMDB per Source.priority" in {
    val multikino  = "https://www.multikino.pl/x.jpg"
    val cinemaCity = "https://www.cinema-city.pl/y.jpg"
    val tmdb       = "https://image.tmdb.org/t/p/w500/z.jpg"
    val imdb       = "https://m.media-amazon.com/.../poster.jpg"
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Multikino           -> SourceData(posterUrl = Some(multikino)),
        CinemaCityKinepolis -> SourceData(posterUrl = Some(cinemaCity)),
        Tmdb                -> SourceData(posterUrl = Some(tmdb)),
        Imdb                -> SourceData(posterUrl = Some(imdb))
      )
    )
    // Source.priority is cinemas (Multikino first) → Tmdb → Imdb, so once
    // Multikino is the primary the chain is CinemaCity, Tmdb, Imdb.
    rec.fallbackPosterUrls shouldBe Seq(cinemaCity, tmdb, imdb)
  }

  it should "drop the primary even when other sources duplicate it" in {
    val url = "https://image.tmdb.org/t/p/w500/x.jpg"
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(posterUrl = Some(url)),
        Tmdb      -> SourceData(posterUrl = Some(url))
      )
    )
    rec.posterUrl          shouldBe Some(url)
    rec.fallbackPosterUrls shouldBe empty
  }

  it should "deduplicate when two non-primary sources share a URL" in {
    val multikino = "https://www.multikino.pl/m.jpg"
    val shared    = "https://image.tmdb.org/t/p/w500/x.jpg"
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Multikino           -> SourceData(posterUrl = Some(multikino)),
        CinemaCityKinepolis -> SourceData(posterUrl = Some(shared)),
        Tmdb                -> SourceData(posterUrl = Some(shared))
      )
    )
    rec.fallbackPosterUrls shouldBe Seq(shared)
  }

  it should "be empty when only the primary source has a poster" in {
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Helios -> SourceData(posterUrl = Some("https://img.helios.pl/x.jpg"))
      )
    )
    rec.fallbackPosterUrls shouldBe empty
  }

  it should "be empty when no source has a poster at all" in {
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Helios -> SourceData(posterUrl = None)
      )
    )
    rec.posterUrl          shouldBe None
    rec.fallbackPosterUrls shouldBe empty
  }
}
