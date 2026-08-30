package controllers

import models._
import services.readmodel.TestReadModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

class SitemapBuilderSpec extends AnyFlatSpec with Matchers {

  private val Origin = "https://kinowo.net"

  // Count literal (non-regex) occurrences — the URLs contain `?`, which
  // String.split would treat as a regex metachar.
  private def count(haystack: String, needle: String): Int = {
    @annotation.tailrec def go(from: Int, acc: Int): Int = {
      val i = haystack.indexOf(needle, from)
      if (i < 0) acc else go(i + needle.length, acc + 1)
    }
    go(0, 0)
  }

  private def film(title: String, cinema: Cinema): FilmSchedule = FilmSchedule(
    movie          = Movie(title = title),
    posterUrl      = None,
    synopsis       = None,
    cast           = Nil,
    director       = Nil,
    cinemaFilmUrls = Nil,
    showings       = Seq(
      LocalDate.of(2026, 5, 17) -> Seq(CinemaShowtimes(cinema, Seq(
        Showtime(dateTime = LocalDateTime.of(2026, 5, 17, 18, 0), bookingUrl = None)
      )))
    ),
    resolved       = TestReadModel.resolved(title, None, MovieRecord())
  )

  private val entries: Seq[(City, Seq[FilmSchedule])] = Seq(
    Poznan  -> Seq(film("Belle", Multikino), film("Diuna: Część druga", Helios)),
    Wroclaw -> Seq(film("Belle", Helios)),
  )

  "SitemapBuilder.build" should "open with the XML prolog and the urlset element" in {
    val xml = SitemapBuilder.build(Origin, entries)
    xml should startWith("""<?xml version="1.0" encoding="UTF-8"?>""")
    xml      should include("""<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">""")
    xml      should include("</urlset>")
  }

  it should "emit the landing page exactly once" in {
    val xml = SitemapBuilder.build(Origin, entries)
    count(xml, s"<loc>$Origin/</loc>") shouldBe 1
  }

  it should "omit changefreq and priority — Google ignores both" in {
    val xml = SitemapBuilder.build(Origin, entries, lastmod = Some("2026-06-28"))
    xml should not include "<changefreq>"
    xml should not include "<priority>"
  }

  it should "emit a listing + plan URL for each city" in {
    val xml = SitemapBuilder.build(Origin, entries)
    xml should include(s"<loc>$Origin/poznan/</loc>")
    xml should include(s"<loc>$Origin/poznan/plan</loc>")
    xml should include(s"<loc>$Origin/wroclaw/</loc>")
    xml should include(s"<loc>$Origin/wroclaw/plan</loc>")
  }

  /** For a city whose `/{slug}/` is a metro chooser rather than a listing, the
   *  crawlable listings are the per-area URLs — omit them and a 486-venue
   *  state's long tail is reachable only by clicking through the picker. */
  it should "emit one area URL per metro for a chooser city, in City.areas order" in {
    val california = City.all.find(_.slug == "california").get
    val xml = SitemapBuilder.build(Origin, Seq(california -> Nil))
    xml should include(s"<loc>$Origin/california/</loc>")
    xml should include(s"<loc>$Origin/california/los-angeles/</loc>")
    xml should include(s"<loc>$Origin/california/san-francisco/</loc>")
    count(xml, s"$Origin/california/") shouldBe (california.areas.size + 2)  // city + areas + plan
    val positions = california.areas.map(g => xml.indexOf(s"$Origin/california/${g.area.slug}/"))
    positions shouldBe positions.sorted
  }

  /** London is split but stays one page, so it has no area URLs to advertise —
   *  emitting `/london/central/` would sitemap a 404. */
  it should "emit no area URLs for a split city below the chooser threshold" in {
    val xml = SitemapBuilder.build(Origin, Seq(London -> Nil))
    xml should include(s"<loc>$Origin/london/</loc>")
    xml should not include s"$Origin/london/central/"
  }

  it should "emit a slug film deep-link per distinct title in the city" in {
    val xml = SitemapBuilder.build(Origin, entries)
    xml should include(s"<loc>$Origin/poznan/film/belle</loc>")
    // The slug carries no percent-escapes at all — diacritics and punctuation
    // fold rather than encode, so a `<loc>` needs no XML escaping either.
    xml should include(s"<loc>$Origin/poznan/film/diuna-czesc-druga</loc>")
    xml should not include "title="
    xml should not include "%"
  }

  it should "de-duplicate titles within a city and sort them deterministically" in {
    val dupes = Seq(Poznan -> Seq(film("Zorro", Multikino), film("Amelia", Helios), film("Zorro", Helios)))
    val xml   = SitemapBuilder.build(Origin, dupes)
    // "Zorro" appears once despite two screenings…
    count(xml, s"<loc>$Origin/poznan/film/zorro</loc>") shouldBe 1
    // …and Amelia sorts before Zorro (stable output across read-model orderings).
    xml.indexOf("/film/amelia") should be < xml.indexOf("/film/zorro")
  }

  it should "stamp the read-model-derived URLs with lastmod when supplied" in {
    val xml = SitemapBuilder.build(Origin, entries, lastmod = Some("2026-06-28"))
    xml should include("<lastmod>2026-06-28</lastmod>")
    xml should not include "<lastmod></lastmod>"
    // Every city listing, plan and film URL regenerates with the read model…
    count(xml, "<lastmod>2026-06-28</lastmod>") shouldBe (xml.split("<url>").length - 2)
  }

  it should "leave the landing page unstamped — it is a static city list" in {
    val landing = SitemapBuilder.build(Origin, entries, lastmod = Some("2026-06-28"))
      .linesIterator.find(_.contains(s"<loc>$Origin/</loc>")).getOrElse("")
    landing should not include "<lastmod>"
  }

  it should "omit lastmod entirely when not supplied" in {
    SitemapBuilder.build(Origin, entries) should not include "<lastmod>"
  }

  it should "produce well-formed, parseable XML" in {
    val xml = SitemapBuilder.build(Origin, entries, lastmod = Some("2026-06-28"))
    val factory = javax.xml.parsers.DocumentBuilderFactory.newInstance()
    val doc = factory.newDocumentBuilder()
      .parse(new java.io.ByteArrayInputStream(xml.getBytes(java.nio.charset.StandardCharsets.UTF_8)))
    doc.getDocumentElement.getTagName shouldBe "urlset"
    doc.getElementsByTagName("url").getLength should be > 4
  }
}
