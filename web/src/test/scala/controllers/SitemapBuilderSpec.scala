package controllers

import models._
import services.readmodel.TestReadModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

class SitemapBuilderSpec extends AnyFlatSpec with Matchers {

  private val Origin = "https://kinowo.fly.dev"

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

  it should "emit the landing page once with top priority" in {
    val xml = SitemapBuilder.build(Origin, entries)
    count(xml, s"<loc>$Origin/</loc>") shouldBe 1
    xml should include("<priority>1.0</priority>")
  }

  it should "emit a listing + plan URL for each city" in {
    val xml = SitemapBuilder.build(Origin, entries)
    xml should include(s"<loc>$Origin/poznan/</loc>")
    xml should include(s"<loc>$Origin/poznan/plan</loc>")
    xml should include(s"<loc>$Origin/wroclaw/</loc>")
    xml should include(s"<loc>$Origin/wroclaw/plan</loc>")
  }

  it should "emit a %20-encoded film deep-link per distinct title in the city" in {
    val xml = SitemapBuilder.build(Origin, entries)
    xml should include(s"<loc>$Origin/poznan/film?title=Belle</loc>")
    // Spaces encode as %20 (RFC 3986) and `:` as %3A, never `+`, matching FilmHref.
    xml should include(s"<loc>$Origin/poznan/film?title=Diuna%3A%20Cz%C4%99%C5%9B%C4%87%20druga</loc>")
    xml should not include "+druga"
  }

  it should "de-duplicate titles within a city and sort them deterministically" in {
    val dupes = Seq(Poznan -> Seq(film("Zorro", Multikino), film("Amelia", Helios), film("Zorro", Helios)))
    val xml   = SitemapBuilder.build(Origin, dupes)
    // "Zorro" appears once despite two screenings…
    count(xml, s"<loc>$Origin/poznan/film?title=Zorro</loc>") shouldBe 1
    // …and Amelia sorts before Zorro (stable output across read-model orderings).
    xml.indexOf("title=Amelia") should be < xml.indexOf("title=Zorro")
  }

  it should "stamp every URL with lastmod when supplied" in {
    val xml = SitemapBuilder.build(Origin, entries, lastmod = Some("2026-06-28"))
    xml should include("<lastmod>2026-06-28</lastmod>")
    xml should not include "<lastmod></lastmod>"
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
