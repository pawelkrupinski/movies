package controllers

import models.{CinemaCityWroclavia, Country, HeliosMagnolia, MovieRecord, Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.readmodel.{InMemoryReadModelRepository, ReadModelReader, TestReadModel}

import java.time.LocalDateTime

/**
 * `GET /debug/readmodel`: the dev-only dump of the read model (`web_movies` + per-film
 * `web_screenings` counts), one country at a time, a row's screenings fetched on expand.
 */
class DebugReadModelSpec extends AnyFlatSpec with Matchers {

  private def record(title: String) = MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some(title))))

  private val records = Seq(("Belle", Some(2021), record("Belle")), ("Incepcja", Some(2010), record("Incepcja")))

  private def controller(readModels: DebugReadModels, mode: Mode = Mode.Dev) =
    TestDebugController.build(Seq.empty, mode, readModels = Some(readModels))._1

  private def single(reader: ReadModelReader) = DebugReadModels.of(Country.default, reader, mirror = None)

  "GET /debug/readmodel" should "list every read-model film in dev mode" in {
    val result = controller(single(TestReadModel.store(records))).debugReadModel().apply(FakeRequest(GET, "/debug/readmodel"))

    status(result) shouldBe OK
    val html = contentAsString(result)
    html should include("Read model")
    html should include("Belle")
    html should include("Incepcja")
    html should include("<b>2</b> movies")
  }

  // The served model of a dev server reads `kinowo_local`, which nothing fills since the local
  // worker went (2026-10-19): the page read it and listed nothing. With the mirror configured
  // every country, the serving one included, is read from its `<db>_prod_mirror`.
  it should "read the serving country from the local mirror when one is configured, not the serving database" in {
    val mirrorDbs = Map(
      "kinowo_prod_mirror"    -> TestReadModel.store(Seq(("Mirrored Pl Film", Some(2021), record("Mirrored Pl Film")))),
      "kinowo_uk_prod_mirror" -> TestReadModel.store(Seq(("Mirrored Uk Film", Some(2021), record("Mirrored Uk Film")))))
    val readModels = DebugReadModels.of(Country.Poland, new InMemoryReadModelRepository(),
      mirror = Some(db => mirrorDbs.getOrElse(db, new InMemoryReadModelRepository())))
    val ctrl = controller(readModels)

    val pl = contentAsString(ctrl.debugReadModel().apply(FakeRequest(GET, "/debug/readmodel")))
    pl should include("Mirrored Pl Film")
    pl should include("kinowo_prod_mirror")

    val uk = contentAsString(ctrl.debugReadModel().apply(FakeRequest(GET, "/debug/readmodel?country=uk")))
    uk should include("Mirrored Uk Film")
    uk should not include ("Mirrored Pl Film")
    uk should include("""value="/debug/readmodel?country=uk" selected""")
  }

  it should "ignore a country it has no read model for" in {
    val html = contentAsString(controller(single(TestReadModel.store(records)))
      .debugReadModel().apply(FakeRequest(GET, "/debug/readmodel?country=uk")))
    html should include("Belle")
  }

  // A row carries counts; its screenings come from the fragment on expand — inlining every
  // showtime of the US read model (~100k screening docs) OOM'd the dev server.
  it should "show a Cinemas count and every resolved field, with the screenings fetched on expand" in {
    val rich = MovieRecord(data = Map(
      CinemaCityWroclavia -> SourceData(
        title          = Some("Belle"),
        originalTitle  = Some("Ryu to Sobakasu no Hime"),
        synopsis       = Some("A shy teenager becomes a virtual-world pop star."),
        cast           = Seq("Kaho Nakamura"),
        director       = Seq("Mamoru Hosoda"),
        runtimeMinutes = Some(124),
        releaseYear    = Some(2021),
        countries      = Seq("Japonia"),
        genres         = Seq("Animacja"),
        posterUrl      = Some("https://img.example/belle.jpg"),
        filmUrl        = Some("https://cinema-city.pl/belle"),
        showtimes      = Seq(Showtime(LocalDateTime.parse("2026-06-28T18:30"),
          bookingUrl = Some("https://book.example/cc"), room = Some("Sala 4"), format = List("2D", "NAP")))),
      HeliosMagnolia -> SourceData(
        title     = Some("Belle"),
        filmUrl   = Some("https://helios.pl/belle"),
        showtimes = Seq(Showtime(LocalDateTime.parse("2026-06-28T20:00"), bookingUrl = Some("https://book.example/helios"))))))
    val ctrl = controller(single(TestReadModel.store(Seq(("Belle", Some(2021), rich)))))
    val html = contentAsString(ctrl.debugReadModel().apply(FakeRequest(GET, "/debug/readmodel")))

    html should include("<th>Cinemas</th>")
    html should include("""class="num cinemas """)
    html should include(""">2</td>""")
    Seq("Ryu to Sobakasu no Hime", "124", "Japonia", "Mamoru Hosoda", "Kaho Nakamura", "Animacja",
      "A shy teenager becomes a virtual-world pop star.", "https://img.example/belle.jpg").foreach(html should include(_))
    html should include("""<b>2</b> screenings""")
    html should not include ("https://book.example/cc")

    val filmId = """data-film-id="([^"]+)"""".r.findFirstMatchIn(html).map(_.group(1)).getOrElse(fail("no screenings slot"))
    val fragment = ctrl.debugReadModelScreenings(filmId).apply(FakeRequest(GET, s"/debug/readmodel/screenings?id=$filmId"))
    status(fragment) shouldBe OK
    val screenings = contentAsString(fragment)
    Seq("https://cinema-city.pl/belle", "https://book.example/cc", "Sala 4", "NAP").foreach(screenings should include(_))
  }

  it should "503 a read that could not complete, rather than list a smaller read model" in {
    val short = new InMemoryReadModelRepository() {
      override def foreachScreening(f: models.CityScreening => Unit) = tools.ScanOutcome.Incomplete(new RuntimeException("short"))
    }
    status(controller(single(short)).debugReadModel().apply(FakeRequest(GET, "/debug/readmodel"))) shouldBe SERVICE_UNAVAILABLE
  }

  it should "404 in production" in {
    status(controller(single(TestReadModel.store(records)), Mode.Prod)
      .debugReadModel().apply(FakeRequest(GET, "/debug/readmodel"))) shouldBe NOT_FOUND
  }

  "GET /debug/readmodel/screenings" should "503 a film whose screenings could not be read, rather than list it as screening nowhere" in {
    val failing = new InMemoryReadModelRepository() { override def findCard(id: String) = tools.ReadOutcome.Failed(tools.ReadFailure.Thrown(new RuntimeException("down"))) }
    status(controller(single(failing)).debugReadModelScreenings("x")
      .apply(FakeRequest(GET, "/debug/readmodel/screenings?id=x"))) shouldBe SERVICE_UNAVAILABLE
  }

  it should "404 in production" in {
    status(controller(single(TestReadModel.store(records)), Mode.Prod).debugReadModelScreenings("x")
      .apply(FakeRequest(GET, "/debug/readmodel/screenings?id=x"))) shouldBe NOT_FOUND
  }
}
