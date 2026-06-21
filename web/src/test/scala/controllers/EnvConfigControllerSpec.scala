package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, JsObject, Json}
import play.api.test.{FakeRequest, Helpers}
import play.api.test.Helpers._
import services.config.{EnvConfigService, InMemoryEnvOverrideStore, InMemoryEnvRegistryStore, RegisteredKnob}
import tools.Env

/**
 * The /admin/config page serves a static shell and exposes the knobs via
 * `/admin/config/data`; Set/Reset are JSON POSTs that write the override store.
 * Driven through the real [[EnvConfigService]] + in-memory stores so the
 * controller↔service contract (merge, validation, secret exclusion, auth) is
 * exercised end to end.
 */
class EnvConfigControllerSpec extends AnyFlatSpec with Matchers {

  private def serviceWith(over: InMemoryEnvOverrideStore, reg: InMemoryEnvRegistryStore) =
    new EnvConfigService("web", over, reg, knobSource = () => Seq.empty, currentValueOf = _ => None)

  private def fixture() = {
    val over = new InMemoryEnvOverrideStore
    val reg  = new InMemoryEnvRegistryStore
    reg.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_TICK", Env.Kind.Long, Some("60"), Some("60"))))
    reg.publish("web",    Seq(RegisteredKnob("web",    "KINOWO_TICK", Env.Kind.Long, Some("60"), Some("60"))))
    (over, reg)
  }

  private def controller(over: InMemoryEnvOverrideStore, reg: InMemoryEnvRegistryStore,
                         gate: AdminAction = TestAdminAction()) =
    new EnvConfigController(Helpers.stubControllerComponents(), gate, serviceWith(over, reg))

  private val adminSession = FakeRequest().withSession("userId" -> TestAdminAction.AdminUserId)
  private def jsonReq(body: JsObject, session: Boolean = true) =
    (if (session) adminSession else FakeRequest()).withBody(body).withHeaders("Content-Type" -> "application/json")

  "GET /admin/config" should "render the page shell that polls the data feed" in {
    val (over, reg) = fixture()
    val result = controller(over, reg).index.apply(adminSession)
    status(result) shouldBe OK
    contentAsString(result) should include ("/admin/config/data")
    contentAsString(result) should include ("Config knobs")
  }

  "GET /admin/config/data" should "list each knob merged across apps with default and no override yet" in {
    val (over, reg) = fixture()
    val result = controller(over, reg).data.apply(adminSession)
    contentType(result) shouldBe Some("application/json")
    val rows = (Json.parse(contentAsString(result)) \ "rows").as[JsArray].value
    rows.map(r => (r \ "key").as[String]) shouldBe Seq("KINOWO_TICK")
    val row = rows.head
    (row \ "default").as[String] shouldBe "60"
    (row \ "override").asOpt[String] shouldBe None             // unset → page shows the Set affordance
    (row \ "apps").as[JsArray].value.map(a => (a \ "app").as[String]) should contain allOf ("web", "worker")
  }

  it should "carry the human-readable secondary unit for a time knob (seconds → minutes)" in {
    val reg = new InMemoryEnvRegistryStore
    reg.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_GAP_SECONDS", Env.Kind.Long, Some("120"), Some("120"))))
    val over = new InMemoryEnvOverrideStore
    over.set("KINOWO_GAP_SECONDS", "90")
    val rows = (Json.parse(contentAsString(controller(over, reg).data.apply(adminSession))) \ "rows").as[JsArray].value
    val row = rows.head
    (row \ "defaultHuman").as[String] shouldBe "2 min"
    (row \ "overrideHuman").as[String] shouldBe "1.5 min"
    (row \ "apps").as[JsArray].value.head.\("currentHuman").as[String] shouldBe "2 min"
  }

  "POST /admin/config/set" should "apply a valid numeric override and surface it on the next data read" in {
    val (over, reg) = fixture()
    val c = controller(over, reg)
    status(c.set.apply(jsonReq(Json.obj("key" -> "KINOWO_TICK", "value" -> "15")))) shouldBe OK
    over.lookup("KINOWO_TICK") shouldBe Some("15")
    val rows = (Json.parse(contentAsString(c.data.apply(adminSession))) \ "rows").as[JsArray].value
    (rows.head \ "override").as[String] shouldBe "15"
  }

  it should "400 a non-numeric value for a Long knob without changing the override" in {
    val (over, reg) = fixture()
    status(controller(over, reg).set.apply(jsonReq(Json.obj("key" -> "KINOWO_TICK", "value" -> "soon")))) shouldBe BAD_REQUEST
    over.lookup("KINOWO_TICK") shouldBe None
  }

  it should "400 an unknown or secret key" in {
    val (over, reg) = fixture()
    val c = controller(over, reg)
    status(c.set.apply(jsonReq(Json.obj("key" -> "KINOWO_NOPE", "value" -> "1")))) shouldBe BAD_REQUEST
    status(c.set.apply(jsonReq(Json.obj("key" -> "ZYTE_API_KEY", "value" -> "x")))) shouldBe BAD_REQUEST
  }

  "POST /admin/config/reset" should "remove an existing override" in {
    val (over, reg) = fixture()
    over.set("KINOWO_TICK", "9")
    status(controller(over, reg).reset.apply(jsonReq(Json.obj("key" -> "KINOWO_TICK")))) shouldBe OK
    over.lookup("KINOWO_TICK") shouldBe None
  }

  // ── Auth gate: closed like the other admin pages ─────────────────────────────
  "the /admin/config gate" should "401 anonymous and 403 a non-allowlisted user" in {
    val (over, reg) = fixture()
    status(controller(over, reg).index.apply(FakeRequest())) shouldBe UNAUTHORIZED
    status(controller(over, reg).data.apply(FakeRequest())) shouldBe UNAUTHORIZED
    val gate = TestAdminAction(allow = Set("someone-else@example.com"))
    status(controller(over, reg, gate).data.apply(adminSession)) shouldBe FORBIDDEN
  }
}
