package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.AgreementQuestionMetrics
import services.identity.agreement.{AgreementStage, VoterFamily}

/** The agreement's series read 0 from boot — a backlog asked and none at all look different from no data — and carry
 *  the stage's last pass and each question's outcome. */
class IdentityAgreementMetricsSpec extends AnyFlatSpec with Matchers {
  import PrometheusExposition.{render, sample}

  "the agreement's series" should "read 0 from boot, before the stage or any question reports" in {
    val registry = new PrometheusRegistry()
    val metrics  = new IdentityAgreementMetrics(registry)
    metrics.stage("pl"); metrics.questions("pl")
    val text = render(registry)
    sample(text, "kinowo_worker_identity_agreement_clusters", """country="pl",state="waiting"""") shouldBe Some(0.0)
    sample(text, "kinowo_worker_identity_agreement_open_questions", """country="pl",family="tmdb-find"""") shouldBe Some(0.0)
    sample(text, "kinowo_worker_identity_agreement_answers_total", """country="pl",family="rt",outcome="failed"""") shouldBe Some(0.0)
  }

  it should "carry the stage's last pass and each question's outcome" in {
    val registry = new PrometheusRegistry()
    val metrics  = new IdentityAgreementMetrics(registry)
    metrics.stage("pl").applied(AgreementStage.Applied(waiting = 40, verdicts = 12, agreed = 5, takenTmdb = 4, takenFallback = 1,
      open = Map(VoterFamily.Imdb -> 30, VoterFamily.RottenTomatoes -> 7), finds = 2, resolves = 9, seconds = 0.5))
    val questions = metrics.questions("pl")
    questions.enqueued("imdb", added = true); questions.enqueued("imdb", added = false)
    questions.asked("rt", AgreementQuestionMetrics.Answered); questions.asked("rt", AgreementQuestionMetrics.Nothing)
    val text = render(registry)
    sample(text, "kinowo_worker_identity_agreement_clusters", """country="pl",state="agreed"""") shouldBe Some(5.0)
    sample(text, "kinowo_worker_identity_agreement_taken", """as="tmdb",country="pl"""")
      .orElse(sample(text, "kinowo_worker_identity_agreement_taken", """country="pl",as="tmdb"""")) shouldBe Some(4.0)
    sample(text, "kinowo_worker_identity_agreement_open_questions", """country="pl",family="imdb"""") shouldBe Some(30.0)
    sample(text, "kinowo_worker_identity_agreement_open_questions", """country="pl",family="wiki"""") shouldBe Some(0.0)
    sample(text, "kinowo_worker_identity_agreement_open_questions", """country="pl",family="tmdb-find"""") shouldBe Some(2.0)
    sample(text, "kinowo_worker_identity_agreement_resolves_total", """country="pl"""") shouldBe Some(9.0)
    sample(text, "kinowo_worker_identity_agreement_enqueued_total", """country="pl",family="imdb",result="duplicate"""") shouldBe Some(1.0)
    sample(text, "kinowo_worker_identity_agreement_answers_total", """country="pl",family="rt",outcome="nothing"""") shouldBe Some(1.0)
  }
}
