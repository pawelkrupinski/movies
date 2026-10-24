package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.VoterFamily
import services.tasks.InMemoryTaskQueue
import tools.HostCircuitBreakerHttpFetch.Transition
import tools.{CircuitOpenException, HostCircuitBreakerHttpFetch, HttpFetch, HttpStatusException, MutableClock}

import java.time.{Duration, Instant}
import scala.concurrent.duration._

/** A host that is back has the identity questions that failed against it asked again at once, not after their failure
 *  wait (the owner, 2026-10-24): an open breaker closing, or an answer after a run of failures that never opened it. */
class HostRecoverySpec extends AnyFlatSpec with Matchers {
  private val imdbHost = "v3.sg.media-imdb.com"
  private val wikiHost = "query.wikidata.org"
  private val imdb     = Question.Family(VoterFamily.Imdb, "title|Lalka")
  private val wiki     = Question.Family(VoterFamily.Wiki, "title|Lalka")
  private val imdbUrl  = s"https://$imdbHost/suggestion/l/lalka.json"

  private class World {
    val clock  = new MutableClock(Instant.parse("2026-10-24T12:00:00Z"))
    val memory = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    val queue  = new InMemoryTaskQueue
    var open: Set[Question] = Set(imdb, wiki)
    var fills  = 0
    val said   = scala.collection.mutable.Buffer.empty[String]
    val recovery = new HostRecovery(memory, () => open, AgreementQuestions.enqueue(queue, _, memory, clock), () => fills += 1,
      line => { said += line; () })
    def queued: Long = queue.monitor().counts.values.sum
    /** Each open question asked and failed against its host, as the asker records it. */
    def failedAgainstHosts(): Unit = {
      memory.failed(Seq(imdb), AgreementQuestions.hostOf(new HttpStatusException(503, "GET", imdbUrl, None)))
      memory.failed(Seq(wiki), Some(wikiHost))
    }
  }

  private class Switching extends HttpFetch {
    @volatile var respond: String => String = url => throw new HttpStatusException(503, "GET", url, None)
    override def get(url: String): String = respond(url)
    override def post(url: String, body: String, contentType: String): String = respond(url)
  }

  "a host back" should "queue again, at once, the open questions whose last ask failed against it, and ask for a fill round" in {
    val world = new World
    world.failedAgainstHosts()
    world.memory.due(world.open) shouldBe empty                            // inside their failure wait
    world.clock.advance(Duration.ofSeconds(90))
    world.recovery.recovered(imdbHost) shouldBe Set(imdb)
    world.queued shouldBe 1
    world.memory.due(world.open) shouldBe Set(imdb)                        // Wikidata's failure still waits
    world.fills shouldBe 1
    world.said.toSeq shouldBe Seq(s"$imdbHost back: 1 questions asked again")
  }

  it should "queue nothing a host other than the one back failed, nor a question no longer open" in {
    val world = new World
    world.failedAgainstHosts()
    world.open = Set(wiki)
    world.recovery.recovered(imdbHost) shouldBe empty
    world.queued shouldBe 0
  }

  "the enrichment breaker" should "release a host's questions when it closes after an outage, and on an answer after failures that never opened it" in {
    val world    = new World
    val delegate = new Switching
    val breaker  = new HostCircuitBreakerHttpFetch(delegate, failureThreshold = 4, openDuration = 60.seconds, clock = world.clock,
      onTransition = world.recovery.heard)
    (1 to 4).foreach(_ => a[HttpStatusException] should be thrownBy breaker.get(imdbUrl))
    world.failedAgainstHosts()
    a[CircuitOpenException] should be thrownBy breaker.get(imdbUrl)
    world.queued shouldBe 0                                                // opening releases nothing
    world.clock.advance(Duration.ofSeconds(61))
    delegate.respond = _ => "{}"
    breaker.get(imdbUrl) shouldBe "{}"
    world.queued shouldBe 1

    val wikiUrl = s"https://$wikiHost/sparql"
    delegate.respond = url => throw new HttpStatusException(503, "GET", url, None)
    (1 to QuestionMemory.MinOutageReads).foreach(_ => a[HttpStatusException] should be thrownBy breaker.get(wikiUrl))
    delegate.respond = _ => "{}"
    breaker.get(wikiUrl) shouldBe "{}"
    world.queued shouldBe 2
  }

  it should "hold a question's backoff when its host answers after fewer failures than an outage takes" in {
    HostRecovery.recovers(Transition.Answered(QuestionMemory.MinOutageReads - 1)) shouldBe false
    HostRecovery.recovers(Transition.Opened) shouldBe false
    HostRecovery.recovers(Transition.Closed) shouldBe true
  }
}
