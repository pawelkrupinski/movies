package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The task queue is per-deployment: each country's worker drains its own queue
 * off its own db. The header names the served country, and offers a hop to the
 * same page on another country's host only while more than one country is
 * deployed. Poland has been the only one since 2026-08-02, so the shared
 * switcher partial renders its label branch. Mirrors `/uptime`
 * (`UptimeViewSpec`), which shares that partial.
 */
class TasksViewSpec extends AnyFlatSpec with Matchers {

  private val html = views.html.tasks().body

  "the tasks header" should "name the served country without a dead switcher" in {
    // KINOWO_COUNTRY unset in tests → Poland.
    html should include ("""class="country-switch-label"""")
    html should include (">Polska<")
    html should not include ("""class="country-switch"""")
  }

  it should "not offer a stopped deployment's task queue" in {
    html should not include ("showtimes-uk.fly.dev")
    html should not include ("showtimes-de.fly.dev")
  }

  it should "point at /tasks, not another admin page" in {
    // Guards the partial being reused with the wrong `path`: /uptime is the
    // other caller, and pasting its path here would silently teleport you off
    // the task queue.
    html should not include ("/uptime")
  }

  it should "name the served country, and brand the title after it" in {
    // A German deployment would serve the same page under its own brand, naming
    // its own country — never Poland's.
    val out = views.html.tasks(current = models.Country.Germany).body

    out should include ("<title>Tasks — Showtimes</title>")
    out should include (">Deutschland<")
    out should not include (">Polska<")
  }
}
