package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The task queue is per-deployment: each country's worker drains its own queue
 * off its own db. The header names the served country and — since more than one
 * country is deployed — offers a hop to the SAME page on the other country's
 * host. Mirrors `/uptime` (`UptimeViewSpec`), which shares the switcher partial.
 */
class TasksViewSpec extends AnyFlatSpec with Matchers {

  private val html = views.html.tasks().body

  "the tasks header" should "offer a switch to another country's task queue on that country's host" in {
    html should include ("""class="country-switch"""")
    html should include ("""value="https://showtimes-uk.fly.dev/tasks"""")
    html should include ("""value="https://showtimes-de.fly.dev/tasks"""")
  }

  it should "point the switcher at /tasks, not another admin page" in {
    // Guards the partial being reused with the wrong `path`: /uptime is the
    // other caller, and pasting its path here would silently teleport you off
    // the task queue.
    html should not include ("/uptime")
  }

  it should "mark this deployment's own country as the selected option" in {
    // KINOWO_COUNTRY unset in tests → Poland; its option is pre-selected.
    html should include ("""value="https://kinowo.fly.dev/tasks" selected""")
  }

  it should "select the served country's own option, and brand the title after it" in {
    // A German deployment serves the same page under its own brand, with its
    // own option pre-selected — never Poland's.
    val out = views.html.tasks(current = models.Country.Germany).body

    out should include ("<title>Tasks — Showtimes</title>")
    out should include ("""value="https://showtimes-de.fly.dev/tasks" selected""")
    out should not include ("""value="https://kinowo.fly.dev/tasks" selected""")
  }
}
