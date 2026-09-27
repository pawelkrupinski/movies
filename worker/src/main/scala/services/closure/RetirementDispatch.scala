package services.closure

import models.{Cinema, GermanRoster, SpanishRoster, UsRoster}
import play.api.libs.json.Json
import services.scrapes.VenueClosure.ClosureEvidence

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration

/** A data-driven roster's directory under `data/` — the one whose `retired.json` takes
 *  the venue's id (see `data/scripts/retired_venues.py`). */
final case class RosterDirectory(value: String)

/** Where a venue sits in a data-driven roster. Poland's and the UK's hand-authored
 *  venues have none: those are retired by hand. */
final case class RosterEntry(directory: RosterDirectory, id: String)

object RosterEntry {
  def of(cinema: Cinema): Option[RosterEntry] =
    GermanRoster.theaterIdByCinema.get(cinema).map(RosterEntry(RosterDirectory("germany"), _))
      .orElse(SpanishRoster.theaterIdByCinema.get(cinema).map(RosterEntry(RosterDirectory("spain"), _)))
      .orElse(UsRoster.flicksSlugByCinema.get(cinema).map(RosterEntry(RosterDirectory("us"), _)))
}

final case class RetirementRequest(entry: RosterEntry, name: String, evidence: ClosureEvidence)

/** Asks for confirmed-closed venues to be taken off their roster. THROWS when the
 *  request did not go through, so the sweep can try again next time. */
trait RetirementDispatch {
  def request(directory: RosterDirectory, venues: Seq[RetirementRequest]): Unit
}

/** Starts `.github/workflows/retire-venues.yml`, which re-checks each venue live, adds
 *  it to `data/<directory>/retired.json`, regenerates the roster and opens a PR. */
final class GitHubRetirementDispatch(token: settings.GitHubDispatchToken, client: HttpClient) extends RetirementDispatch {

  def request(directory: RosterDirectory, venues: Seq[RetirementRequest]): Unit = {
    val body = Json.obj("ref" -> "main", "inputs" -> Json.obj(
      "roster" -> directory.value,
      "venues" -> Json.stringify(Json.toJson(venues.map(GitHubRetirementDispatch.venueJson)))))
    val request = HttpRequest.newBuilder(URI.create(GitHubRetirementDispatch.Endpoint)).timeout(Duration.ofSeconds(20))
      .header("Authorization", s"Bearer ${token.value}")
      .header("Accept", "application/vnd.github+json")
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(Json.stringify(body))).build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    if (response.statusCode() / 100 != 2)
      throw new IllegalStateException(s"retire-venues dispatch: HTTP ${response.statusCode()} ${response.body().take(300)}")
  }
}

object GitHubRetirementDispatch {
  val Endpoint = "https://api.github.com/repos/pawelkrupinski/movies/actions/workflows/retire-venues.yml/dispatches"

  /** One venue as the workflow reads it: its id, name, and the evidence for the PR body. */
  def venueJson(venue: RetirementRequest) = Json.obj(
    "id"       -> venue.entry.id,
    "name"     -> venue.name,
    "evidence" -> ClosureSweep.evidenceLine(venue.evidence))
}
