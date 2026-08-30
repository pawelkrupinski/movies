package tools

import models.{User, UserState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/**
 * The merge rules behind [[SharedUsersMigration]] — the part that decides what
 * a person's ONE account looks like once four countries' copies of them are
 * folded together.
 *
 * Worth asserting rather than eyeballing because the migration runs once, over
 * real accounts, and every mistake it can make is quiet: a re-dated signup, a
 * blanked avatar, a set of hidden films silently dropped because the other
 * country's row happened to be newer.
 */
class SharedUsersMigrationSpec extends AnyFlatSpec with Matchers {

  private val Early = Instant.parse("2026-01-01T00:00:00Z")
  private val Mid   = Instant.parse("2026-05-01T00:00:00Z")
  private val Late  = Instant.parse("2026-08-30T00:00:00Z")

  private def user(
    id: String, createdAt: Instant, lastSeenAt: Instant,
    displayName: Option[String] = Some("Alice"),
    avatarUrl: Option[String]   = Some("https://lh3/avatar"),
    provider: String            = "google"
  ) = User(id, provider, s"sub-$provider-$id", Some(id), displayName, avatarUrl, createdAt, lastSeenAt)

  // ── Users ────────────────────────────────────────────────────────────────

  "mergeUsers" should "fold the same person's rows from several countries into one" in {
    val merged = SharedUsersMigration.mergeUsers(Seq(
      user("alice@example.com", Early, Mid),
      user("alice@example.com", Mid,   Late),
      user("bob@example.com",   Mid,   Mid)))

    merged.map(_.id) shouldBe Seq("alice@example.com", "bob@example.com")
  }

  it should "keep the EARLIEST signup — the account is as old as the first country it was made on" in {
    val merged = SharedUsersMigration.mergeUser(Seq(
      user("alice@example.com", Mid,   Late),
      user("alice@example.com", Early, Mid)))

    merged.createdAt shouldBe Early
  }

  it should "keep the latest sighting and the profile that came with it" in {
    val merged = SharedUsersMigration.mergeUser(Seq(
      user("alice@example.com", Early, Mid,  displayName = Some("Alice")),
      user("alice@example.com", Early, Late, displayName = Some("Alice (married)"))))

    merged.lastSeenAt  shouldBe Late
    merged.displayName shouldBe Some("Alice (married)")
  }

  // A country where the visitor declined a field must not blank one another
  // country has — that is a profile getting worse for having been merged.
  it should "fill gaps in the newest row from the older ones rather than blanking them" in {
    val merged = SharedUsersMigration.mergeUser(Seq(
      user("alice@example.com", Early, Mid,  displayName = Some("Alice"), avatarUrl = Some("https://lh3/avatar")),
      user("alice@example.com", Early, Late, displayName = None,          avatarUrl = None)))

    merged.lastSeenAt  shouldBe Late
    merged.displayName shouldBe Some("Alice")
    merged.avatarUrl   shouldBe Some("https://lh3/avatar")
  }

  it should "leave a person who only ever existed on one country untouched" in {
    val only = user("solo@example.com", Early, Mid)
    SharedUsersMigration.mergeUsers(Seq(only)) shouldBe Seq(only)
  }

  // Re-runnable: more sign-ins land, run it again, nothing already folded moves.
  it should "be idempotent — merging the merged result changes nothing" in {
    val rows   = Seq(user("alice@example.com", Early, Mid), user("alice@example.com", Mid, Late))
    val once   = SharedUsersMigration.mergeUsers(rows)
    SharedUsersMigration.mergeUsers(once) shouldBe once
  }

  // ── User state ───────────────────────────────────────────────────────────

  private def state(
    userId: String, updatedAt: Instant,
    hidden: Set[String] = Set.empty, cinemas: Set[String] = Set.empty,
    selected: Set[String] = Set.empty, rooms: Set[String] = Set.empty
  ) = UserState(userId, hidden, cinemas, updatedAt, selected, rooms)

  // UNION, not last-write-wins. The keys are already global and out-of-city
  // entries are inert (see `UserState`'s own note), so keeping both countries'
  // choices shows nothing anywhere it does not belong — whereas picking the
  // newer row would throw a country's worth of choices away on a timestamp.
  "mergeStates" should "union what the visitor chose on each country" in {
    val merged = SharedUsersMigration.mergeState(Seq(
      state("alice@example.com", Mid,  hidden = Set("Madagaskar"), cinemas = Set("Helios Posnania")),
      state("alice@example.com", Late, hidden = Set("Dune"),       cinemas = Set("Cineworld Kent"))))

    merged.hiddenFilms     shouldBe Set("Madagaskar", "Dune")
    merged.disabledCinemas shouldBe Set("Helios Posnania", "Cineworld Kent")
  }

  it should "union the /plan picks too" in {
    val merged = SharedUsersMigration.mergeState(Seq(
      state("alice@example.com", Mid,  selected = Set("Dune"),  rooms = Set("Helios Posnania|3")),
      state("alice@example.com", Late, selected = Set("Wicked"), rooms = Set("Cineworld Kent|IMAX"))))

    merged.selectedMovies shouldBe Set("Dune", "Wicked")
    merged.favouriteRooms shouldBe Set("Helios Posnania|3", "Cineworld Kent|IMAX")
  }

  it should "carry the latest updatedAt of the rows it folded" in {
    SharedUsersMigration.mergeState(Seq(
      state("alice@example.com", Late),
      state("alice@example.com", Mid))).updatedAt shouldBe Late
  }

  it should "keep separate people separate" in {
    val merged = SharedUsersMigration.mergeStates(Seq(
      state("alice@example.com", Mid, hidden = Set("Dune")),
      state("bob@example.com",   Mid, hidden = Set("Wicked"))))

    merged.map(_.userId)      shouldBe Seq("alice@example.com", "bob@example.com")
    merged.map(_.hiddenFilms) shouldBe Seq(Set("Dune"), Set("Wicked"))
  }

  it should "be idempotent — merging the merged result changes nothing" in {
    val rows = Seq(
      state("alice@example.com", Mid,  hidden = Set("Madagaskar")),
      state("alice@example.com", Late, hidden = Set("Dune")))
    val once = SharedUsersMigration.mergeStates(rows)
    SharedUsersMigration.mergeStates(once) shouldBe once
  }
}
