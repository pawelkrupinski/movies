package controllers

import models.User
import play.api.test.Helpers
import services.users.InMemoryUserRepository

import java.time.Instant
import scala.concurrent.ExecutionContext

/** Builds the [[AdminAction]] gate the operational controllers now require, for
 *  specs. Defaults to a user repository holding one allowlisted admin
 *  (session id [[AdminUserId]] → [[AdminEmail]]), so a request carrying
 *  `withSession("userId" -> AdminUserId)` passes the gate; anything else gets
 *  401 (no session) or 403 (unknown / non-allowlisted email) — the same contract
 *  the real ADMIN_ALLOWLIST enforces in prod. */
object TestAdminAction {
  val AdminUserId = "admin1"
  val AdminEmail  = "admin@example.com"

  /** A user repository with one allowlisted admin. */
  def adminRepository: InMemoryUserRepository = {
    val r = new InMemoryUserRepository
    r.upsert(User(AdminUserId, "google", "sub-admin", Some(AdminEmail),
      Some("Admin"), None, Instant.EPOCH, Instant.EPOCH))
    r
  }

  def apply(
    users: InMemoryUserRepository = adminRepository,
    allow: Set[String]      = Set(AdminEmail)
  ): AdminAction =
    new AdminAction(Helpers.stubControllerComponents().parsers.anyContent, users, allow)(
      using ExecutionContext.global)
}
