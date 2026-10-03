package models

import java.time.ZoneId

/**
 * The zones the product keeps calendars in, named once. Production code asks [[VenueClock]] for a
 * local date or time and names a zone through here (or through the venue's [[City]]) — never a
 * zone-id string of its own, nor the JVM's default zone, which on a pod is UTC (`NoDefaultZoneSpec`).
 */
object TimeZones {
  val Poland: ZoneId        = ZoneId.of("Europe/Warsaw")
  val UnitedKingdom: ZoneId = ZoneId.of("Europe/London")
  val Germany: ZoneId       = ZoneId.of("Europe/Berlin")
  /** Peninsular Spain and the Balearics; the Canaries run an hour behind ([[Canary]]). */
  val Spain: ZoneId         = ZoneId.of("Europe/Madrid")
  val Canary: ZoneId        = ZoneId.of("Atlantic/Canary")
  val UsEastern: ZoneId     = ZoneId.of("America/New_York")
  val UsCentral: ZoneId     = ZoneId.of("America/Chicago")

  /** A zone named by roster DATA (a venue's or metro's own `zoneId` field). */
  def named(id: String): ZoneId = ZoneId.of(id)
}
