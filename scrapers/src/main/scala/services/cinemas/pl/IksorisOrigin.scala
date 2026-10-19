package services.cinemas.pl

/** The origin of one venue's iKsoris ticketing site (`https://kinoplon.pl`) —
 *  SoftCOM Wrocław's white-label platform. An install serves its booking page
 *  ([[IksorisBookingClient]]), its day-by-day repertoire ([[IksorisRepertoireClient]]),
 *  or both; which one a venue has is per install, so each client reads its own. */
final case class IksorisOrigin(value: String) extends AnyVal
