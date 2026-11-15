package services.cinemas.uk

/** The Vista bearer [[OdeonClient]]'s ocapi calls carry: production's [[OdeonAuthHarvester]], or a fixed one. */
trait OdeonBearer {

  /** The bearer to send, or `None` when there is none to be had (the client then throws, and the venue rides its
   *  flicks fallback). */
  def token(): Option[String]

  /** ocapi answered `token` with a 401: it is revoked before its `exp`. Drop it if it is still the one served, so the
   *  next [[token]] fetches a fresh one; a token already replaced is left alone. */
  def refused(token: String): Unit
}

object OdeonBearer {

  /** A bearer that never changes (`None`: no bearer at all) — a fixture wiring's or a spec's. A refusal changes
   *  nothing: there is no other token to fetch. */
  def fixed(bearer: Option[String]): OdeonBearer = new OdeonBearer {
    def token(): Option[String]       = bearer
    def refused(token: String): Unit = ()
  }
}
