package settings

/** `DECODO_PROXY_USER` — the residential egress's user. */
final case class ProxyUser(value: String) extends AnyVal
/** `DECODO_PROXY_PASS` — the residential egress's password. */
final case class ProxyPassword(value: String) extends AnyVal
