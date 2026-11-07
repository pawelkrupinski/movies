package tools

/**
 * The base of every [[HttpFetch]] decorator: each synchronous member — `get`, `get` with headers, `getBytes`, `post` —
 * goes through ONE hook, [[around]], which is handed the call to make and decides which fetch makes it and what
 * happens about it (pace it, gate it, count it, retry it). A decorator writes that hook once, and none of the members
 * can be forgotten: inheriting the trait's default `getBytes` round-trips a legacy single-byte page through UTF-8, and
 * its default header `get` drops the headers.
 *
 * `getAsync` keeps the trait's default — `get` on a pool thread, so it goes through [[around]] too. A decorator whose
 * async path differs (forwarded undecorated, metered on completion) overrides it.
 *
 * A decorator that treats one member differently (a cache answering `get` itself) overrides that member and leaves the
 * rest to the hook.
 */
abstract class ForwardingHttpFetch extends HttpFetch {

  /** Makes `call` for a request to `url`: hands it the fetch to ask, and returns (or throws) what it should. */
  protected def around[T](url: String)(call: HttpFetch => T): T

  override def get(url: String): String = around(url)(_.get(url))
  override def get(url: String, headers: Map[String, String]): String = around(url)(_.get(url, headers))
  override def getBytes(url: String): Array[Byte] = around(url)(_.getBytes(url))
  override def post(url: String, body: String, contentType: String): String = around(url)(_.post(url, body, contentType))
}
