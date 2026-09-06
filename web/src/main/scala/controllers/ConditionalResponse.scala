package controllers

import models.City
import play.api.mvc.Results.{NotModified, Ok}
import play.api.mvc.{RequestHeader, Result}

import java.time.Instant

/** Conditional-GET + compressed-response cache for a response that is
 *  byte-identical for every client at the current read-model version — the
 *  bare city listing, its `?filter=` variants, and the mobile JSON payloads.
 *
 *  A client whose `If-None-Match` offers this response's validator, or whose
 *  `If-Modified-Since` is still current, gets a bodiless 304 — so a browser
 *  refresh revalidates cheaply and reuses its cached copy instead of
 *  re-downloading the body.
 *
 *  Otherwise the body is served gzipped ([[AcceptEncoding.acceptsGzip]]) from
 *  the versioned [[EncodedResponseCache]]. Naming the `Content-Encoding`
 *  ourselves is also what keeps Play's `GzipFilter` off the response: it skips
 *  anything that already declares one. A `cacheBody = false` caller, or a
 *  client that refuses gzip, gets the body uncompressed and the filter handles
 *  it instead.
 *
 *  `modelStamp` is when the read model a payload draws on last moved — per
 *  city, or model-wide for a payload that really is (see [[serve]] for why the
 *  distinction matters). `now` is the clock the city's calendar day is read
 *  from; injectable so a spec can walk a validator across midnight.
 */
class ConditionalResponse(responseCache: EncodedResponseCache,
                          modelStamp: Option[City] => Instant,
                          now: () => Instant = () => Instant.now()) {

  /** Every response here varies by exactly this, on the 200s and the 304 alike:
   *  the class itself decides between the gzip and the identity representation. */
  private val Vary = "Accept-Encoding"

  /** Does this client take gzip — the ONE compressed form the origin offers.
   *
   *  ⚠️ NOT BROTLI, THOUGH IT WOULD BE A THIRD SMALLER. It was built here
   *  (eb90279cd) and taken off the wire (cedfaa4f6, 6ef854b81), then deleted:
   *  Cloudflare caches one variant per URL and does not forward the client's
   *  real `Accept-Encoding` even on a `BYPASS` response. It asked the origin with
   *  its own header, got br, and — barred by `no-transform` from re-compressing —
   *  DECOMPRESSED for gzip-only clients: 3,789,572 B on `/uk/manchester/` where
   *  297,089 was right, and 1,470,154 on `/api/repertoire`, the Android app's own
   *  fetch. Offering a second coding needs an edge that keys its cache on the
   *  encoding, which this zone cannot express. */
  private def acceptsGzip(request: RequestHeader): Boolean =
    AcceptEncoding.acceptsGzip(request.headers.get("Accept-Encoding"))

  private def ifModifiedSinceCurrent(request: RequestHeader, lastMod: Instant): Boolean =
    request.headers.get("If-Modified-Since").exists { ims =>
      scala.util.Try(java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.parse(ims))
        .map(Instant.from)
        .toOption
        .exists(!lastMod.isAfter(_))
    }

  /** The conditional response for `request`: a 304 when the client already holds
   *  the current version, otherwise `body` — gzipped from the cache when
   *  `cacheBody` and the client takes gzip, uncompressed for the `GzipFilter`
   *  otherwise.
   *
   *  `cacheKey` carries every input beyond host + path that changes the body (a
   *  normalised `?days=` window, or a filter variant's whole query string).
   *  `city` scopes the validator to one city's stamp and calendar day; `None` is
   *  a payload that really is model-wide.
   *
   *  `policy` decides what a cache may do with the result — see [[CachePolicy]] for
   *  the two, and for why every one of them carries `no-transform`. */
  def serve(request: RequestHeader, contentType: String, policy: CachePolicy,
            cacheKey: String = "", city: Option[City] = None,
            cacheBody: Boolean = true)(body: => String): Result = {
    // THE VALIDATOR IS PER CITY, not model-wide. `readModel.lastModified` moves
    // when anything anywhere changes, so validating London's payload with it
    // meant a Warsaw showtime expired London's ETag: every city looked like it
    // changed every couple of minutes, and the client 304s and the edge cache
    // both lost most of their value. `lastModifiedFor` moves only when the bytes
    // THIS city renders can have changed -- including the corpus-wide film-address
    // reshuffles that genuinely do reach every city. `None` means a payload that
    // really is model-wide.
    //
    // AND THE CITY'S CALENDAR DAY FLOORS IT. Every city-scoped payload here is
    // anchored on `LocalDate.now(city.zoneId)`: the listing renders that day's
    // `data-next-day` (the midnight the document retires itself at) and the
    // expiry stamps the client prunes forward from, and `/api/repertoire` cuts
    // its window from the same date. The read-model stamp knows nothing about
    // that -- so a quiet night would answer a request made AFTER midnight with a
    // 304 for a body rendered before it, handing back a document that has
    // already told itself to reload and reloads into the same 304. Flooring at
    // the day's start retires every held copy at the boundary the payload
    // itself names, and stays monotonic because both inputs only ever advance.
    val lastMod  = ConditionalResponse.dayFlooredValidator(modelStamp(city), city.map(_.zoneId), now())
      .truncatedTo(java.time.temporal.ChronoUnit.SECONDS)
    val httpDate = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME
      .format(lastMod.atOffset(java.time.ZoneOffset.UTC))
    // `no-transform` IS WHAT LETS THE ETAG BELOW REACH ANYONE. Cloudflare deletes
    // the ETag from every `text/html` response these two zones serve -- measured
    // 2026-09-06 on an UNCACHED (`cf-cache-status: BYPASS`) page, so it is not a
    // stale stored copy, and on BOTH domains; a weak tag was stripped exactly as a
    // strong one was, while the JSON built by this same method came through
    // untouched. The transform it is reserving the right to make is visible in the
    // response: the origin sends `content-encoding: gzip` and the edge hands the
    // client `content-encoding: br`, having recompressed the body. Bytes it rewrites
    // are bytes no validator of ours can describe, so it drops ours rather than
    // forward a lie. `no-transform` withdraws the permission (it also covers Email
    // Obfuscation, Rocket Loader and Mirage -- none of which have anything to do
    // here: the listing carries no email address and the edge body contains no
    // `cdn-cgi` marker).
    //
    // WHAT IT COST. Giving up the edge's brotli was worth it on its own: Cloudflare
    // answers a conditional ONLY from an ETag, so with none to send, a refreshing
    // browser was handed the entire body under a 200, and a revalidation went from
    // 287,531 bytes to a bodiless 304. But the full fetch did grow, 228,940 ->
    // 287,531, +26% -- and building the brotli here instead could not get it back;
    // see `acceptsGzip`.
    val cacheControl: Seq[(String, String)] = policy match {
      case CachePolicy.BrowserOnly         => Seq("Cache-Control" -> "private, no-cache, no-transform")
      case CachePolicy.RevalidatedAnywhere => Seq("Cache-Control" -> "public, max-age=0, must-revalidate, no-transform")
    }
    // ⚠️ THE KEY MUST CARRY EVERY INPUT THAT CHANGES THE BODY, and `request.path`
    // does not.
    //
    // It drops the query string: `?days=7` and the full payload are the same
    // path, so keying on it alone would serve one client's window to another --
    // silently, with a 200 and a plausible body. `cacheKey` is the normalised,
    // parsed parameter rather than the raw query wherever a blob is kept, so a
    // crawler appending `?foo=1` cannot mint unbounded entries.
    //
    // It also drops the HOST, and one deployment serves two of them -- a
    // country's own domain and the shared brand apex (`Country.servesApex`).
    // `og:url`, `<link rel=canonical>` and the JSON-LD are all built from
    // `PageMeta.origin`, so a blob rendered for the first host was handed to
    // the second advertising the wrong canonical URL for the page. A SHARED
    // cache keys on host itself and never saw this; it was ours getting it
    // wrong.
    val bodyKey = PageMeta.host(request) + request.path + cacheKey

    // AN ETAG AS WELL AS Last-Modified, BECAUSE A SHARED CACHE NEEDS ONE.
    //
    // Measured against the live edge on 2026-09-05: once Cloudflare holds a copy,
    // an `If-Modified-Since` against it comes back 200 WITH THE WHOLE BODY --
    // Cloudflare answers a conditional from cache off the ETag, and these
    // responses had none. So letting the edge hold them traded the mobile apps'
    // 0-byte 304s for ~750 KB payloads: better for the origin, worse for the
    // phone. (The TTL that first exposed this is gone; the ETag it forced is
    // what makes revalidation work at all.)
    // `/api/catalog` never had the problem precisely because it carries one.
    //
    // Derived from the read-model version and `bodyKey` rather than hashing the
    // body: the body is the expensive thing here (it is why the gzip cache
    // exists) and the version already changes exactly when the body does. ONE
    // key answers both "which body is this" questions -- which blob to reuse,
    // and which validator to stamp -- so the two cannot disagree about what
    // counts as a different page. They used to: the blob learned about the host
    // (below) and the ETag did not, leaving both hosts' pages sharing a
    // validator that named only the path.
    //
    // ⚠️ WEAK (`W/`), AND IT HAS TO BE — Cloudflare strips a STRONG ETag off
    // anything it serves as HTML. Measured 2026-09-06 on the same URL with the
    // same `Accept-Encoding: gzip`: straight at the k3s node with Cloudflare
    // bypassed, `/uk/manchester/` carried `etag: "7ea9812c-6a9cfc88"`; through
    // the edge it carried none, while `/uk/manchester/api/repertoire` — this
    // same line — kept its ETag both ways. The two bodies were byte-identical
    // (3826089 bytes), so nothing had in fact been rewritten: the zone simply
    // has an HTML-transforming feature on, and a strong validator is a promise
    // about BYTES that Cloudflare will not forward when it reserves the right
    // to change them. That is the whole reason fa7f5dd5a's ETag was not
    // reaching the edge it was added for.
    //
    // `W/` is not a concession made to get past that — it is what this
    // validator always was. It is `bodyKey.hashCode` + the read-model stamp, a
    // CONTENT VERSION rather than a hash of the body, and since b84004f84 the
    // filtered branch keeps no blob, so two responses sharing one validator can
    // legitimately differ in which showtimes have already started. The same tag
    // also goes on BOTH representations below -- gzip and identity -- which a
    // strong validator is flatly not permitted to do, since a strong tag promises
    // byte-equality and the two share no bytes at all. Weak says all of that out
    // loud.
    val etag = "W/\"" + Integer.toHexString(bodyKey.hashCode) + "-" + lastMod.getEpochSecond.toHexString + "\""
    val validators: Seq[(String, String)] = ("Last-Modified" -> httpDate) +: ("ETag" -> etag) +: cacheControl

    if (ConditionalResponse.offersValidator(request.headers.get("If-None-Match"), etag)
        || ifModifiedSinceCurrent(request, lastMod))
      // ⚠️ `Vary` ON THE 304 TOO, not just on the 200s below. RFC 9110 §15.4.5 makes
      // it a MUST, not a nicety -- a 304 "MUST generate any of the following header
      // fields that would have been sent in a 200 to the same request:
      // Content-Location, Date, ETag, and Vary". And this response needs it more
      // than most: ONE weak validator covers BOTH representations of the page --
      // gzip and identity -- which is legitimate precisely because it is weak, but
      // it means a cache that stores this 304's headers without being told the
      // response varies by `Accept-Encoding` can hand a gzip body to a client
      // that refused it.
      //
      // Measured on the live origin before this line existed: a 304 came back
      // `vary: Origin`, while the 200 beside it said `vary: Accept-Encoding,Origin`.
      // Cloudflare adds the missing token on the way out, so the edge looked
      // correct and the origin was not -- and nothing else in front of us would.
      NotModified.withHeaders((("Vary" -> Vary) +: validators)*)
    else if (cacheBody && acceptsGzip(request)) {
      // Gzipped HERE, and stamped `Content-Encoding` HERE, which is also what
      // keeps Play's GzipFilter off it: the filter skips any response that already
      // names an encoding. The uncached branch below deliberately names none, and
      // the filter gzips it on the way out.
      val bytes = responseCache.gzippedBody(bodyKey, lastMod)(body)
      Ok(bytes).as(contentType)
        .withHeaders((Seq("Content-Encoding" -> "gzip", "Vary" -> Vary) ++ validators)*)
    } else
      // An uncached response, or a client that refuses gzip: leave it uncompressed
      // and let the GzipFilter handle it, which is what keeps a filter variant
      // from minting a blob.
      Ok(body).as(contentType).withHeaders((("Vary" -> Vary) +: validators)*)
  }
}

object ConditionalResponse {

  private val OfferedEntityTag = """(?:[Ww]/)?("[^"]*")""".r

  /** The opaque part of an entity tag — the quoted string, with any `W/` weakness
   *  marker dropped. `W/"abc"` and `"abc"` both reduce to `"abc"`. */
  private def opaqueTag(entityTag: String): String =
    entityTag.trim.stripPrefix("W/").stripPrefix("w/")

  /** Does the client's `If-None-Match` offer `etag`?
   *
   *  WEAK comparison, which is the one RFC 9110 §13.1.2 mandates for
   *  `If-None-Match` — two tags match when their opaque parts are equal, whether
   *  or not either carries the `W/` marker. Exact string equality was survivable
   *  only while we emitted one spelling of one tag and nothing in the path
   *  touched it; now that the validator IS weak, a cache is free to hand it back
   *  bare, and answering that with a 200 would re-send the whole ~750 KB listing
   *  to a client that already holds it.
   *
   *  The header is a LIST — a browser holding two variants of a URL offers both,
   *  comma-separated — so every tag in it is considered, not just a header that
   *  equals ours outright. Tags are matched by pattern rather than split on `,`
   *  because a comma is a legal character inside the quoted part; a tag we fail
   *  to parse simply does not match, which costs a body, never a wrong 304.
   *
   *  `*` matches any current representation, per the same section. */
  def offersValidator(ifNoneMatch: Option[String], etag: String): Boolean =
    ifNoneMatch.exists { header =>
      header.trim == "*" ||
        OfferedEntityTag.findAllMatchIn(header).exists(_.group(1) == opaqueTag(etag))
    }

  /** The validator instant for a payload, floored at the start of the day it was
   *  rendered for.
   *
   *  `modelStamp` is when the read model this payload draws on last moved. That
   *  is the whole story for a payload that says the same thing at any hour, and
   *  none of it for the ones here, which are cut against `LocalDate.now(zone)`:
   *  the listing carries that day's retire-at-midnight stamp, and the repertoire
   *  API cuts its window from that date. On a quiet night the model stamp can
   *  sit still across midnight, and a client revalidating at 00:05 would then be
   *  told 304 for a body belonging to the day before -- a document that has
   *  already scheduled its own reload, and that reloads straight back into the
   *  same 304.
   *
   *  So a zoned payload's validator is the LATER of the two. Both inputs only
   *  advance, so the result is monotonic, which is what a validator has to be:
   *  a repeated value must mean unchanged bytes. `None` is a payload with no
   *  day in it, which keeps the model stamp alone.
   */
  def dayFlooredValidator(modelStamp: Instant,
                          zone: Option[java.time.ZoneId],
                          now: Instant = Instant.now()): Instant =
    zone.map(z => now.atZone(z).toLocalDate.atStartOfDay(z).toInstant) match {
      case Some(dayStart) if dayStart.isAfter(modelStamp) => dayStart
      case _                                              => modelStamp
    }
}
