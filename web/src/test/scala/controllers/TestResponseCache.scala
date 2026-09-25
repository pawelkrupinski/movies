package controllers

import java.time.Instant
import scala.concurrent.ExecutionContext

/** An [[EncodedResponseCache]] for specs that are not about WHEN a superseded copy
 *  is re-rendered: the background refresh runs inline, on the requesting thread,
 *  once that request has already been answered from the previous copy — so the
 *  request after a change still gets the old body, and the one after that the new
 *  one, with no thread to wait for. The clock stands still, so a held copy never
 *  ages past [[EncodedResponseCache.MaxStaleAge]]. A spec about the refresh itself
 *  drives the executor and clock by hand ([[StaleWhileRevalidateSpec]]). */
object TestResponseCache {
  val FixedNow: Instant = Instant.parse("2026-06-10T10:00:00Z")

  def apply(maxBytes: Long = EncodedResponseCache.DefaultMaxBytes): EncodedResponseCache =
    new EncodedResponseCache(ExecutionContext.parasitic, () => FixedNow, maxBytes)
}
