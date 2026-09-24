package tools

import com.mongodb.{MongoWriteException, ServerAddress, WriteError}
import org.bson.BsonDocument
import services.cinemas.common.ZyteClient
import tools.contracts.RetryClassificationTable.Row

import java.io.IOException
import java.net.http.{HttpConnectTimeoutException, HttpTimeoutException}
import scala.util.Try

/** The failure a row of the retry-classification table names, built the way production
 *  builds it — through `ZyteClient`'s own throw helpers, with the exact message the JDK
 *  gives a refused proxy tunnel — so a spec feeds the classifiers what they really meet. */
object RetryClassificationFailures {

  val Url = "https://www.odeon.co.uk/api/v1/showtimes?date=2026-09-24"

  def of(row: Row): Throwable = (row.source, row.error) match {
    case ("origin", _)          => origin(row)
    case ("origin-via-zyte", _) => thrown(ZyteClient.bodyBytesOrThrow(s"""{"statusCode":${status(row)}}""", Url))
    case ("zyte", "no-origin-status") => thrown(ZyteClient.bodyBytesOrThrow("{}", Url))
    case ("zyte", _)            => thrown(ZyteClient.apiBodyOrThrow(status(row), """{"title":"refused"}""", Url))
    // What java.net.http throws when the proxy answers CONNECT with a non-200: a 407 with an
    // Authenticator installed (as RealHttpFetch's proxy config does) after its three tries,
    // anything else at once. Probed against a stub proxy on JDK 21, 2026-09-24.
    case ("decodo", _) if status(row) == 407 => new IOException("too many authentication attempts. Limit: 3")
    case ("decodo", _)          => new IOException(s"Tunnel failed, got: ${status(row)}")
    case ("task", "require")    => thrown(require(false, "rekey requires same normalised cleanTitle"))
    case ("task", "illegal-argument") => new IllegalArgumentException("unknown cinema slug")
    case ("task", "number-format")    => thrown("<html>502".toInt)
    case ("task", "illegal-state")    => new IllegalStateException("run already reduced")
    case ("task", "runtime")    => new RuntimeException("All 2 backends failed for get " + Url)
    case ("task", "io")         => new IOException("Connection reset")
    case ("mongo-write", "duplicate-key")        => mongoWrite(11000, "E11000 duplicate key error collection: kinowo.tasks index: dedupKey_1")
    case ("mongo-write", "write-conflict")       => mongoWrite(112, "WriteConflict error: this operation conflicted with another operation")
    case ("mongo-write", "not-writable-primary") => mongoWrite(10107, "not primary")
    case _ => throw new IllegalArgumentException(s"no failure built for $row — teach RetryClassificationFailures this row")
  }

  private def origin(row: Row): Throwable = row.error match {
    case "timeout"            => new HttpTimeoutException("request timed out")
    case "connect-timeout"    => new HttpConnectTimeoutException("HTTP connect timed out")
    case "connection-refused" => new java.net.ConnectException("Connection refused")
    case "unknown-host"       => new java.net.UnknownHostException("www.odeon.co.uk")
    case "tls-handshake"      => new javax.net.ssl.SSLHandshakeException("PKIX path building failed")
    case _                    => new HttpStatusException(status(row), "GET", Url, None)
  }

  private def status(row: Row): Int =
    row.status.getOrElse(throw new IllegalArgumentException(s"$row names no status"))

  private def mongoWrite(code: Int, message: String): MongoWriteException =
    new MongoWriteException(new WriteError(code, message, new BsonDocument()),
      new ServerAddress("localhost", 27017), java.util.Collections.emptyList[String]())

  private def thrown(block: => Any): Throwable =
    Try(block).failed.getOrElse(throw new AssertionError("expected the production helper to throw"))
}
