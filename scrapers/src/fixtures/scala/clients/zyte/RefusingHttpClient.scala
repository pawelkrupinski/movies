package clients.zyte

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.util.Optional
import scala.collection.mutable

/** A JDK client that refuses every call, keeping each request it was sent — standing
 *  in for the Zyte API, so a spec can count the calls and inspect how they were built. */
class RefusingHttpClient extends HttpClient {
  val requests: mutable.Buffer[HttpRequest] = mutable.Buffer.empty
  val sends = new java.util.concurrent.atomic.AtomicInteger(0)
  override def send[T](request: HttpRequest, handler: HttpResponse.BodyHandler[T]): HttpResponse[T] = {
    requests.synchronized(requests += request)
    sends.incrementAndGet(); throw new java.io.IOException("refused by the spec's client")
  }
  override def sendAsync[T](request: HttpRequest, handler: HttpResponse.BodyHandler[T]) = ???
  override def sendAsync[T](request: HttpRequest, handler: HttpResponse.BodyHandler[T],
                            push: HttpResponse.PushPromiseHandler[T]) = ???
  override def cookieHandler()   = Optional.empty()
  override def connectTimeout()  = Optional.empty()
  override def followRedirects() = HttpClient.Redirect.NEVER
  override def proxy()           = Optional.empty()
  override def sslContext()      = ???
  override def sslParameters()   = ???
  override def authenticator()   = Optional.empty()
  override def version()         = HttpClient.Version.HTTP_1_1
  override def executor()        = Optional.empty()
}
