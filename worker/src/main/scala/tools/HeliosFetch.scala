package tools

import java.net.http.HttpRequest

object HeliosFetch extends RealHttpFetch {
  override protected def decorateBuilder(builder: HttpRequest.Builder, url: String): HttpRequest.Builder =
    if (url.contains("restapi.helios.pl"))
      builder.header("Accept", "application/json, text/plain, */*")
        .header("Origin", "https://bilety.helios.pl")
    else
      builder.header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
}
