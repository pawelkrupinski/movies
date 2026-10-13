package controllers

import play.api.i18n.{Lang, Messages, MessagesApi}

/** The language the `showtimes.cc` front door speaks: English, the brand's, whichever
 *  deployment the proxy points the apex at — which is Poland's. The picker renders in it
 *  ([[LandingController]]), and so does every unknown-city 404 the apex answers
 *  ([[ServedCity]], `FrontDoorRedirectFilter`), so `showtimes.cc/xyzzy/` reads "Unknown
 *  city", not Polish. Fixed, not resolved per request: it speaks for the brand. */
object FrontDoorMessages {
  def apply(messagesApi: MessagesApi): Messages = new MemoisedMessages(messagesApi.preferred(Seq(Lang("en"))))
}
