package controllers

import play.api.libs.json.{JsValue, Json}
import play.twirl.api.Html

/** JSON made safe to write inside an inline `<script>` element — a JS literal or a
 *  `type="application/json"`/`ld+json` data block alike.
 *
 *  A `<script>` element ends at the first `</script` in its text whatever the JSON
 *  around it means, and `Json.stringify` leaves `<` alone, so a scraped cinema name,
 *  synopsis or upstream error string containing `</script>` would close the element
 *  and run whatever follows as markup. Escaping every `<` to `<` rules that out
 *  (and `<!--` with it); the value parses identically. */
object ScriptJson {

  def escape(json: String): String = json.replace("<", "\\u003c")

  def stringify(value: JsValue): String = escape(Json.stringify(value))

  def embed(value: JsValue): Html = Html(stringify(value))

  /** JSON that is already serialized — a generated pack, a hand-built payload — made
   *  safe the same way. Escaping `<` is right for any JSON text: outside a string a `<`
   *  cannot occur, and inside one `\u003c` is the same character. Idempotent, so text
   *  that was escaped already passes through unchanged. */
  def embedSerialized(json: String): Html = Html(escape(json))
}
