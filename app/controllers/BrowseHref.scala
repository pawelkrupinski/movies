package controllers

import models.City

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/** Per-axis browse links (`/{city}/filmy?kraj=…` etc.). City comes in
 *  implicitly so call sites in city-scoped templates read unchanged. */
object BrowseHref {
  private def enc(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)

  def country(name: String)(implicit city: City): String  = s"/${city.slug}/filmy?kraj=${enc(name)}"
  def director(name: String)(implicit city: City): String = s"/${city.slug}/filmy?rezyser=${enc(name)}"
  def actor(name: String)(implicit city: City): String    = s"/${city.slug}/filmy?aktor=${enc(name)}"
  def genre(name: String)(implicit city: City): String    = s"/${city.slug}/filmy?gatunek=${enc(name)}"
}
