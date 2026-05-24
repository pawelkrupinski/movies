package controllers

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

object BrowseHref {
  private def enc(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)

  def country(name: String): String  = s"/filmy?kraj=${enc(name)}"
  def director(name: String): String = s"/filmy?rezyser=${enc(name)}"
  def actor(name: String): String    = s"/filmy?aktor=${enc(name)}"
}
