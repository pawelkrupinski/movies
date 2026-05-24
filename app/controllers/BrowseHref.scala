package controllers

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

object BrowseHref {
  private def enc(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)

  def country(name: String): String  = s"/kraj?name=${enc(name)}"
  def director(name: String): String = s"/rezyser?name=${enc(name)}"
  def actor(name: String): String    = s"/aktor?name=${enc(name)}"
}
