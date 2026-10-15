package services.movies

/** The BBFC's certificates, as UK box offices and feeds bill them: one list every reader takes them from — the title
 *  rules' `CERT` placeholder (which the ingest's certificate peel, [[FormatTags]], reads too) and the UK clients reading
 *  a feed's rating. */
object BbfcCertificate {
  /** The certificates of a public cinema screening, "12A" before "12" so an alternation of them reads the longer. */
  val Public: Seq[String] = Seq("U", "PG", "12A", "12", "15", "18")
  /** R18: licensed premises only — a badge a programme may still show. */
  val Restricted: String = "R18"
  /** What a box office bills while one is awaited. */
  val Pending: String = "TBC"

  /** `values` as one regex alternation, no group of its own. */
  def alternation(values: Seq[String]): String = values.map(java.util.regex.Pattern.quote).mkString("(?:", "|", ")")
}
