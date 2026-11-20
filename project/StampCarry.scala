package kinowo.build

/**
 * THE STAMP CARRY (the owner, 2026-11-20): an identity version stamp (`identity-rules`, `venue-slot`, `agreement-rules`;
 * `build.sbt`) is the digest of the code it versions (`SourceDigest`), unless that digest was PROVEN to decide what an
 * earlier digest decides -- then it carries the earlier one's stamp, so a deploy of a pure move or a proven-equivalent
 * refactor keeps every worker's stored families, verdicts and slots instead of re-deciding them all on boot.
 *
 * The proofs are rows of `test/resources/identity-stamp-carry.tsv`, one a (stamp, digest):
 *
 *   stamp  digest  equals  proof  base  head
 *
 * `digest` is the raw digest of `head`'s code, `equals` the raw digest of `base`'s, and `proof` how `base..head` was proven
 * to change no decision: `pure-move` (scripts/pure-move-check) or `what-if` (a recorded scripts/identity-what-if verdict,
 * every country, every family, no take moved). A stamp is followed row by row (`version`): a digest carried to one that is
 * itself carried takes ITS stamp, so a chain holds only while every step in it was proven. scripts/identity-stamp-carry
 * writes the rows; the pre-push hook's `stampcarry` check proves every row a push adds or changes again, and refuses any
 * it cannot. A digest with no row is its own stamp: an unproven change moves it, as before.
 *
 * Compiled twice, as `SourceDigest` is: into the build (Scala 2.12) and into identity's tests (Scala 3).
 */
object StampCarry {

  /** The stamps the build writes; a row names one. */
  val Stamps: Seq[String] = Seq("identity-rules", "venue-slot", "agreement-rules")

  val Proofs: Seq[String] = Seq("pure-move", "what-if")

  /** Where the rows live, from the repository's root. */
  val Table = "test/resources/identity-stamp-carry.tsv"

  final case class Row(stamp: String, digest: String, equals: String, proof: String, base: String, head: String)

  private val Hex64 = "[0-9a-f]{64}"
  private val Sha   = "[0-9a-f]{40}"

  /** The table's rows; `#` lines and blank lines left out. Anything malformed fails the build, naming the line: a row the
   *  build could not read would silently stop carrying (or, worse, carry what it should not). */
  def parse(lines: Seq[String]): Seq[Row] = {
    val rows = lines.zipWithIndex.filterNot { case (line, _) => line.trim.isEmpty || line.startsWith("#") }.map { case (line, i) =>
      val at = s"$Table:${i + 1}"
      line.split("\t", -1).toSeq match {
        case Seq(stamp, digest, equals, proof, base, head) =>
          require(Stamps.contains(stamp), s"$at: no stamp $stamp (one of ${Stamps.mkString(", ")})")
          require(digest.matches(Hex64) && equals.matches(Hex64), s"$at: digest and equals are sha-256 hex digests")
          require(digest != equals, s"$at: a digest carried to itself")
          require(Proofs.contains(proof), s"$at: no proof $proof (one of ${Proofs.mkString(", ")})")
          require(base.matches(Sha) && head.matches(Sha), s"$at: base and head are full commit shas")
          Row(stamp, digest, equals, proof, base, head)
        case fields => throw new IllegalArgumentException(s"$at: ${fields.size} fields, not 6 (stamp digest equals proof base head)")
      }
    }
    val twice = rows.groupBy(row => (row.stamp, row.digest)).collect { case (key, more) if more.size > 1 => s"${key._1} ${key._2}" }
    require(twice.isEmpty, s"$Table: one digest carried twice: ${twice.mkString("; ")}")
    rows
  }

  /** The stamp `digest` carries: the digest of the row its chain ends at, or `digest` itself when no row carries it. */
  def version(stamp: String, digest: String, rows: Seq[Row]): String = {
    val carries = rows.filter(_.stamp == stamp).map(row => row.digest -> row.equals).toMap
    @annotation.tailrec
    def follow(at: String, seen: Set[String]): String = carries.get(at) match {
      case None       => at
      case Some(next) =>
        require(!seen(next), s"$Table: $stamp's carry rows loop through $next")
        follow(next, seen + next)
    }
    follow(digest, Set(digest))
  }
}
