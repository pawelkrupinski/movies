package services.identity.vocabulary

import java.awt.image.BufferedImage

/**
 * A poster's perceptual hash (pHash, 63 bits): the signs of the lowest 8×8 DCT frequencies of its 32×32 grey image,
 * DC term dropped, against their median. Two prints of one poster — another size, another JPEG quality, a venue's
 * crop — differ in a few bits; two different posters in ~32. Measured on prod's venue posters against up to 8 of each
 * candidate's TMDB posters (2026-10-05, 1,852 right and 1,992 wrong pairs): a right film's nearest poster sits a median
 * 6 bits away, a wrong one's 28, and 5 of 1,697 wrong pairs came within 8.
 */
final case class PosterHash(bits: Long) {
  def distance(other: PosterHash): Int = java.lang.Long.bitCount(bits ^ other.bits)
}

object PosterHash {
  private val Side = 32
  private val Low  = 8

  /** The DCT-II basis, orthonormal: `C(k)(i)`. */
  private val Basis: Array[Array[Double]] = Array.tabulate(Side, Side) { (k, i) =>
    math.cos(math.Pi * k * (2 * i + 1) / (2.0 * Side)) * (if (k == 0) math.sqrt(1.0 / Side) else math.sqrt(2.0 / Side))
  }

  /** `image`'s hash: grey by luma, a landscape image centre-cut to a 2:3 portrait first (a venue's banner of the
   *  poster), area-averaged down to 32×32. */
  def of(image: BufferedImage): PosterHash = {
    val (w, h) = (image.getWidth, image.getHeight)
    val (x0, cut) = if (w > h) { val portrait = math.max(1, h * 2 / 3); ((w - portrait) / 2, portrait) } else (0, w)
    val grey = Array.ofDim[Double](h, cut)
    val row  = new Array[Int](cut)
    for (y <- 0 until h) {
      image.getRGB(x0, y, cut, 1, row, 0, cut)
      var x = 0
      while (x < cut) {
        val p = row(x)
        grey(y)(x) = 0.299 * ((p >> 16) & 0xff) + 0.587 * ((p >> 8) & 0xff) + 0.114 * (p & 0xff)
        x += 1
      }
    }
    ofGrey(areaAverage(grey, cut, h))
  }

  /** `grey` (`h` rows of `w`) area-averaged to [[Side]]×[[Side]]: each target cell the mean of the source area it
   *  covers, fractional edges weighed by their share. */
  private def areaAverage(grey: Array[Array[Double]], w: Int, h: Int): Array[Array[Double]] = {
    def spans(n: Int): Array[Seq[(Int, Double)]] = Array.tabulate(Side) { t =>
      val (from, to) = (t.toDouble * n / Side, (t + 1).toDouble * n / Side)
      (from.toInt until math.min(n, math.ceil(to).toInt)).map(i => i -> (math.min(to, i + 1.0) - math.max(from, i.toDouble))).filter(_._2 > 0)
    }
    val (xs, ys) = (spans(w), spans(h))
    Array.tabulate(Side, Side) { (ty, tx) =>
      var sum = 0.0; var weight = 0.0
      for ((y, wy) <- ys(ty); (x, wx) <- xs(tx)) { sum += grey(y)(x) * wy * wx; weight += wy * wx }
      sum / weight
    }
  }

  /** The hash of a 32×32 grey image. */
  private[identity] def ofGrey(a: Array[Array[Double]]): PosterHash = {
    // C · A · Cᵀ, only its first 8 rows and columns
    val rows = Array.tabulate(Low, Side)((k, j) => (0 until Side).map(i => Basis(k)(i) * a(i)(j)).sum)
    val low  = Array.tabulate(Low, Low)((k, l) => (0 until Side).map(j => rows(k)(j) * Basis(l)(j)).sum).flatten.drop(1)
    val median = low.sorted.apply(low.length / 2)
    PosterHash(low.zipWithIndex.foldLeft(0L) { case (bits, (v, i)) => if (v > median) bits | (1L << i) else bits })
  }
}

/** What the posters are, as filed: `Unknown` while one is not hashed yet — a gap the poster fill asks, never "no poster".
 *  A venue poster that could not be read is `Known(None)` and [[unread]]: a failed read, no evidence, asked again; a
 *  film TMDB keeps no poster of is `Known(Nil)`. */
trait PosterAnswers {
  /** The hash of the image at a venue's poster `url`. */
  def venue(url: String): Answer[Option[PosterHash]]
  /** The hashes of TMDB's posters of `tmdbId` ([[PosterEvidence.FilmPosters]] of them at most). */
  def film(tmdbId: Int): Answer[Seq[PosterHash]]
  /** Was `question`'s poster GIVEN UP on — filed as none because its fetch kept failing, not because there is none? It
   *  is no evidence either way: what it might have vetoed is read without it. */
  def unread(question: PosterQuestion): Boolean = false
  /** Is `question`'s filed answer still fresh? One that is not — a hash a year old, a poster unread a week — is read
   *  meanwhile and asked again, as a family's stale answer is. */
  def fresh(question: PosterQuestion): Boolean = true
}

object PosterAnswers {
  /** No poster anywhere: no evidence, and no gap. */
  val Silent: PosterAnswers = new PosterAnswers {
    def venue(url: String): Answer[Option[PosterHash]] = Answer.Known(None)
    def film(tmdbId: Int): Answer[Seq[PosterHash]]     = Answer.Known(Nil)
  }

  /** TMDB's CDN at the small size a film's posters are hashed from (a hash is of a 32×32 grey image) — and shown at by the
   *  review pages, from the paths filed beside the hashes. */
  val FilmPosterBase = "https://image.tmdb.org/t/p/w185"

  /** The field a film's poster document keeps its TMDB poster paths in (`/abc.jpg`), each beside the hash read from it:
   *  for the review pages, which show a candidate's poster, and for hashing a film again, which reads only the paths not
   *  hashed yet. The agreement never reads them. Written as ONE string ([[pathsValue]]): an array of strings cost each
   *  path ~9 bytes of BSON framing, more than a quarter of a path, in the answer cache every TMDB answer shares. */
  val Paths = "paths"

  /** `paths` as filed: one string, the paths in order, space-separated, a `/<name>.jpg` written as its `<name>` alone. */
  def pathsValue(paths: Seq[String]): org.bson.BsonString =
    new org.bson.BsonString(paths.map(path => JpgPath.unapplySeq(path).flatMap(_.headOption).getOrElse(path)).mkString(" "))
  private val JpgPath = """/([^/ ]+)\.jpg""".r

  /** The TMDB poster paths filed in a poster document, in its hashes' order; none for a venue's, or one filed before
   *  them. Read as filed now ([[pathsValue]]), or as an array, as they were filed before 2026-11-11. */
  def pathsOf(d: org.bson.BsonDocument): Seq[String] = d.get(Paths) match {
    case s: org.bson.BsonString => s.getValue.split(' ').toSeq.filter(_.nonEmpty).map(token => if (token.startsWith("/")) token else s"/$token.jpg")
    case _                      => BsonFields.elementsOf(d, Paths).collect { case v if v.isString => v.asString.getValue }
  }

  /** The id a poster's hashes are filed under among the families' answers: `poster|venue|<url>`, `poster|film|<tmdbId>`. */
  def idOf(question: PosterQuestion): String = question match {
    case PosterQuestion.Venue(url)   => s"poster|venue|$url"
    case PosterQuestion.Film(tmdbId) => s"poster|film|$tmdbId"
  }
}
