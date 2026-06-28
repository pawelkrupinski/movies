package scripts

import play.api.libs.json._
import services.enrichment.FilmwebClient
import services.movies.{MongoMovieRepository, StoredMovieRecord}
import tools.{RealHttpFetch, TextNormalization}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

/**
 * THROWAWAY investigation harness (re-derived 2026-06-28 from the original
 * 2026-06-25 run). Resolve-by-synopsis, gated on an INDEPENDENT corroborator —
 * never accept on plot text alone:
 *
 *   HARD REJECT  director-contradiction, or year off by >1 with no exact title
 *   ACCEPT       exact-title OR director-overlap                        (corroborated)
 *                OR (synopsis≥τ AND (character-name-overlap OR cast≥2))  (domain-signal)
 *   ABSTAIN      otherwise (a missing link beats a wrong one)
 *
 * Unlike the original, this reads the LIVE prod corpus directly via
 * MongoMovieRepository (real `synopsisCinema` / `director` accessors) and loops
 * all three modes in one run. The IMDb mode now selects rows whose `imdbId`
 * is MISSING (not whose rating is missing) — the earlier rating-gate swept in
 * unreleased films that already had an id (e.g. Evil Dead Burn 2026).
 *
 * Writes the full per-card field set the report page reads to accepts.jsonl.
 *
 *   TMDB_API_KEY=... MONGODB_URI=... sbt 'worker/Test/runMain scripts.GateReport [out.jsonl] [capPerMode]'
 */
object GateReport {

  private val ApiKey = sys.env.getOrElse("TMDB_API_KEY", "")
  private val DefaultOut = "/Users/pawel/projects/movies-synopsis-refresh/docs/synopsis-resolution/accepts.jsonl"
  private val Tau = 0.06 // stem-IDF synopsis floor (scores run low; corroborators carry precision)
  private val http = new RealHttpFetch
  private val filmweb = new FilmwebClient(http)
  private val Modes = Seq("tmdb-pl", "filmweb-pl", "imdb-eng")

  private case class Meta(title: String, origTitle: Option[String], year: Option[Int], overview: String,
                          directors: Set[String], cast: Set[String], characters: Set[String])
  private case class Cand(key: String, url: String, title: String, year: Option[Int], text: String,
                          directors: Set[String], cast: Set[String], characters: Set[String] = Set.empty)
  private case class Film(id: String, title: String, search: String, year: Option[Int], tmdbId: Option[Int],
                          queries: Seq[String], cinemaSyn: String, dirs: Set[String])

  def main(args: Array[String]): Unit = {
    val out = args.headOption.filter(_.nonEmpty).getOrElse(DefaultOut)
    val cap = args.lift(1).map(_.toInt)

    val repo = new MongoMovieRepository()
    if (!repo.enabled) { println("MONGODB_URI not set — nothing to read."); sys.exit(1) }
    // Paginated read (200/batch) — far more robust over the flyctl proxy than a
    // single 736-doc `findAll`, which intermittently corrupts the BSON stream
    // (StackOverflow on decode) / blows the 60s cap.
    val buf = scala.collection.mutable.ArrayBuffer.empty[StoredMovieRecord]
    repo.foreachRecord(buf += _)
    val corpus = buf.toSeq
    println(s"\ncorpus: ${corpus.size} rows")

    Files.write(Paths.get(out), Array.emptyByteArray) // truncate once
    Modes.foreach(m => runMode(m, corpus, out, cap))
    repo.close()
    println(s"\n→ wrote $out")
  }

  private def srcFor(mode: String) =
    mode match { case "tmdb-pl" => "TMDB"; case "filmweb-pl" => "Filmweb"; case "imdb-eng" => "IMDb"; case _ => "?" }

  private def buildFilms(mode: String, corpus: Seq[StoredMovieRecord]): Seq[Film] = corpus.flatMap { r =>
    val rec = r.record
    val keep = mode match {
      case "tmdb-pl"    => rec.tmdbId.isEmpty && rec.synopsisCinema.exists(_.trim.nonEmpty)
      case "filmweb-pl" => rec.tmdbId.nonEmpty && rec.filmwebUrl.forall(_.isEmpty)
      case "imdb-eng"   => rec.imdbId.forall(_.isEmpty) && rec.tmdbId.nonEmpty
      case _            => false
    }
    if (!keep) None
    else {
      val search = rec.searchTitle.map(_.trim).filter(_.nonEmpty).getOrElse(r.title)
      Some(Film(
        id = r.persistedId.getOrElse(s"${r.title}|${r.year.map(_.toString).getOrElse("")}"),
        title = r.title, search = search, year = r.year, tmdbId = rec.tmdbId,
        queries = (Seq(search, r.title) ++ stripVariants(r.title)).map(_.trim).filter(_.nonEmpty).distinct,
        cinemaSyn = rec.synopsisCinema.getOrElse(""), dirs = rec.director.toSet
      ))
    }
  }

  private def runMode(mode: String, corpus: Seq[StoredMovieRecord], out: String, cap: Option[Int]): Unit = {
    val src = srcFor(mode)
    var films = buildFilms(mode, corpus)
    cap.foreach(c => films = films.take(c))
    println(s"\n=== GATE mode=$mode src=$src films=${films.size} ===")
    if (films.isEmpty) return

    val rows = runPool(if (mode == "filmweb-pl") 3 else 6, films) { f => (f, fetchRowAndCands(mode, f)) }
      .collect { case (f, Some((ref, cands))) if cands.nonEmpty => (f, ref, cands) }

    val lang = if (mode.endsWith("-eng")) "en" else "pl"
    val idf = buildIdf(rows.flatMap { case (_, ref, cs) => ref +: cs.map(_.text) }.filter(_.nonEmpty), lang)

    var accepted, corrob, domainOnly, abstained = 0
    val writer = new StringBuilder
    rows.foreach { case (f, ref, cands) =>
      val rowMeta = if (mode == "tmdb-pl") None else f.tmdbId.flatMap(tmdbMeta)
      val knownDirs = if (mode == "tmdb-pl") f.dirs else rowMeta.map(_.directors).filter(_.nonEmpty).getOrElse(f.dirs)
      val knownCast = rowMeta.map(_.cast).getOrElse(Set.empty)
      val knownChars = rowMeta.map(_.characters).getOrElse(Set.empty)
      val knownYear = if (mode == "tmdb-pl") f.year else rowMeta.flatMap(_.year).orElse(f.year)
      val qset = f.queries.map(norm).toSet

      val evals = cands.map { c =>
        val sim = cosine(vec(ref, lang, idf), vec(c.text, lang, idf))
        val chars = if (mode == "tmdb-pl") c.characters else knownChars
        val checkText = if (mode == "tmdb-pl") ref else c.text
        val matchedChars = chars.filter(ch => containsName(checkText, ch)).toSeq
        val matchedCast = matchedNames(knownCast, c.cast)
        val matchedDirs = matchedNames(knownDirs, c.directors)
        val exact = qset.exists(q => q == norm(c.title) || (c.title.nonEmpty && norm(c.title).startsWith(q) && q.length >= 4))
        val yearContradict = (for { ry <- knownYear; cy <- c.year } yield math.abs(ry - cy) > 1).getOrElse(false)
        val dirContradict = knownDirs.nonEmpty && c.directors.nonEmpty && matchedDirs.isEmpty
        (c, sim, matchedChars, matchedCast, matchedDirs, exact, yearContradict, dirContradict)
      }
      val eligible = evals.filter { case (_, sim, mChars, mCast, mDirs, exact, yc, dc) =>
        !dc && !(yc && !exact) && (exact || mDirs.nonEmpty || (sim >= Tau && (mChars.nonEmpty || mCast.size >= 2)))
      }
      eligible.sortBy { case (_, sim, _, _, mDirs, exact, _, _) => (!(exact || mDirs.nonEmpty), -sim) }.headOption match {
        case None => abstained += 1
        case Some((c, sim, mChars, mCast, mDirs, exact, _, _)) =>
          accepted += 1
          val corroborated = exact || mDirs.nonEmpty
          if (corroborated) corrob += 1 else domainOnly += 1
          val via = if (mDirs.nonEmpty) "director" else if (exact) "exact-title" else if (mChars.nonEmpty) "character-name" else "cast"
          val yearMatch = (for { ry <- knownYear; cy <- c.year } yield ry == cy).getOrElse(false)
          writer.append(Json.stringify(Json.obj(
            "src" -> src, "filmId" -> f.id, "filmTitle" -> f.title, "filmYear" -> f.year.map(_.toString).getOrElse(""),
            "searchTitle" -> f.search, "extUrl" -> c.url, "extTitle" -> c.title, "extYear" -> c.year.map(_.toString).getOrElse(""),
            "via" -> via, "corroborated" -> corroborated, "sim" -> f"$sim%.3f",
            "refSyn" -> ref, "candSyn" -> c.text,
            "rowDirectors" -> knownDirs.toSeq.sorted, "candDirectors" -> c.directors.toSeq.sorted,
            "matchedDirectors" -> mDirs.sorted, "matchedCharacters" -> mChars.sorted, "matchedCast" -> mCast.sorted,
            "yearMatch" -> yearMatch
          ))).append("\n")
      }
    }
    Files.write(Paths.get(out), writer.toString.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    println(f"  candidates fetched for ${rows.size}/${films.size}")
    println(f"  ACCEPTED $accepted  (corroborated: $corrob, domain-signal only: $domainOnly)   ABSTAINED $abstained")
  }

  // ── fetch row ref + candidates ────────────────────────────────────────────
  private def fetchRowAndCands(mode: String, f: Film): Option[(String, Seq[Cand])] = mode match {
    case "tmdb-pl" =>
      val cands = f.queries.flatMap(q => tmdbSearch(q, f.year) ++ tmdbSearch(q, None)).groupBy(_.key).map(_._2.head).toSeq
        .take(8).map { c =>
          tmdbMeta(c.key.toInt) match {
            case Some(m) => c.copy(directors = m.directors, cast = m.cast, characters = m.characters)
            case None    => c
          }
        }
      Some((f.cinemaSyn, cands))
    case "filmweb-pl" =>
      val ref = f.tmdbId.flatMap(tmdbMeta).map(_.overview).getOrElse("")
      val cands = f.queries.take(2).flatMap { q =>
        Try(filmweb.search(q)).getOrElse(Seq.empty).take(6).flatMap { hit =>
          for { i <- Try(filmweb.info(hit.id)).toOption.flatten } yield {
            val p = Try(filmweb.preview(hit.id)).toOption.flatten
            Cand(s"fw${hit.id}", FilmwebClient.canonicalUrl(hit.id, hit.kind, i.title, i.year),
              i.title, i.year, p.flatMap(_.plot).getOrElse(""), p.map(_.directors).getOrElse(Set.empty), Set.empty)
          }
        }
      }.groupBy(_.key).map(_._2.head).toSeq.filter(_.text.nonEmpty)
      Some((ref, cands))
    case "imdb-eng" =>
      val ref = f.tmdbId.map(tmdbEnOverview).getOrElse("")
      val enTitle = f.tmdbId.flatMap(tmdbMeta).map(_.title)
      val qs = (enTitle.toSeq ++ f.queries).distinct.take(2)
      val cands = qs.flatMap(imdbSuggest).groupBy(_._1).map(_._2.head).toSeq.take(8).flatMap { case (tt, st, sy) =>
        imdbGraphql(tt).map { case (plot, t, y, dirs, cast) =>
          Cand(tt, s"https://www.imdb.com/title/$tt/", if (t.nonEmpty) t else st, y.orElse(sy), plot, dirs, cast)
        }
      }.filter(_.text.nonEmpty)
      Some((ref, cands))
    case _ => None
  }

  // ── TMDB metadata bundle (credits: director, cast, character names) ─────────
  private def tmdbMeta(tmdbId: Int): Option[Meta] = {
    val url = s"https://api.themoviedb.org/3/movie/$tmdbId?language=en-US&append_to_response=credits&api_key=$ApiKey"
    Try {
      val js = Json.parse(http.get(url, Map.empty[String, String]))
      val cast = (js \ "credits" \ "cast").asOpt[JsArray].map(_.value).getOrElse(Nil)
      Meta(
        title = (js \ "title").asOpt[String].getOrElse(""),
        origTitle = (js \ "original_title").asOpt[String],
        year = (js \ "release_date").asOpt[String].filter(_.length >= 4).flatMap(s => Try(s.take(4).toInt).toOption),
        overview = (js \ "overview").asOpt[String].getOrElse(""),
        directors = (js \ "credits" \ "crew").asOpt[JsArray].map(_.value).getOrElse(Nil)
          .filter(c => (c \ "job").asOpt[String].contains("Director")).flatMap(c => (c \ "name").asOpt[String]).toSet,
        cast = cast.flatMap(c => (c \ "name").asOpt[String]).take(10).toSet,
        characters = cast.flatMap(c => (c \ "character").asOpt[String]).take(10).toSet
      )
    }.toOption
  }
  private def tmdbEnOverview(tmdbId: Int): String = tmdbMeta(tmdbId).map(_.overview).getOrElse("")

  private def tmdbSearch(q: String, year: Option[Int]): Seq[Cand] = {
    val yp = year.map(y => s"&year=$y&primary_release_year=$y").getOrElse("")
    val url = s"https://api.themoviedb.org/3/search/movie?language=pl-PL&include_adult=false&query=${enc(q)}$yp&api_key=$ApiKey"
    Try {
      (Json.parse(http.get(url, Map.empty[String, String])) \ "results").asOpt[JsArray].map(_.value).getOrElse(Nil).flatMap { js =>
        (js \ "id").asOpt[Int].map { id =>
          Cand(id.toString, s"https://www.themoviedb.org/movie/$id", (js \ "title").asOpt[String].getOrElse(""),
            (js \ "release_date").asOpt[String].filter(_.length >= 4).flatMap(s => Try(s.take(4).toInt).toOption),
            (js \ "overview").asOpt[String].getOrElse(""), Set.empty, Set.empty)
        }
      }.toSeq
    }.getOrElse(Seq.empty)
  }

  private def imdbSuggest(title: String): Seq[(String, String, Option[Int])] = {
    if (title.trim.isEmpty) return Seq.empty
    val prefix = title.trim.headOption.filter(c => c.isLetter && c.toInt < 128).map(_.toLower).getOrElse('x')
    Try {
      (Json.parse(http.get(s"https://v3.sg.media-imdb.com/suggestion/$prefix/${enc(title)}.json", Map.empty[String, String])) \ "d")
        .asOpt[JsArray].map(_.value).getOrElse(Nil).flatMap { js =>
        val id = (js \ "id").asOpt[String].filter(_.startsWith("tt"))
        val qid = (js \ "qid").asOpt[String].getOrElse("")
        if (id.isDefined && (qid.contains("movie") || qid.isEmpty)) Some((id.get, (js \ "l").asOpt[String].getOrElse(""), (js \ "y").asOpt[Int])) else None
      }.toSeq
    }.getOrElse(Seq.empty)
  }

  private def imdbGraphql(tt: String): Option[(String, String, Option[Int], Set[String], Set[String])] = {
    val query = "query T($id:ID!){title(id:$id){titleText{text} releaseYear{year} plot{plotText{plainText}} principalCredits{category{id} credits{name{nameText{text}}}}}}"
    val body = Json.stringify(Json.obj("query" -> query, "variables" -> Json.obj("id" -> tt)))
    Try {
      val t = Json.parse(http.post("https://caching.graphql.imdb.com/", body, "application/json")) \ "data" \ "title"
      val pcs = (t \ "principalCredits").asOpt[JsArray].map(_.value).getOrElse(Nil)
      def names(cat: String) = pcs.filter(c => (c \ "category" \ "id").asOpt[String].contains(cat))
        .flatMap(c => (c \ "credits").asOpt[JsArray].map(_.value).getOrElse(Nil).flatMap(cr => (cr \ "name" \ "nameText" \ "text").asOpt[String])).toSet
      ((t \ "plot" \ "plotText" \ "plainText").asOpt[String].getOrElse(""), (t \ "titleText" \ "text").asOpt[String].getOrElse(""),
        (t \ "releaseYear" \ "year").asOpt[Int], names("director"), names("cast"))
    }.toOption
  }

  // ── signals + text ─────────────────────────────────────────────────────────
  private def containsName(text: String, name: String): Boolean = {
    val n = norm(name); val parts = n.split(" ").filter(_.length >= 4)
    parts.nonEmpty && parts.forall(p => norm(text).contains(p))
  }
  private def matchedNames(a: Set[String], b: Set[String]): Seq[String] = {
    val nb = b.map(norm)
    a.toSeq.filter { x => val nx = norm(x); nb.exists(y => nx == y || (nx.length > 4 && y.contains(nx)) || (y.length > 4 && nx.contains(y))) }
  }

  private val StopPL = Set("i","w","na","z","ze","do","że","się","to","jest","są","nie","o","a","po","od","za","jako","który","która","które","przez","ich","jego","jej","ale","dla","gdy","czy","też","już","być","co","jak","ten","oraz","pod","nad","bez","przy")
  private val StopEN = Set("the","a","an","of","to","and","in","on","for","with","his","her","their","its","is","are","as","by","at","from","that","this","who","when","after","into","but","or","he","she","they","it","was","has","have","not","which")
  private def stopwords(lang: String) = if (lang == "pl") StopPL else StopEN
  private val SufPL = Seq("iejszego","owania","ości","ami","ach","owi","ego","emu","iej","ych","ymi","owie","owa","owe","owy","nie","cie","ką","ek","ę","ą","y","i","a","e","u","o")
  private val SufEN = Seq("ization","ational","ation","ement","ments","able","ible","ing","ies","ied","ment","ness","tion","sion","ous","ive","ed","ly","es","s")
  private def stem(t: String, lang: String) = {
    val sufs = if (lang == "pl") SufPL else SufEN
    sufs.find(s => t.length - s.length >= 3 && t.endsWith(s)).map(s => t.dropRight(s.length)).getOrElse(t)
  }
  private def tokens(s: String, lang: String) =
    TextNormalization.deburr(TextNormalization.stripUrls(s)).toLowerCase.split("[^\\p{L}]+").iterator
      .filter(_.length > 2).filterNot(stopwords(lang)).map(stem(_, lang)).toSeq
  private def buildIdf(docs: Seq[String], lang: String): Map[String, Double] = {
    val n = math.max(1, docs.size); val df = scala.collection.mutable.HashMap.empty[String, Int]
    docs.foreach(d => tokens(d, lang).distinct.foreach(t => df.update(t, df.getOrElse(t, 0) + 1)))
    df.iterator.map { case (t, c) => t -> math.log(n.toDouble / c) }.toMap
  }
  private def vec(s: String, lang: String, idf: Map[String, Double]): Map[String, Double] = {
    val maxIdf = if (idf.isEmpty) 1.0 else idf.valuesIterator.max
    tokens(s, lang).groupMapReduce(identity)(_ => 1)(_ + _).map { case (t, c) => t -> c.toDouble * idf.getOrElse(t, maxIdf) }
  }
  private def cosine(a: Map[String, Double], b: Map[String, Double]): Double = {
    if (a.isEmpty || b.isEmpty) return 0.0
    val dot = a.iterator.collect { case (k, v) if b.contains(k) => v * b(k) }.sum
    val na = math.sqrt(a.valuesIterator.map(v => v * v).sum); val nb = math.sqrt(b.valuesIterator.map(v => v * v).sum)
    if (na == 0 || nb == 0) 0.0 else dot / (na * nb)
  }

  private def norm(s: String) = TextNormalization.deburr(s).toLowerCase.trim.replaceAll("\\s+", " ")
  private def enc(s: String) = URLEncoder.encode(s, StandardCharsets.UTF_8)
  private def stripVariants(t: String): Seq[String] = {
    val noParen = t.replaceAll("\\s*\\([^)]*\\)", "").trim
    val pipe = t.split("[|]").map(_.trim).filter(_.nonEmpty).toSeq
    val colon = if (t.contains(":")) Seq(t.substring(t.indexOf(":") + 1).trim) else Seq.empty
    (Seq(noParen) ++ pipe ++ colon).filter(_.nonEmpty)
  }
  private def runPool[A, B](workers: Int, items: Seq[A])(f: A => B): Seq[B] = {
    val pool = Executors.newFixedThreadPool(workers); implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    val done = new AtomicInteger(0)
    val out = Await.result(Future.sequence(items.map(it => Future { val r = f(it); val n = done.incrementAndGet(); if (n % 25 == 0) println(s"  ...$n/${items.size}"); r })), 40.minutes)
    pool.shutdown(); pool.awaitTermination(1, TimeUnit.MINUTES); out
  }
}
