package scripts

import clients._
import models._

import java.util.concurrent.{Callable, Executors}
import scala.util.{Failure, Success, Try}

object FilmwebDiff extends App {

  private val pool = Executors.newFixedThreadPool(12)
  private def submit[A](f: => A) = pool.submit(new Callable[A] { def call(): A = f })

  println("Fetching from all sources in parallel...")

  val filmwebFuture = submit(Try(FilmwebClient.fetch()))

  val existingFutures = Seq(
    ("Multikino",             Multikino,             submit(Try(MultikinoClient.fetch()))),
    ("Cinema City Kinepolis", CinemaCityKinepolis,   submit(Try(CinemaCityClient.fetch("1081", CinemaCityKinepolis)))),
    ("Cinema City Plaza",     CinemaCityPoznanPlaza, submit(Try(CinemaCityClient.fetch("1078", CinemaCityPoznanPlaza)))),
    ("Helios",                Helios,                submit(Try(HeliosClient.fetch()))),
    ("Kino Bułgarska",        KinoBulgarska,         submit(Try(KinoBulgarskaClient.fetch()))),
    ("Charlie Monroe",        CharlieMonroe,         submit(Try(CharlieMonroeClient.fetch()))),
    ("Kino Pałacowe",         KinoPalacowe,          submit(Try(KinoPalacoweClient.fetch()))),
    ("Kino Muza",             KinoMuza,              submit(Try(KinoMuzaClient.fetch()))),
    ("Rialto",                Rialto,                submit(Try(RialtoClient.fetch())))
  )

  val filmwebMovies: Seq[CinemaMovie] = filmwebFuture.get() match {
    case Success(ms) => println(s"  Filmweb: ${ms.size} entries"); ms
    case Failure(e)  => println(s"  Filmweb: FAILED - ${e.getMessage}"); Seq.empty
  }

  val existingMovies: Seq[CinemaMovie] = existingFutures.flatMap { case (name, _, future) =>
    future.get() match {
      case Success(ms) => println(s"  $name: ${ms.size} entries"); ms
      case Failure(e)  => println(s"  $name: FAILED - ${e.getMessage}"); Seq.empty
    }
  }

  pool.shutdown()

  // ── Normalization ──────────────────────────────────────────────────────────

  private val ArabicToRoman = Map(
    "1" -> "I", "2" -> "II", "3" -> "III", "4" -> "IV", "5" -> "V",
    "6" -> "VI", "7" -> "VII", "8" -> "VIII", "9" -> "IX", "10" -> "X",
    "11" -> "XI", "12" -> "XII", "13" -> "XIII", "14" -> "XIV", "15" -> "XV",
    "16" -> "XVI", "17" -> "XVII", "18" -> "XVIII", "19" -> "XIX", "20" -> "XX"
  )

  private def normalize(title: String): String =
    title.trim.split("\\s+").map(w => ArabicToRoman.getOrElse(w, w)).mkString(" ").toLowerCase

  // ── Index: (Cinema, normalizedTitle) -> CinemaMovie ───────────────────────

  def index(movies: Seq[CinemaMovie]): Map[(Cinema, String), CinemaMovie] =
    movies.map(cm => (cm.cinema, normalize(cm.movie.title)) -> cm).toMap

  val fwIdx  = index(filmwebMovies)
  val exIdx  = index(existingMovies)

  // ── Per-cinema report ─────────────────────────────────────────────────────

  val TitleW = 42

  Cinema.all.foreach { cinema =>
    val fwKeys = fwIdx.keySet.filter(_._1 == cinema)
    val exKeys = exIdx.keySet.filter(_._1 == cinema)

    if (fwKeys.nonEmpty || exKeys.nonEmpty) {

    val onlyFw  = (fwKeys -- exKeys).toSeq.sortBy(_._2)
    val onlyEx  = (exKeys -- fwKeys).toSeq.sortBy(_._2)
    val both    = (fwKeys & exKeys).toSeq.sortBy(_._2)

    println(s"\n══ ${cinema.displayName} ══")
    println(s"  ${"title".padTo(TitleW, ' ')} shared  fw-only  ex-only")
    println(s"  ${"─" * TitleW} ──────  ───────  ───────")

    both.foreach { k =>
      val fw      = fwIdx(k)
      val ex      = exIdx(k)
      val fwTimes = fw.showtimes.map(_.dateTime).toSet
      val exTimes = ex.showtimes.map(_.dateTime).toSet
      val shared  = (fwTimes & exTimes).size
      val fwOnly  = (fwTimes -- exTimes).size
      val exOnly  = (exTimes -- fwTimes).size
      val title   = ex.movie.title.take(TitleW).padTo(TitleW, ' ')
      println(s"  $title ${shared.toString.padTo(6, ' ')}  ${fwOnly.toString.padTo(7, ' ')}  $exOnly")
    }

    onlyFw.foreach { k =>
      val cm    = fwIdx(k)
      val title = s"[fw only] ${cm.movie.title}".take(TitleW).padTo(TitleW, ' ')
      println(s"  $title ${"─".padTo(6, ' ')}  ${cm.showtimes.size.toString.padTo(7, ' ')}  ─")
    }

    onlyEx.foreach { k =>
      val cm    = exIdx(k)
      val title = s"[ex only] ${cm.movie.title}".take(TitleW).padTo(TitleW, ' ')
      println(s"  $title ${"─".padTo(6, ' ')}  ${"─".padTo(7, ' ')}  ${cm.showtimes.size}")
    }
    }
  }
}
