package controllers

import java.nio.file.Path

import scala.util.{Failure, Success, Try}

import services.review.{LabelRow, LabelsTsv}

/** The labels file the review pages show beside each card (`identity-judgements.tsv`): one that cannot be read is said
 *  so on the page, never shown as "no labels". */
final class ReviewLabels(val path: Path) {
  /** The file's rows, or none and what went wrong. */
  def read(): (Seq[LabelRow], Option[String]) = ReviewLabels.readReported(path, Seq.empty[LabelRow])(LabelsTsv.read(path))
}

object ReviewLabels {
  /** `read` of the file at `path`, or `fallback` and what went wrong, for the page to say: never shown as an empty file. */
  def readReported[A](path: Path, fallback: A)(read: => A): (A, Option[String]) = Try(read) match {
    case Success(value) => (value, None)
    case Failure(e)     => (fallback, Some(s"could not read $path: ${e.getMessage}"))
  }
}
