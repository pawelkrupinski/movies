package tools

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** Evaluates a property's INDEPENDENT cases — seeds, corpora — on every core instead of one
 *  after another, answering in the cases' own order. For specs whose cases share nothing
 *  mutable: each builds its own model, lookups and store from its case, so the order they run
 *  in (or running them at once) cannot change what any of them answers.
 *
 *  Every case still runs; nothing is sampled away. A case that throws (a failed assertion)
 *  fails the whole call with that case's exception. */
object IndependentCases {

  def map[A, B](cases: Seq[A])(f: A => B): Seq[B] = {
    given ExecutionContext = ExecutionContext.global
    Await.result(Future.sequence(cases.map(c => Future(f(c)))), Duration.Inf)
  }

  def flatMap[A, B](cases: Seq[A])(f: A => IterableOnce[B]): Seq[B] = map(cases)(c => f(c).iterator.toSeq).flatten

  def foreach[A](cases: Seq[A])(f: A => Unit): Unit = { map(cases)(f); () }
}
