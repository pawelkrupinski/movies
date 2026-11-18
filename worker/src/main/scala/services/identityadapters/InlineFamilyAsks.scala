package services.identityadapters

import services.identity.Question

/** The agreement's [[services.identity.agreement.AgreementStage.AskNow]]: a new cluster's family questions, each asked
 *  through `asker` — as the queue asks it — on `pool` at once, waited on for at most `within`, each timed to `metrics`
 *  ([[AgreementQuestionMetrics.inlineAsk]]: its wait for the pool, then the ask). One still out by then is
 *  filed when it comes and asks for a projection, as on the queue; the queue holds it too, and skips it once it is
 *  filed. A failure is remembered by the asker, as on the queue, and asked again once its wait ends; one still inside
 *  its wait is not asked. */
final class InlineFamilyAsks(asker: AgreementAsker, pool: java.util.concurrent.ExecutorService, stopwatch: tools.Stopwatch,
                             metrics: AgreementQuestionMetrics = AgreementQuestionMetrics.Silent)
    extends services.identity.agreement.AgreementStage.AskNow {
  def apply(questions: Set[Question], within: scala.concurrent.duration.FiniteDuration): Unit = {
    val started = stopwatch.start()
    val late    = new java.util.concurrent.atomic.AtomicBoolean(false)
    val asked = questions.toSeq.collect { case question: Question.Family =>
      val submitted = stopwatch.start()
      pool.submit(new Runnable {
        def run(): Unit = {
          val queued = submitted.seconds
          try { tools.HttpPath.within(tools.HttpPath.Agreement)(asker.ask(Seq(question), announce = late.get)); () }
          finally metrics.inlineAsk(question.family.label, queued, submitted.seconds - queued)
        }
      })
    }
    asked.foreach { future =>
      try future.get((within - started.elapsed).toNanos.max(0L), java.util.concurrent.TimeUnit.NANOSECONDS)
      catch { case _: java.util.concurrent.TimeoutException | _: java.util.concurrent.ExecutionException => () }
    }
    late.set(true)
  }
}
