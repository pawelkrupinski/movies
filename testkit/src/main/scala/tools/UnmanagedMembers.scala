package tools

import java.util.concurrent.ExecutorService

/** The lifecycle-bearing members of a composition root — each `services.Stoppable` and executor it holds
 *  directly, or in an `Option`/`Seq` member — that its [[ManagedResources]] does not hold: what its
 *  `stop()` would leave running. Call after building every member (`ObjectGraph.forceLazyMembers`). */
object UnmanagedMembers {
  // The root's own field, optionally one element in (a Seq's `[i]`, an Option's `.value`).
  private val DirectMember = """^[^.\[]+\.([^.\[]+?)(?:\$lzy\d+)?(?:\[\d+\]|\.value)?$""".r

  def of(root: AnyRef, managed: ManagedResources): Seq[String] =
    ObjectGraph.collect(root) {
      case s: services.Stoppable => s: AnyRef
      case e: ExecutorService    => e: AnyRef
    }.collect { case (DirectMember(member), resource) if !managed.holds(resource) => s"$member (${resource.getClass.getSimpleName})" }
      .distinct.sorted
}
