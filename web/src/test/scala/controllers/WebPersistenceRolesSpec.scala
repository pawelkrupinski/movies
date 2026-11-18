package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.MovieRepository
import services.tasks.TaskQueue
import tools.contracts.ReadOnlyCollaborators.writerTypedIn

/** The web's /debug and admin collaborators keep the narrow persistence role they were split to —
 *  the worker's side of the same guard is `services.PersistenceRolesSpec`. */
class WebPersistenceRolesSpec extends AnyFlatSpec with Matchers {

  "the /debug stack and corpus listing" should "read the corpus through MovieRepositoryReader, never MovieRepository" in {
    (writerTypedIn(classOf[DebugStack], classOf[MovieRepository]) ++
      writerTypedIn(Class.forName("controllers.CorpusListing$"), classOf[MovieRepository], "read")) shouldBe empty
  }

  "the tasks admin page" should "enqueue and monitor through the roles, never TaskQueue" in {
    (writerTypedIn(classOf[TasksController], classOf[TaskQueue]) ++
      writerTypedIn(classOf[TaskRunController], classOf[TaskQueue])) shouldBe empty
  }
}
