package pl.kinowo.location

import com.google.android.gms.tasks.CancellationToken
import com.google.android.gms.tasks.TaskCompletionSource
import com.google.android.gms.tasks.Tasks
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.async
import kotlinx.coroutines.test.runCurrent
import kotlinx.coroutines.test.runTest
import kotlinx.coroutines.withTimeoutOrNull
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import java.io.IOException

/**
 * The Play Services task bridge behind the location fix. A fresh-fix request
 * used to run without a cancellation token, so a timeout or a cleared
 * ViewModel left the radio working for an answer nobody read, and a task
 * cancelled from outside left its caller suspended forever.
 */
@OptIn(ExperimentalCoroutinesApi::class)
class AwaitTaskOrNullTest {

    @Test
    fun answersTheTasksResult() = runTest {
        val task = TaskCompletionSource<String?>()
        val answer = async { awaitTaskOrNull { task.task } }
        runCurrent()
        task.setResult("fix")
        assertEquals("fix", answer.await())
    }

    @Test
    fun aFailedTaskIsNoFix() = runTest {
        val task = TaskCompletionSource<String?>()
        val answer = async { awaitTaskOrNull { task.task } }
        runCurrent()
        task.setException(IOException("no provider"))
        assertNull(answer.await())
    }

    @Test
    fun aTaskCancelledFromOutsideIsNoFixRatherThanAHang() = runTest {
        assertNull(awaitTaskOrNull<String> { Tasks.forCanceled() })
    }

    @Test
    fun timingOutCancelsTheRequestThroughItsToken() = runTest {
        var token: CancellationToken? = null
        val answer = withTimeoutOrNull(8_000) {
            awaitTaskOrNull<String> { t -> token = t; TaskCompletionSource<String?>(t).task }
        }
        assertNull(answer)
        assertTrue("the location request was left running", token!!.isCancellationRequested)
    }
}
