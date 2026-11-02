package pl.kinowo.location

import com.google.android.gms.tasks.CancellationToken
import com.google.android.gms.tasks.TaskCompletionSource
import com.google.android.gms.tasks.Tasks
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.async
import kotlinx.coroutines.cancel
import kotlinx.coroutines.launch
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

    // Order independence #33: a language switch recreated MainActivity while its
    // locate-me request was starting, so the caller was already cancelled. The
    // cancellation handler cancelled the token before the request was built, and
    // getCurrentLocation refuses an already-cancelled token with an
    // IllegalArgumentException that crashed the app instead of answering no fix.
    @Test
    fun aCallerAlreadyCancelledNeverHandsTheRequestACancelledToken() = runTest {
        var thrown: Throwable? = null
        launch {
            cancel()
            thrown = runCatching {
                awaitTaskOrNull<String> { token ->
                    // Play Services' own precondition in getCurrentLocation.
                    require(!token.isCancellationRequested) { "cancellationToken may not be already canceled" }
                    TaskCompletionSource<String?>(token).task
                }
            }.exceptionOrNull()
        }.join()
        assertTrue("expected the caller's cancellation, got $thrown", thrown is CancellationException)
    }
}
