package pl.kinowo

import kotlin.coroutines.cancellation.CancellationException

/**
 * [runCatching] for coroutine code: captures a failure but RETHROWS
 * [CancellationException]. Plain `runCatching` / `catch (e: Exception)` also
 * swallows cancellation, so a cancelled coroutine (a city switch's
 * `collectLatest`, a cleared ViewModel) carries on as if its work had merely
 * failed — reporting an error, or returning normally to a caller that then
 * keeps going.
 */
inline fun <T> runCatchingCancellable(block: () -> T): Result<T> =
    try {
        Result.success(block())
    } catch (e: CancellationException) {
        throw e
    } catch (e: Exception) {
        Result.failure(e)
    }
