package pl.kinowo.auth

/**
 * What [pl.kinowo.ui.KinowoViewModel] drives of the server mirror: begin
 * observing the auth state, reconcile on foreground-resume, and push the
 * user's hide/unhide/clear edits (each naming the country it was made in).
 * [StateSyncService] is the real one; the seam lets a ViewModel test assert
 * what it asked for without standing up the network-facing sync.
 */
interface StateSync {
    fun start()
    suspend fun reconcileCurrentCountry()
    fun hide(country: String, title: String)
    fun unhide(country: String, title: String)
    fun clear(country: String)
}
