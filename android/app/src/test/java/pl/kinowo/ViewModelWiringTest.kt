package pl.kinowo

import androidx.test.core.app.ApplicationProvider
import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.auth.HttpHiddenFilmsClient
import pl.kinowo.auth.HttpLanguageClient
import pl.kinowo.model.Country

/**
 * Every signed-in call must reach the SAME deployment as the sign-in itself:
 * the session cookie `/auth/exchange` sets belongs to that host, and the
 * per-country hiddenFilms / language endpoints only see it there. The auth
 * repository used to keep its `https://kinowo.net` default while the sync
 * clients moved to the country's base URL, so outside Poland the user signed
 * in on one host and every sync call went, cookie-less, to another.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class ViewModelWiringTest {

    @Test
    fun authAndSyncClientsAllTalkToTheSelectedCountrysDeployment() {
        val uk = Country.byCode("uk")

        val factory = kinowoViewModelFactory(ApplicationProvider.getApplicationContext(), uk)

        assertEquals(uk.baseUrl, factory.authRepository.baseUrl)
        assertEquals(uk.baseUrl, (factory.hiddenFilmsClient as HttpHiddenFilmsClient).baseUrl)
        assertEquals(uk.baseUrl, (factory.languageClient as HttpLanguageClient).baseUrl)
    }
}
