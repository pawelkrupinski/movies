package pl.kinowo.ui

import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.flow.SharingStarted
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.combine
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.flow.stateIn
import kotlinx.coroutines.launch
import pl.kinowo.data.UserPreferences
import pl.kinowo.location.GrantedLocationSource
import pl.kinowo.model.Catalog
import pl.kinowo.model.Cities
import pl.kinowo.model.City
import pl.kinowo.model.CitySwitchSuggestion
import pl.kinowo.model.Country
import pl.kinowo.model.switchSuggestion
import pl.kinowo.ui.city.CityGateStart

/**
 * Which city the app shows, and how it changes: the first-launch gate (what it
 * starts on, a located or a picked city), a country switch, and the
 * "you're nearer another city — switch?" prompt with its one-shot suppressor.
 * Built and held by [KinowoViewModel] over its scope; every write goes to
 * [prefs], whose `selectedCity` collector re-fetches the listing.
 */
class CitySelection(
    private val prefs: UserPreferences,
    private val location: GrantedLocationSource,
    private val countryCatalog: StateFlow<Catalog>,
    private val selectedCity: StateFlow<String?>,
    private val selectedCountryCode: StateFlow<String?>,
    private val scope: CoroutineScope,
) {
    // Skips the one nearer-city check that the post-OAuth resume would otherwise
    // fire — armed when a web sign-in starts ([suppressNextCheck]) or a city is
    // re-picked with no detected nearest (see [chooseCityAtGate]).
    private val citySwitchSuppressor = CitySwitchSuppressor()

    /** A pending "you're nearer another city — switch?" prompt, or null. Set by
     *  [checkCitySwitch] when a granted-only location lands nearer a different
     *  supported city; cleared on accept ([setCity]) or decline
     *  ([dismissCitySwitch]). */
    var citySwitchSuggestion by mutableStateOf<CitySwitchSuggestion?>(null)
        private set

    /** What the first-launch gate should do — which country, and whether it may
     *  offer a located city (see [CityGateStart]).
     *
     *  Null means "the stored choices have not been read yet" and NOTHING else —
     *  unlike [selectedCountryCode], whose null conflates that with "never
     *  chosen". The gate must wait for a non-null value instead of filling the
     *  gap itself: the DataStore reads are asynchronous, so on the first
     *  composition after a country switch nothing has landed, and defaulting
     *  there scopes the nearest-city search to Poland and greets a Berlin user
     *  with "You're near Poznan". */
    val gateStart: StateFlow<CityGateStart?> =
        combine(prefs.selectedCountryCode, prefs.awaitingExplicitCityPick) { code, explicit ->
            CityGateStart(
                countryCode = Country.normalizeCode(code) ?: Country.default.code,
                locate = !explicit,
            )
        }.stateIn(scope, SharingStarted.Eagerly, null)

    /** Skip the next [checkCitySwitch]: a web sign-in's Custom Tab resume must
     *  not re-fire the prompt the user just dealt with. */
    fun suppressNextCheck() = citySwitchSuppressor.suppressNextCheck()

    /** Persist the chosen country. The activity observes [selectedCountryCode] and
     *  recreates itself so the new base URL + locale take effect.
     *
     *  Each country is its own deployment serving a DISJOINT set of cities, so the
     *  switch also drops the current city: keeping the old
     *  country's slug would request it against the new country's host, and re-arming
     *  the city gate lets the user pick a city that actually exists there. Clearing
     *  the city before persisting the code means the gate is already re-armed by the
     *  time MainActivity recreates. Harmless at the first-launch gate, where the
     *  city is null anyway. */
    fun setCountry(code: String) = scope.launch {
        reArmCityGate()
        prefs.setCountryCode(code)
    }

    /** Drop the current city and arm the gate for an explicit pick, without
     *  touching the country. Backs the Filtry sheet's "Pick another city"
     *  button — the sole replacement for the old inline Kraj/Miasto pickers —
     *  which re-gates to [pl.kinowo.ui.city.CityChoiceScreen] the same way
     *  [setCountry] already did for a country switch. */
    fun pickAnotherCity() = scope.launch { reArmCityGate() }

    /** Clear the persisted city and ask the gate for an explicit pick rather
     *  than a located offer — shared by [setCountry] (a new country's
     *  deployment may not serve the old city) and [pickAnotherCity] (the user
     *  asked to switch city or country from within the app). */
    private suspend fun reArmCityGate() {
        prefs.clearCity()
        // The user just asked to pick, so let them: the re-armed gate offers
        // an explicit choice rather than whatever city the device happens to
        // sit near.
        prefs.awaitExplicitCityPick()
    }

    /** Persist the chosen city. The ViewModel's `selectedCity` collector picks up
     *  the change and re-fetches that city's repertoire — no explicit reload.
     *  Also clears any pending switch prompt, since accepting one lands here. */
    fun setCity(slug: String) = scope.launch {
        citySwitchSuggestion = null
        // The excluded-cinema set is global and scoped per city at read time, so
        // a new city needs no reset — its own cinemas simply start unticked-free.
        prefs.setCity(slug)
    }

    /** Adopt a location-detected [city] at the gate's confirm screen — the
     *  first-launch hit, or a manual "use my location" hit from the picker.
     *  The manual button now searches EVERY country, so [city] may sit in a
     *  country other than the one currently open; when it does, switch the
     *  country and set the city in ONE atomic write via `prefs.setCityInCountry`
     *  rather than [setCountry], which clears the city — as a cross-country deep
     *  link ([DeepLinkRouter]) does: we're about to set this exact city, not
     *  re-arm the gate for a fresh pick. */
    fun adoptDetectedCity(city: City) = scope.launch {
        citySwitchSuggestion = null
        // Never two writes: MainActivity recreates the activity (cancelling
        // this coroutine) the instant the country pref changes, so a
        // `setCountryCode` followed by a `setCity` exposes the new country with
        // the old city and can lose the city to that teardown.
        if (city.country != selectedCountryCode.value) prefs.setCityInCountry(city.slug, city.country)
        else prefs.setCity(city.slug)
    }

    /** Adopt a city the user deliberately picked at the gate. When it differs
     *  from the location-detected [nearestSlug], pre-record that pair so
     *  [checkCitySwitch] doesn't fire the "you're nearer …" prompt the moment
     *  the repertoire appears — the pick was intentional. Seeds the key before
     *  persisting the city so the prompt check sees it.
     *
     *  [nearestSlug] is only ever non-null on the first-launch flow, which
     *  resolved a location fix before landing here. Reached any other way —
     *  Filtry's "Pick another city", or a country switch — there's no detected
     *  nearest to build a precise key from, so fall back to skipping the ONE
     *  check [checkCitySwitch] fires right after this pick (the same one-shot
     *  suppressor a web sign-in's Custom Tab resume uses): the pick was still
     *  deliberate, and a genuine later foreground still re-arms the check
     *  normally. */
    fun chooseCityAtGate(slug: String, nearestSlug: String?) = scope.launch {
        val key = Cities.initialChoiceSuppressKey(slug, nearestSlug)
        if (key != null) {
            prefs.setCitySwitchPromptKey(key)
        } else if (nearestSlug == null) {
            citySwitchSuppressor.suppressNextCheck()
        }
        citySwitchSuggestion = null
        prefs.setCity(slug)
    }

    /**
     * Offer to switch to a nearer supported city when the device — with location
     * already granted — is closer to a different city than the chosen one. Reads
     * a fix only if `ACCESS_COARSE_LOCATION` is granted (never prompts), then
     * defers to [Cities.switchSuggestion] for the once-per-pair decision. On a
     * hit, persists the pair key immediately so the prompt shows at most once per
     * pair regardless of accept/decline, and surfaces it as [citySwitchSuggestion]
     * for [KinowoApp] to render.
     */
    fun checkCitySwitch() = scope.launch {
        // A web sign-in just returned via a Custom Tab resume — skip the one
        // check that would re-surface the prompt the user already answered.
        if (citySwitchSuppressor.consumeShouldSkip()) return@launch
        if (citySwitchSuggestion != null) return@launch
        val chosen = selectedCity.value ?: return@launch
        val fix = location.resolveIfGranted() ?: return@launch
        val suggestion = countryCatalog.value.cities.switchSuggestion(
            chosenSlug = chosen,
            lat = fix.first,
            lon = fix.second,
            lastPromptKey = prefs.citySwitchPromptKey.first(),
            countryCode = selectedCountryCode.value ?: Country.default.code,
        ) ?: return@launch
        prefs.setCitySwitchPromptKey(suggestion.key)
        citySwitchSuggestion = suggestion
    }

    /** Decline the nearer-city prompt — just clears the dialog; the pair key was
     *  already persisted in [checkCitySwitch], so we won't re-ask for it. */
    fun dismissCitySwitch() { citySwitchSuggestion = null }
}
