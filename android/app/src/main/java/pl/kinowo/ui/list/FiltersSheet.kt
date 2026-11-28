package pl.kinowo.ui.list

import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.DragInteraction
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.LazyListScope
import androidx.compose.foundation.lazy.rememberLazyListState
import androidx.compose.foundation.lazy.items
import androidx.compose.ui.res.painterResource
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.Button
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.SheetState
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.nestedscroll.nestedScroll
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.pluralStringResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.R
import pl.kinowo.model.Cities
import pl.kinowo.model.Country
import pl.kinowo.model.defaultCity
import pl.kinowo.model.sortedForPicker
import pl.kinowo.model.selected
import pl.kinowo.filter.SortOption
import pl.kinowo.model.Film
import pl.kinowo.filter.hasImaxShowtime
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.filter.NameCount
import pl.kinowo.filter.FilterFacets
import pl.kinowo.ui.countryNameRes
import pl.kinowo.ui.common.labelText
import pl.kinowo.ui.theme.TextSecondary

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun FiltersSheet(
    viewModel: KinowoViewModel,
    films: List<Film>,
    sheetState: SheetState,
    onDismiss: () -> Unit,
) {
    ModalBottomSheet(onDismissRequest = onDismiss, sheetState = sheetState) {
        FiltersSheetContent(viewModel, films, onClose = onDismiss)
    }
}

/**
 * The Filtry sheet body — extracted from the [ModalBottomSheet] wrapper so a
 * Robolectric Compose test can render the section list directly and assert
 * its order (see FiltersSheetOrderTest).
 */
@Composable
internal fun FiltersSheetContent(
    viewModel: KinowoViewModel,
    films: List<Film>,
    onClose: () -> Unit = {},
) {
    // "Ukryte filmy" opens its own full-screen card (mirroring iOS's pushed
    // screen) rather than expanding inline — the hidden set grows large enough
    // to crowd out every other filter, and a full screen gives it room. The
    // filter list stays mounted underneath; closing the card returns to it.
    var showHidden by remember { mutableStateOf(false) }
    FiltersList(viewModel, films, onOpenHidden = { showHidden = true }, onClose = onClose)
    if (showHidden) {
        HiddenFilmsScreen(viewModel, onClose = { showHidden = false })
    }
}

@Composable
private fun FiltersList(
    viewModel: KinowoViewModel,
    films: List<Film>,
    onOpenHidden: () -> Unit,
    onClose: () -> Unit,
) {
    val hidden by viewModel.hiddenFilms.collectAsState()
    val catalogCountries = viewModel.countryCatalog.collectAsState().value.countries
    val selectedCountryCode by viewModel.selectedCountryCode.collectAsState()
    val selectedCountry = catalogCountries.selected(selectedCountryCode)
    // Kraj / Gatunek / Reżyseria / Obsada: the entries each lists, and the excluded set it edits.
    val nameFilters = listOf(
        NameFilter(stringResource(R.string.filter_country), remember(films) { FilterFacets.allCountries(films) },
            viewModel.excludedCountries) { viewModel.excludedCountries = it },
        NameFilter(stringResource(R.string.filter_genre), remember(films) { FilterFacets.allGenres(films) },
            viewModel.excludedGenres) { viewModel.excludedGenres = it },
        NameFilter(stringResource(R.string.meta_director), remember(films) { FilterFacets.allDirectors(films) },
            viewModel.excludedDirectors) { viewModel.excludedDirectors = it },
        NameFilter(stringResource(R.string.meta_cast), remember(films) { FilterFacets.allCast(films) },
            viewModel.excludedCast) { viewModel.excludedCast = it },
    )

    // Drag-to-close must only fire when the gesture STARTS with the list already
    // at the top. Otherwise a downward swipe to scroll back up — which rolls the
    // list to its top mid-gesture — would spill its leftover into the
    // ModalBottomSheet's own nested-scroll drag and yank the sheet closed instead
    // of scrolling (see FiltersSheetDragDismissTest). The gate snapshots "was the
    // list at the top when this drag began?" from the list's own
    // [DragInteraction.Start] — which fires once per real drag and ignores
    // programmatic scrolls — NOT a pointerInput, which shadowed the rows' taps
    // (a swipe near a section header read as a tap that collapsed it, the first
    // tap after a scroll swallowed).
    val listState = rememberLazyListState()
    val gate = remember(listState) { TopOnlyDismissScrollGate(atTop = { !listState.canScrollBackward }) }
    LaunchedEffect(gate, listState) {
        listState.interactionSource.interactions.collect { interaction ->
            if (interaction is DragInteraction.Start) gate.onDragStart()
        }
    }

    Box(Modifier.nestedScroll(gate.connection)) {
        LazyColumn(
            state = listState,
            modifier = Modifier.fillMaxWidth().padding(horizontal = 16.dp),
            verticalArrangement = Arrangement.spacedBy(4.dp),
        ) {
            // Section order mirrors the web Filtry panel (_navbar.scala.html):
            // Sortuj → Ukryte filmy → Kina → Kraj/Gatunek/Reżyseria/Obsada →
            // Wymiar/Wersja/IMAX/Od godziny. (Web's "Sale" room picker has no
            // Android equivalent.)
            headerItems(viewModel, films, onClose)
            if (hidden.isNotEmpty()) {
                // Ukryte filmy — a nav row that opens its own card (HiddenFilmsCard),
                // mirroring iOS; the inline list would crowd out every other filter
                // once the hidden set grows. Stays below Sortuj.
                item(key = "sec_hidden") { HiddenFilmsRow(count = hidden.size, onClick = onOpenHidden) }
            }
            // Kina — the one cinema filter. Flat cities get a plain checkbox
            // list, split cities the same universe grouped into areas.
            item(key = "sec_cinemas") { CinemasSection(viewModel, films) }
            for (filter in nameFilters) collapsibleNameFilter(this, filter.title, filter.entries, filter.excluded, filter.onChange)
            formatItems(viewModel, films, selectedCountry)
            tailItems(viewModel, onClose)
        }
    }
}

/** One collapsible name filter: its [entries] and the [excluded] subset [onChange] edits. */
private class NameFilter(
    val title: String,
    val entries: List<NameCount>,
    val excluded: Set<String>,
    val onChange: (Set<String>) -> Unit,
)

/** The title row with Wyczyść, and Sortuj — the view-ordering axis above the content filters,
 *  mirroring the web "Sortuj" dropdown. */
private fun LazyListScope.headerItems(viewModel: KinowoViewModel, films: List<Film>, onClose: () -> Unit) {
    item {
        Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
            Text(stringResource(R.string.filters), fontSize = 20.sp, fontWeight = FontWeight.Bold, modifier = Modifier.weight(1f))
            TextButton(onClick = { viewModel.clearFilters(films); onClose() }) { Text(stringResource(R.string.clear)) }
        }
    }
    item(key = "sec_sort") {
        FilterSectionLabel(stringResource(R.string.sort))
        SegmentedChoice(
            options = SortOption.entries.map { it.labelText() to it },
            selected = viewModel.sortBy,
        ) { viewModel.sortBy = it }
    }
}

/** Wymiar, Wersja, IMAX (only where some loaded showtime is IMAX; elsewhere the toggle could
 *  only blank the list, and the web and iOS hide it too) and Od godziny. */
private fun LazyListScope.formatItems(viewModel: KinowoViewModel, films: List<Film>, selectedCountry: Country) {
    item {
        FilterSectionLabel(stringResource(R.string.filter_dimension))
        SegmentedChoice(
            options = listOf(stringResource(R.string.all) to "", "2D" to "2D", "3D" to "3D"),
            selected = viewModel.formatFilter.dimension,
        ) { viewModel.formatFilter = viewModel.formatFilter.copy(dimension = it) }
    }
    // Wersja — the two choices filter on a LITERAL format token, so they carry the selected
    // country's own pair from the catalog (`OmU`/`DF` in Germany, `VOSE`/`DOB` in Spain);
    // Poland's, hardcoded here before, matched nothing anywhere else.
    item {
        val versionTokens = selectedCountry.versionTokens
        FilterSectionLabel(stringResource(R.string.filter_version))
        SegmentedChoice(
            options = listOf(
                stringResource(R.string.all) to "",
                stringResource(R.string.version_subtitles) to versionTokens.subtitled,
                stringResource(R.string.version_dubbing) to versionTokens.dubbed,
            ),
            selected = viewModel.formatFilter.language,
        ) { viewModel.formatFilter = viewModel.formatFilter.copy(language = it) }
    }
    if (films.hasImaxShowtime()) {
        item {
            ToggleRow(stringResource(R.string.imax_only), viewModel.formatFilter.imax) {
                viewModel.formatFilter = viewModel.formatFilter.copy(imax = it)
            }
        }
    }
    item {
        FilterSectionLabel(stringResource(R.string.from_time))
        FromHourRow(viewModel.formatFilter) { viewModel.formatFilter = it }
    }
}

/** Miasto — a single entry point back to the first-launch picker, replacing the inline
 *  Kraj/Miasto dropdowns; last filter, right above the account section, mirroring iOS
 *  FiltersSheet and usable before login (it's just a city switch). Język — independent of
 *  Miasto/Kraj (see KinowoViewModel.selectedLanguage): picking a country no longer implies a
 *  language, so it gets its own row right below, as iOS and the web surface the same picker. */
private fun LazyListScope.tailItems(viewModel: KinowoViewModel, onClose: () -> Unit) {
    item(key = "sec_city") { CitySection(viewModel, onClose) }
    item(key = "sec_language") { LanguageSection(viewModel) }
    item { AccountSection(viewModel) }
    item { Column(Modifier.padding(bottom = 24.dp)) {} }
}

/**
 * Konto / Zaloguj się — the Android twin of iOS FiltersSheet's `AccountSection`.
 * Signed in: show who, plus Wyloguj / Usuń konto. Signed out: the two web
 * OAuth buttons. Hiding/disabling sync to the server while signed in (see
 * [pl.kinowo.auth.StateSyncService]).
 */
@Composable
private fun AccountSection(viewModel: KinowoViewModel) {
    val user by viewModel.user.collectAsState()
    val context = LocalContext.current
    val signedIn = user

    Column(Modifier.fillMaxWidth().padding(top = 12.dp)) {
        if (signedIn != null) {
            Text(stringResource(R.string.account), fontWeight = FontWeight.SemiBold, modifier = Modifier.padding(bottom = 4.dp))
            Text(
                signedIn.displayName ?: signedIn.email ?: signedIn.provider,
                fontSize = 14.sp,
                color = TextSecondary,
            )
            Row(
                Modifier.fillMaxWidth().padding(top = 6.dp),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
                verticalAlignment = Alignment.CenterVertically,
            ) {
                OutlinedButton(onClick = { viewModel.signOut() }, modifier = Modifier.weight(1f)) {
                    Text(stringResource(R.string.sign_out))
                }
                TextButton(onClick = { viewModel.deleteAccount() }) {
                    Text(stringResource(R.string.delete_account), color = MaterialTheme.colorScheme.error)
                }
            }
            if (viewModel.deleteAccountFailed.collectAsState().value) {
                AlertDialog(
                    onDismissRequest = { viewModel.dismissDeleteAccountFailure() },
                    title = { Text(stringResource(R.string.delete_account)) },
                    text = { Text(stringResource(R.string.delete_account_failed)) },
                    confirmButton = {
                        TextButton(onClick = { viewModel.dismissDeleteAccountFailure() }) { Text(stringResource(R.string.close)) }
                    },
                )
            }
        } else {
            Text(stringResource(R.string.sign_in), fontWeight = FontWeight.SemiBold, modifier = Modifier.padding(bottom = 4.dp))
            Button(
                onClick = { viewModel.signInWithGoogle(context) },
                modifier = Modifier.fillMaxWidth(),
            ) { Text(stringResource(R.string.sign_in_google)) }
            Button(
                onClick = { viewModel.signInWithFacebook(context) },
                modifier = Modifier.fillMaxWidth().padding(top = 6.dp),
            ) { Text(stringResource(R.string.sign_in_facebook)) }
        }
    }
}

/**
 * A collapsible exclude-list: each entry is checked when *included*; un-checking
 * adds it to the excluded set. Mirrors iOS NameFilterList (toggle = visible).
 */
@OptIn(ExperimentalMaterial3Api::class)
private fun collapsibleNameFilter(
    scope: LazyListScope,
    title: String,
    entries: List<NameCount>,
    excluded: Set<String>,
    onChange: (Set<String>) -> Unit,
) {
    if (entries.isEmpty()) return
    scope.item(key = "sec_$title") {
        CollapsibleSection(title, if (excluded.isNotEmpty()) pluralStringResource(R.plurals.hidden_count, excluded.size, excluded.size) else null) {
            LazyColumn(Modifier.fillMaxWidth().heightIn(max = 280.dp)) {
                items(entries, key = { "${title}_${it.name}" }) { nc ->
                    CheckRow(
                        label = "${nc.name}  (${nc.count})",
                        checked = nc.name !in excluded,
                    ) { on -> onChange(if (on) excluded - nc.name else excluded + nc.name) }
                }
            }
        }
    }
}

/**
 * Expand/collapse section: a tappable header (bold title, optional count
 * badge, chevron) over a body that only composes when expanded. Shared by
 * Ukryte filmy, Kina, and the Kraj/Gatunek/… name filters so they fold
 * identically. Each call site keeps its own expanded state.
 *
 * [rememberSaveable], not [remember]: an expanded section (e.g. Kina with every
 * cinema) is one tall LazyColumn item, so scrolling past it disposes the item
 * and a plain `remember` would reset it to collapsed. LazyColumn restores
 * rememberSaveable per item key, so it stays expanded when scrolled back.
 */
@Composable
internal fun CollapsibleSection(
    title: String,
    countLabel: String?,
    content: @Composable () -> Unit,
) {
    var expanded by rememberSaveable { mutableStateOf(false) }
    Column {
        Row(
            Modifier.fillMaxWidth().clickable { expanded = !expanded }.padding(vertical = 8.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Text(title, fontWeight = FontWeight.SemiBold, modifier = Modifier.weight(1f))
            if (countLabel != null) {
                Text(countLabel, color = TextSecondary, fontSize = 12.sp, modifier = Modifier.padding(end = 8.dp))
            }
            Icon(if (expanded) painterResource(R.drawable.ic_expand_less) else painterResource(R.drawable.ic_expand_more), contentDescription = null)
        }
        if (expanded) content()
    }
}

/**
 * Miasto — a single entry point back to [pl.kinowo.ui.city.CityChoiceScreen],
 * the first-launch picker, replacing the inline Kraj/Miasto
 * `ExposedDropdownMenu`s: a status line naming the current city and country,
 * plus a "Wybierz inne miasto" button. Tapping it calls
 * [KinowoViewModel.pickAnotherCity] — the same clear-city + arm-explicit-pick
 * pair [KinowoViewModel.setCountry] already used for an in-sheet country
 * switch — so [pl.kinowo.ui.KinowoApp] re-gates to the chooser once the sheet
 * closes ([onClose]). Always shown (independent of
 * [pl.kinowo.model.isSwitchable]) since it now covers picking a different city
 * within the same country too, mirroring iOS FiltersSheet.
 */
@Composable
private fun CitySection(viewModel: KinowoViewModel, onClose: () -> Unit) {
    // Frozen at first composition (`.value`, not `collectAsState()`) rather
    // than read live: "Wybierz inne miasto" clears the city as part of
    // closing the sheet, and a live value would react to that before the
    // ModalBottomSheet finishes dismissing, flashing the country's DEFAULT
    // city in for a frame where the actually-selected one belongs. Nothing
    // else in this sheet can change the selection while it's open, so one
    // snapshot is enough — mirrors iOS FiltersSheet's `currentCityLabel`.
    val (currentCityName, currentCountryCode) = remember {
        val catalog = viewModel.countryCatalog.value
        val countryCode = Country.normalizeCode(viewModel.selectedCountryCode.value) ?: Country.default.code
        val cities = catalog.cities.sortedForPicker(countryCode)
        val currentCity = cities.firstOrNull { it.slug == viewModel.selectedCity.value }
            ?: catalog.cities.defaultCity(countryCode) ?: Cities.DEFAULT
        currentCity.name to countryCode
    }
    // The country half is a UI-language string (stringResource is a
    // @Composable call, so it can't live inside the `remember` block above)
    // read fresh every recomposition off the frozen code — deterministic, so
    // freezing only the city/country CHOICE (not the rendered country text)
    // changes nothing observable.
    val currentCityLabel = "$currentCityName, ${stringResource(countryNameRes(currentCountryCode))}"

    FilterSectionLabel(stringResource(R.string.filter_city))
    Text(
        currentCityLabel,
        color = TextSecondary,
        fontSize = 14.sp,
        modifier = Modifier.padding(bottom = 6.dp),
    )
    Button(
        onClick = { viewModel.pickAnotherCity(); onClose() },
        modifier = Modifier.fillMaxWidth(),
    ) { Text(stringResource(R.string.choose_other_city)) }
}

/** BCP-47 tag → native display name, in the fixed order offered by the picker. */
private val LanguageOptions = listOf(
    "pl" to "Polski",
    "en" to "English",
    "de" to "Deutsch",
    "es" to "Español",
)

/**
 * Język — the UI-language picker, independent of Miasto/Kraj (picking a
 * country no longer forces a language — see [KinowoViewModel.selectedLanguage]).
 * Reuses the file's one general-purpose dropdown widget ([Dropdown], the same
 * one [FromHourRow] opens for its hour/minute pickers) rather than inventing a
 * new selector. Persists through [KinowoViewModel.setLanguage]; MainActivity
 * observes the pref and recreates the activity so the new locale applies from
 * the next frame, without dropping the retained ViewModel.
 */
@Composable
private fun LanguageSection(viewModel: KinowoViewModel) {
    val selected by viewModel.selectedLanguage.collectAsState()
    var expanded by remember { mutableStateOf(false) }

    FilterSectionLabel(stringResource(R.string.filter_language))
    Dropdown(
        label = LanguageOptions.firstOrNull { it.first == selected }?.second ?: LanguageOptions.first().second,
        expanded = expanded,
        onExpandedChange = { expanded = it },
        items = LanguageOptions.map { (tag, name) -> name to tag },
    ) { tag -> viewModel.setLanguage(tag); expanded = false }
}
