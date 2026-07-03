package pl.kinowo.ui.list

import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.DragInteraction
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.IntrinsicSize
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.systemBarsPadding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.rememberLazyListState
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.material.icons.automirrored.filled.KeyboardArrowRight
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.ExpandLess
import androidx.compose.material.icons.filled.ExpandMore
import androidx.compose.material.icons.filled.Search
import androidx.compose.material3.Button
import androidx.compose.material3.Checkbox
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.ExposedDropdownMenuBox
import androidx.compose.material3.ExposedDropdownMenuDefaults
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.MenuAnchorType
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.SegmentedButton
import androidx.compose.material3.SegmentedButtonDefaults
import androidx.compose.material3.SheetState
import androidx.compose.material3.SingleChoiceSegmentedButtonRow
import androidx.compose.material3.Surface
import androidx.compose.material3.Switch
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
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.input.nestedscroll.NestedScrollConnection
import androidx.compose.ui.input.nestedscroll.NestedScrollSource
import androidx.compose.ui.input.nestedscroll.nestedScroll
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.IntRect
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.Velocity
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties
import androidx.compose.ui.window.Popup
import androidx.compose.ui.window.PopupPositionProvider
import androidx.compose.ui.window.PopupProperties
import pl.kinowo.filter.FormatFilter
import pl.kinowo.model.Cities
import pl.kinowo.filter.SortOption
import pl.kinowo.model.Film
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.ui.NameCount
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

/**
 * The "Ukryte filmy" card as a full-screen overlay over the Filtry sheet: a
 * [Dialog] sized to the whole window (default platform width removed, content
 * filling max size) so the hidden list gets the entire screen. System back and
 * a scrim tap both close it back to the filter list.
 */
@Composable
private fun HiddenFilmsScreen(viewModel: KinowoViewModel, onClose: () -> Unit) {
    Dialog(
        onDismissRequest = onClose,
        properties = DialogProperties(usePlatformDefaultWidth = false),
    ) {
        Surface(Modifier.fillMaxSize()) {
            HiddenFilmsCard(viewModel, onBack = onClose)
        }
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
    val allCountries = remember(films) { viewModel.allCountries(films) }
    val allGenres = remember(films) { viewModel.allGenres(films) }
    val allDirectors = remember(films) { viewModel.allDirectors(films) }
    val allCast = remember(films) { viewModel.allCast(films) }

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
            item {
                Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
                    Text("Filtry", fontSize = 20.sp, fontWeight = FontWeight.Bold, modifier = Modifier.weight(1f))
                    TextButton(onClick = { viewModel.clearFilters(); onClose() }) { Text("Wyczyść") }
                }
            }

            // Sortuj — view-ordering axis above the content filters, mirroring
            // the web "Sortuj" dropdown.
            item(key = "sec_sort") {
                FilterSectionLabel("Sortuj")
                SegmentedChoice(
                    options = SortOption.entries.map { it.label to it },
                    selected = viewModel.sortBy,
                ) { viewModel.sortBy = it }
            }

            // Section order mirrors the web Filtry panel (app/views/_navbar.scala.html):
            // Sortuj → Ukryte filmy → Kraj/Gatunek/Reżyseria/Obsada → Wymiar/Wersja/IMAX/Od godziny.
            // Cinema choice lives in the top-bar pill row (see CinemaPillBar), not
            // here. (Web's "Sale" room picker has no Android equivalent either.)

            // Ukryte filmy — a nav row that opens its own card (HiddenFilmsCard),
            // mirroring iOS; the inline list would crowd out every other filter
            // once the hidden set grows. Stays below Sortuj.
            if (hidden.isNotEmpty()) {
                item(key = "sec_hidden") {
                    HiddenFilmsRow(count = hidden.size, onClick = onOpenHidden)
                }
            }

            // Kraj / Gatunek / Reżyseria / Obsada (excluded sets)
            collapsibleNameFilter(this, "Kraj produkcji", allCountries, viewModel.excludedCountries) { viewModel.excludedCountries = it }
            collapsibleNameFilter(this, "Gatunek", allGenres, viewModel.excludedGenres) { viewModel.excludedGenres = it }
            collapsibleNameFilter(this, "Reżyseria", allDirectors, viewModel.excludedDirectors) { viewModel.excludedDirectors = it }
            collapsibleNameFilter(this, "Obsada", allCast, viewModel.excludedCast) { viewModel.excludedCast = it }

            // Wymiar
            item {
                FilterSectionLabel("Wymiar")
                SegmentedChoice(
                    options = listOf("Wszystkie" to "", "2D" to "2D", "3D" to "3D"),
                    selected = viewModel.formatFilter.dimension,
                ) { viewModel.formatFilter = viewModel.formatFilter.copy(dimension = it) }
            }
            // Wersja
            item {
                FilterSectionLabel("Wersja")
                SegmentedChoice(
                    options = listOf("Wszystkie" to "", "Napisy" to "NAP", "Dubbing" to "DUB"),
                    selected = viewModel.formatFilter.language,
                ) { viewModel.formatFilter = viewModel.formatFilter.copy(language = it) }
            }
            // IMAX
            item {
                ToggleRow("Tylko IMAX", viewModel.formatFilter.imax) {
                    viewModel.formatFilter = viewModel.formatFilter.copy(imax = it)
                }
            }
            // Od godziny
            item {
                FilterSectionLabel("Od godziny")
                FromHourRow(viewModel.formatFilter) { viewModel.formatFilter = it }
            }

            // Miasto — the city the repertoire is served for. Last filter,
            // right above the account section, mirroring iOS FiltersBar.
            // Usable before login (it's just a city switch); switching
            // re-fetches.
            item(key = "sec_city") { CitySection(viewModel) }

            item { AccountSection(viewModel) }

            item { Column(Modifier.padding(bottom = 24.dp)) {} }
        }
    }
}

/**
 * Konto / Zaloguj się — the Android twin of iOS FiltersBar's Account section.
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
            Text("Konto", fontWeight = FontWeight.SemiBold, modifier = Modifier.padding(bottom = 4.dp))
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
                    Text("Wyloguj")
                }
                TextButton(onClick = { viewModel.deleteAccount() }) {
                    Text("Usuń konto", color = MaterialTheme.colorScheme.error)
                }
            }
        } else {
            Text("Zaloguj się", fontWeight = FontWeight.SemiBold, modifier = Modifier.padding(bottom = 4.dp))
            Button(
                onClick = { viewModel.signInWithGoogle(context) },
                modifier = Modifier.fillMaxWidth(),
            ) { Text("Zaloguj przez Google") }
            Button(
                onClick = { viewModel.signInWithFacebook(context) },
                modifier = Modifier.fillMaxWidth().padding(top = 6.dp),
            ) { Text("Zaloguj przez Facebook") }
        }
    }
}



/**
 * A collapsible exclude-list: each entry is checked when *included*; un-checking
 * adds it to the excluded set. Mirrors iOS NameFilterList (toggle = visible).
 */
@OptIn(ExperimentalMaterial3Api::class)
private fun collapsibleNameFilter(
    scope: androidx.compose.foundation.lazy.LazyListScope,
    title: String,
    entries: List<NameCount>,
    excluded: Set<String>,
    onChange: (Set<String>) -> Unit,
) {
    if (entries.isEmpty()) return
    scope.item(key = "sec_$title") {
        CollapsibleSection(title, if (excluded.isNotEmpty()) "${excluded.size} ukrytych" else null) {
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
private fun CollapsibleSection(
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
            Icon(if (expanded) Icons.Filled.ExpandLess else Icons.Filled.ExpandMore, contentDescription = null)
        }
        if (expanded) content()
    }
}

/**
 * Miasto — the active city, as a Material3 ExposedDropdownMenu: a read-only
 * field showing the active city over an elevated, animated menu of
 * [Cities.allSorted]. Picking persists the choice (and re-fetches that city's
 * repertoire). Independent of login — it's just a city switch.
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun CitySection(viewModel: KinowoViewModel) {
    val selected by viewModel.selectedCity.collectAsState()
    val current = Cities.allSorted.firstOrNull { it.slug == selected } ?: Cities.DEFAULT
    var expanded by remember { mutableStateOf(false) }
    FilterSectionLabel("Miasto")
    ExposedDropdownMenuBox(
        expanded = expanded,
        onExpandedChange = { expanded = it },
        modifier = Modifier.fillMaxWidth(),
    ) {
        OutlinedTextField(
            value = current.name,
            onValueChange = {},
            readOnly = true,
            singleLine = true,
            trailingIcon = { ExposedDropdownMenuDefaults.TrailingIcon(expanded = expanded) },
            modifier = Modifier.menuAnchor(MenuAnchorType.PrimaryNotEditable).fillMaxWidth(),
        )
        ExposedDropdownMenu(expanded = expanded, onDismissRequest = { expanded = false }) {
            Cities.allSorted.forEach { city ->
                DropdownMenuItem(
                    text = { Text(city.name) },
                    onClick = {
                        viewModel.setCity(city.slug)
                        expanded = false
                    },
                    contentPadding = ExposedDropdownMenuDefaults.ItemContentPadding,
                )
            }
        }
    }
}

/**
 * The "Ukryte filmy" entry in the filter list: a tappable row (title, count
 * badge, forward chevron) that opens [HiddenFilmsCard]. Mirrors iOS's
 * NavigationLink row rather than the old inline collapsible.
 */
@Composable
private fun HiddenFilmsRow(count: Int, onClick: () -> Unit) {
    Row(
        Modifier.fillMaxWidth().clickable { onClick() }.padding(vertical = 8.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        Text("Ukryte filmy", fontWeight = FontWeight.SemiBold, modifier = Modifier.weight(1f))
        Text("$count", color = TextSecondary, fontSize = 12.sp, modifier = Modifier.padding(end = 8.dp))
        Icon(Icons.AutoMirrored.Filled.KeyboardArrowRight, contentDescription = null)
    }
}

/**
 * The "Ukryte filmy" card body (hosted full-screen by [HiddenFilmsScreen]): a
 * back header, a search box that narrows the list as you type (case-insensitive
 * substring, shared shape with iOS HiddenFilmsFilter), each title with a "Pokaż"
 * un-hide, and a "Pokaż wszystkie" bulk action. The list fills the rest of the
 * screen. Closes automatically once the set drains empty — there is nothing
 * left to manage.
 */
@Composable
private fun HiddenFilmsCard(viewModel: KinowoViewModel, onBack: () -> Unit) {
    val hidden by viewModel.hiddenFilms.collectAsState()
    // Unhiding the last film leaves nothing to manage — close back to the filter
    // list (its "Ukryte filmy" row has vanished too). Run as an effect so the
    // state change doesn't happen mid-composition.
    LaunchedEffect(hidden.isEmpty()) {
        if (hidden.isEmpty()) onBack()
    }
    var query by remember { mutableStateOf("") }
    val titles = remember(hidden, query) {
        hidden.sortedWith(String.CASE_INSENSITIVE_ORDER)
            .filter { query.isBlank() || it.contains(query.trim(), ignoreCase = true) }
    }
    Column(Modifier.fillMaxSize().systemBarsPadding().padding(horizontal = 16.dp)) {
        Row(Modifier.fillMaxWidth().padding(vertical = 4.dp), verticalAlignment = Alignment.CenterVertically) {
            IconButton(onClick = onBack) {
                Icon(Icons.AutoMirrored.Filled.ArrowBack, contentDescription = "Wstecz")
            }
            Text("Ukryte filmy", fontSize = 20.sp, fontWeight = FontWeight.Bold)
        }
        HiddenFilmsSearchField(query) { query = it }
        LazyColumn(Modifier.fillMaxWidth().weight(1f)) {
            items(titles, key = { "hid_$it" }) { title ->
                Row(Modifier.fillMaxWidth().padding(vertical = 2.dp), verticalAlignment = Alignment.CenterVertically) {
                    Text(title, fontSize = 14.sp, modifier = Modifier.weight(1f))
                    TextButton(onClick = { viewModel.unhide(title) }) { Text("Pokaż") }
                }
            }
            item(key = "hid_unhide_all") {
                Row(Modifier.fillMaxWidth().padding(vertical = 2.dp), horizontalArrangement = Arrangement.End) {
                    TextButton(onClick = { viewModel.unhideAll() }) { Text("Pokaż wszystkie") }
                }
            }
        }
    }
}

/**
 * Search box on the Ukryte filmy card: a Material3 [OutlinedTextField] with a
 * magnifier lead and a clear glyph, fitting the sheet surface (the dark
 * top-bar SearchFieldContent in ListScreen would clash here).
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun HiddenFilmsSearchField(query: String, onChange: (String) -> Unit) {
    OutlinedTextField(
        value = query,
        onValueChange = onChange,
        singleLine = true,
        placeholder = { Text("Szukaj filmu") },
        leadingIcon = { Icon(Icons.Filled.Search, contentDescription = null) },
        trailingIcon = {
            if (query.isNotEmpty()) {
                Icon(
                    Icons.Filled.Close,
                    contentDescription = "Wyczyść",
                    modifier = Modifier.clickable { onChange("") },
                )
            }
        },
        keyboardOptions = KeyboardOptions(imeAction = ImeAction.Search),
        modifier = Modifier.fillMaxWidth().padding(vertical = 4.dp),
    )
}

@Composable
private fun FilterSectionLabel(text: String) {
    Text(text, fontWeight = FontWeight.SemiBold, modifier = Modifier.padding(top = 8.dp, bottom = 2.dp))
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun <T> SegmentedChoice(options: List<Pair<String, T>>, selected: T, onSelect: (T) -> Unit) {
    SingleChoiceSegmentedButtonRow(Modifier.fillMaxWidth()) {
        options.forEachIndexed { i, (label, value) ->
            SegmentedButton(
                selected = selected == value,
                onClick = { onSelect(value) },
                shape = SegmentedButtonDefaults.itemShape(i, options.size),
            ) { Text(label) }
        }
    }
}

@Composable
private fun ToggleRow(label: String, checked: Boolean, onChange: (Boolean) -> Unit) {
    Row(Modifier.fillMaxWidth().padding(vertical = 2.dp), verticalAlignment = Alignment.CenterVertically) {
        Text(label, modifier = Modifier.weight(1f))
        Switch(checked = checked, onCheckedChange = onChange)
    }
}

@Composable
private fun CheckRow(label: String, checked: Boolean, onChange: (Boolean) -> Unit) {
    Row(
        Modifier.fillMaxWidth().clickable { onChange(!checked) }.padding(vertical = 1.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        Checkbox(checked = checked, onCheckedChange = onChange)
        Text(label, fontSize = 14.sp)
    }
}

/**
 * Od godziny — the hour and minute floor as a pair of (window-centred)
 * dropdowns. The hour leads with "Dowolna" (no floor); the minute dropdown only
 * appears once a real hour is picked, and choosing "Dowolna" clears the minute.
 */
@Composable
private fun FromHourRow(filter: FormatFilter, onChange: (FormatFilter) -> Unit) {
    val hours = listOf(-1) + (0..23).toList()
    var hourExpanded by remember { mutableStateOf(false) }
    var minExpanded by remember { mutableStateOf(false) }
    Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.spacedBy(8.dp), verticalAlignment = Alignment.CenterVertically) {
        Dropdown(
            label = if (filter.fromHour < 0) "Dowolna" else "%02d".format(filter.fromHour),
            expanded = hourExpanded,
            onExpandedChange = { hourExpanded = it },
            items = hours.map { (if (it < 0) "Dowolna" else "%02d".format(it)) to it },
            modifier = Modifier.weight(1f),
        ) { onChange(filter.copy(fromHour = it, fromMinute = if (it < 0) 0 else filter.fromMinute)); hourExpanded = false }
        if (filter.fromHour >= 0) {
            Dropdown(
                label = "%02d".format(filter.fromMinute),
                expanded = minExpanded,
                onExpandedChange = { minExpanded = it },
                items = listOf(0, 15, 30, 45).map { "%02d".format(it) to it },
                modifier = Modifier.weight(1f),
            ) { onChange(filter.copy(fromMinute = it)); minExpanded = false }
        }
    }
}

@Composable
private fun <T> Dropdown(
    label: String,
    expanded: Boolean,
    onExpandedChange: (Boolean) -> Unit,
    items: List<Pair<String, T>>,
    modifier: Modifier = Modifier,
    onPick: (T) -> Unit,
) {
    Box(modifier) {
        OutlinedButton(onClick = { onExpandedChange(true) }, modifier = Modifier.fillMaxWidth()) {
            Text(label, modifier = Modifier.weight(1f))
            Icon(
                if (expanded) Icons.Filled.ExpandLess else Icons.Filled.ExpandMore,
                contentDescription = null,
            )
        }
        if (expanded) {
            // A plain DropdownMenu anchors to the button's left edge; for the
            // full-width city picker that reads as "stuck to the left", and the
            // half-width Od-godziny pickers would sit off to one side. Centre the
            // menu in the window instead (see WindowCenteredMenuPositionProvider).
            val gapPx = with(LocalDensity.current) { MenuAnchorGap.roundToPx() }
            Popup(
                popupPositionProvider = remember(gapPx) { WindowCenteredMenuPositionProvider(gapPx) },
                onDismissRequest = { onExpandedChange(false) },
                properties = PopupProperties(focusable = true),
            ) {
                Surface(
                    shape = MaterialTheme.shapes.extraSmall,
                    color = MaterialTheme.colorScheme.surfaceContainer,
                    tonalElevation = 3.dp,
                    shadowElevation = 3.dp,
                ) {
                    Column(
                        Modifier
                            .padding(vertical = 8.dp)
                            .width(IntrinsicSize.Max)
                            .verticalScroll(rememberScrollState()),
                    ) {
                        items.forEach { (text, value) ->
                            DropdownMenuItem(text = { Text(text) }, onClick = { onPick(value) })
                        }
                    }
                }
            }
        }
    }
}

private val MenuAnchorGap = 4.dp

/**
 * Gates the Filtry [ModalBottomSheet]'s drag-to-close so it only fires when a
 * downward drag STARTS with the list already at the top. [onDragStart] snapshots
 * whether the list was at the top at finger-down; while it wasn't, [connection]
 * keeps the leftover from the sheet's own nested-scroll connection so it can't
 * yank the sheet closed mid-scroll.
 *
 * Two leftovers, handled differently:
 *  - [onPostScroll] is swallowed ONLY for a real drag ([NestedScrollSource.UserInput]).
 *    During a fling ([NestedScrollSource.SideEffect]) it must pass through as zero:
 *    swallowing it would make the LazyColumn's fling believe it's still scrolling, so
 *    it never cancels at the top and keeps "flinging in place" — leaving the list in
 *    scrollInProgress, which eats the first tap after a flick. The sheet ignores
 *    SideEffect in its own onPostScroll anyway, so letting it through can't close it.
 *  - [onPostFling] (the leftover velocity, where the sheet DOES close on a flick) is
 *    always swallowed, so a flick that began below the top still can't close it.
 *
 * The decisions are pure functions of the snapshot + direction + source, so they're
 * unit-tested directly (TopOnlyDismissScrollGateTest).
 */
internal class TopOnlyDismissScrollGate(private val atTop: () -> Boolean) {
    private var atTopAtDragStart = true

    /** Snapshot, when a drag begins, whether the list was already at the top.
     *  Driven by the list's own [DragInteraction.Start] (see FiltersList) — that
     *  fires once per real drag and ignores programmatic scrolls, so the snapshot
     *  is taken at the true gesture start, not on a stray earlier scroll event. */
    fun onDragStart() {
        atTopAtDragStart = atTop()
    }

    /** Leftover downward DRAG scroll the sheet must not see (positive Y, user input),
     *  else [Offset.Zero]. Fling frames pass through so the list's fling can stop. */
    fun swallowedPostScroll(available: Offset, source: NestedScrollSource): Offset =
        if (source == NestedScrollSource.UserInput && !atTopAtDragStart && available.y > 0f)
            Offset(0f, available.y)
        else Offset.Zero

    /** Leftover downward fling velocity the sheet must not see (positive Y), else [Velocity.Zero]. */
    fun swallowedPostFling(available: Velocity): Velocity =
        if (!atTopAtDragStart && available.y > 0f) Velocity(0f, available.y) else Velocity.Zero

    val connection: NestedScrollConnection = object : NestedScrollConnection {
        override fun onPostScroll(consumed: Offset, available: Offset, source: NestedScrollSource): Offset =
            swallowedPostScroll(available, source)

        override suspend fun onPostFling(consumed: Velocity, available: Velocity): Velocity =
            swallowedPostFling(available)
    }
}

/**
 * Positions a popup horizontally centred in the window, dropping below the
 * anchor (and flipping above it when there isn't room below). Used by
 * [Dropdown] so the filter menus open in the middle of the screen rather than
 * pinned to the anchor's left edge. [calculatePosition] is a pure function of
 * its geometry inputs, so it's unit-tested directly (WindowCenteredMenuPositionProviderTest).
 */
internal class WindowCenteredMenuPositionProvider(
    private val verticalGapPx: Int,
) : PopupPositionProvider {
    override fun calculatePosition(
        anchorBounds: IntRect,
        windowSize: IntSize,
        layoutDirection: LayoutDirection,
        popupContentSize: IntSize,
    ): IntOffset {
        val horizontalOffset = ((windowSize.width - popupContentSize.width) / 2).coerceAtLeast(0)
        val below = anchorBounds.bottom + verticalGapPx
        val verticalOffset =
            if (below + popupContentSize.height <= windowSize.height || anchorBounds.top < popupContentSize.height) {
                below
            } else {
                (anchorBounds.top - verticalGapPx - popupContentSize.height).coerceAtLeast(0)
            }
        return IntOffset(horizontalOffset, verticalOffset)
    }
}
