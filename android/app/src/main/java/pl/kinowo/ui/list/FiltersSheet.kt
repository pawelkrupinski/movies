package pl.kinowo.ui.list

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.ExpandLess
import androidx.compose.material.icons.filled.ExpandMore
import androidx.compose.material3.Button
import androidx.compose.material3.Checkbox
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.SegmentedButton
import androidx.compose.material3.SegmentedButtonDefaults
import androidx.compose.material3.SheetState
import androidx.compose.material3.SingleChoiceSegmentedButtonRow
import androidx.compose.material3.Switch
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.filter.CinemaSection
import pl.kinowo.filter.FormatFilter
import pl.kinowo.filter.SortOption
import pl.kinowo.model.Film
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.ui.NameCount
import pl.kinowo.ui.theme.TextSecondary

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun FiltersSheet(
    vm: KinowoViewModel,
    films: List<Film>,
    showCinemaFilter: Boolean,
    sheetState: SheetState,
    onDismiss: () -> Unit,
) {
    val hidden by vm.hiddenFilms.collectAsState()
    val disabled by vm.disabledCinemas.collectAsState()
    val allCinemas = remember(films) { vm.allCinemas(films) }
    val allCountries = remember(films) { vm.allCountries(films) }
    val allGenres = remember(films) { vm.allGenres(films) }
    val allDirectors = remember(films) { vm.allDirectors(films) }
    val allCast = remember(films) { vm.allCast(films) }

    ModalBottomSheet(onDismissRequest = onDismiss, sheetState = sheetState) {
        LazyColumn(
            Modifier.fillMaxWidth().padding(horizontal = 16.dp),
            verticalArrangement = Arrangement.spacedBy(4.dp),
        ) {
            item {
                Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
                    Text("Filtry", fontSize = 20.sp, fontWeight = FontWeight.Bold, modifier = Modifier.weight(1f))
                    TextButton(onClick = { vm.clearFilters() }) { Text("Wyczyść") }
                }
            }

            // Sortuj — view-ordering axis above the content filters, mirroring
            // the web "Sortuj" dropdown.
            item(key = "sec_sort") {
                FilterSectionLabel("Sortuj")
                SegmentedChoice(
                    options = SortOption.entries.map { it.label to it },
                    selected = vm.sortBy,
                ) { vm.sortBy = it }
            }

            // Section order mirrors the web Filtry panel (app/views/_navbar.scala.html):
            // Sortuj → Ukryte filmy → Kina → Kraj/Gatunek/Reżyseria/Obsada → Wymiar/Wersja/IMAX/Od godziny.
            // (Web's "Sale" room picker has no Android equivalent.)

            // Ukryte filmy — collapsible, matching Kina and the name filters.
            if (hidden.isNotEmpty()) {
                item(key = "sec_hidden") {
                    CollapsibleSection("Ukryte filmy", "${hidden.size}") {
                        LazyColumn(Modifier.fillMaxWidth().heightIn(max = 280.dp)) {
                            items(hidden.toList(), key = { "hid_$it" }) { title ->
                                Row(Modifier.fillMaxWidth().padding(vertical = 2.dp), verticalAlignment = Alignment.CenterVertically) {
                                    Text(title, fontSize = 14.sp, modifier = Modifier.weight(1f))
                                    TextButton(onClick = { vm.unhide(title) }) { Text("Pokaż") }
                                }
                            }
                            item(key = "hid_unhide_all") {
                                Row(Modifier.fillMaxWidth().padding(vertical = 2.dp), horizontalArrangement = Arrangement.End) {
                                    TextButton(onClick = { vm.unhideAll() }) { Text("Pokaż wszystkie") }
                                }
                            }
                        }
                    }
                }
            }

            // Kina — collapsible, matching Ukryte filmy and the name filters.
            // Hidden on the /kina tab, where the cinema pill row pins a single
            // cinema and this multi-select would only confuse.
            if (showCinemaFilter && allCinemas.isNotEmpty()) {
                item(key = "sec_cinemas") {
                    CollapsibleSection("Kina", if (disabled.isNotEmpty()) "${disabled.size} wyłączonych" else null) {
                        LazyColumn(Modifier.fillMaxWidth().heightIn(max = 280.dp)) {
                            item(key = "cin_all") {
                                ToggleRow("Wszystkie kina", disabled.isEmpty()) { on ->
                                    vm.setDisabledCinemas(if (on) emptySet() else allCinemas.toSet())
                                }
                            }
                            items(allCinemas, key = { "cin_$it" }) { cinema ->
                                CheckRow(
                                    label = CinemaSection.pillName(cinema),
                                    checked = cinema !in disabled,
                                ) { on -> vm.toggleCinema(cinema, disabled = !on) }
                            }
                        }
                    }
                }
            }

            // Kraj / Gatunek / Reżyseria / Obsada (excluded sets)
            collapsibleNameFilter(this, "Kraj produkcji", allCountries, vm.excludedCountries) { vm.excludedCountries = it }
            collapsibleNameFilter(this, "Gatunek", allGenres, vm.excludedGenres) { vm.excludedGenres = it }
            collapsibleNameFilter(this, "Reżyseria", allDirectors, vm.excludedDirectors) { vm.excludedDirectors = it }
            collapsibleNameFilter(this, "Obsada", allCast, vm.excludedCast) { vm.excludedCast = it }

            // Wymiar
            item {
                FilterSectionLabel("Wymiar")
                SegmentedChoice(
                    options = listOf("Wszystkie" to "", "2D" to "2D", "3D" to "3D"),
                    selected = vm.formatFilter.dimension,
                ) { vm.formatFilter = vm.formatFilter.copy(dimension = it) }
            }
            // Wersja
            item {
                FilterSectionLabel("Wersja")
                SegmentedChoice(
                    options = listOf("Wszystkie" to "", "Napisy" to "NAP", "Dubbing" to "DUB"),
                    selected = vm.formatFilter.language,
                ) { vm.formatFilter = vm.formatFilter.copy(language = it) }
            }
            // IMAX
            item {
                ToggleRow("Tylko IMAX", vm.formatFilter.imax) {
                    vm.formatFilter = vm.formatFilter.copy(imax = it)
                }
            }
            // Od godziny
            item {
                FilterSectionLabel("Od godziny")
                FromHourRow(vm.formatFilter) { vm.formatFilter = it }
            }

            item { AccountSection(vm) }

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
private fun AccountSection(vm: KinowoViewModel) {
    val user by vm.user.collectAsState()
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
                OutlinedButton(onClick = { vm.signOut() }, modifier = Modifier.weight(1f)) {
                    Text("Wyloguj")
                }
                TextButton(onClick = { vm.deleteAccount() }) {
                    Text("Usuń konto", color = MaterialTheme.colorScheme.error)
                }
            }
        } else {
            Text("Zaloguj się", fontWeight = FontWeight.SemiBold, modifier = Modifier.padding(bottom = 4.dp))
            Button(
                onClick = { vm.signInWithGoogle(context) },
                modifier = Modifier.fillMaxWidth(),
            ) { Text("Zaloguj przez Google") }
            Button(
                onClick = { vm.signInWithFacebook(context) },
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
 */
@Composable
private fun CollapsibleSection(
    title: String,
    countLabel: String?,
    content: @Composable () -> Unit,
) {
    var expanded by remember { mutableStateOf(false) }
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

@OptIn(ExperimentalMaterial3Api::class)
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
    androidx.compose.foundation.layout.Box(modifier) {
        OutlinedButton(onClick = { onExpandedChange(true) }, modifier = Modifier.fillMaxWidth()) {
            Text(label, modifier = Modifier.weight(1f))
            Icon(
                if (expanded) Icons.Filled.ExpandLess else Icons.Filled.ExpandMore,
                contentDescription = null,
            )
        }
        DropdownMenu(expanded = expanded, onDismissRequest = { onExpandedChange(false) }) {
            items.forEach { (text, value) ->
                DropdownMenuItem(text = { Text(text) }, onClick = { onPick(value) })
            }
        }
    }
}
