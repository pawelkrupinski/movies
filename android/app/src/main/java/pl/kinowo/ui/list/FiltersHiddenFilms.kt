package pl.kinowo.ui.list

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.systemBarsPadding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties
import pl.kinowo.R
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.ui.theme.TextSecondary

/**
 * The "Ukryte filmy" card as a full-screen overlay over the Filtry sheet: a
 * [Dialog] sized to the whole window (default platform width removed, content
 * filling max size) so the hidden list gets the entire screen. System back and
 * a scrim tap both close it back to the filter list.
 */
@Composable
internal fun HiddenFilmsScreen(viewModel: KinowoViewModel, onClose: () -> Unit) {
    Dialog(
        onDismissRequest = onClose,
        properties = DialogProperties(usePlatformDefaultWidth = false),
    ) {
        Surface(Modifier.fillMaxSize()) {
            HiddenFilmsCard(viewModel, onBack = onClose)
        }
    }
}

/**
 * The "Ukryte filmy" entry in the filter list: a tappable row (title, count
 * badge, forward chevron) that opens [HiddenFilmsCard]. Mirrors iOS's
 * NavigationLink row rather than the old inline collapsible.
 */
@Composable
internal fun HiddenFilmsRow(count: Int, onClick: () -> Unit) {
    Row(
        Modifier.fillMaxWidth().clickable { onClick() }.padding(vertical = 8.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        Text(stringResource(R.string.hidden_films), fontWeight = FontWeight.SemiBold, modifier = Modifier.weight(1f))
        Text("$count", color = TextSecondary, fontSize = 12.sp, modifier = Modifier.padding(end = 8.dp))
        Icon(painterResource(R.drawable.ic_keyboard_arrow_right), contentDescription = null)
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
internal fun HiddenFilmsCard(viewModel: KinowoViewModel, onBack: () -> Unit) {
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
                Icon(painterResource(R.drawable.ic_arrow_back), contentDescription = stringResource(R.string.back))
            }
            Text(stringResource(R.string.hidden_films), fontSize = 20.sp, fontWeight = FontWeight.Bold)
        }
        HiddenFilmsSearchField(query) { query = it }
        LazyColumn(Modifier.fillMaxWidth().weight(1f)) {
            items(titles, key = { "hid_$it" }) { title ->
                Row(Modifier.fillMaxWidth().padding(vertical = 2.dp), verticalAlignment = Alignment.CenterVertically) {
                    Text(title, fontSize = 14.sp, modifier = Modifier.weight(1f))
                    TextButton(onClick = { viewModel.unhide(title) }) { Text(stringResource(R.string.show)) }
                }
            }
            item(key = "hid_unhide_all") {
                Row(Modifier.fillMaxWidth().padding(vertical = 2.dp), horizontalArrangement = Arrangement.End) {
                    TextButton(onClick = { viewModel.unhideAll() }) { Text(stringResource(R.string.show_all)) }
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
        placeholder = { Text(stringResource(R.string.search_films)) },
        leadingIcon = { Icon(painterResource(R.drawable.ic_search), contentDescription = null) },
        trailingIcon = {
            if (query.isNotEmpty()) {
                Icon(
                    painterResource(R.drawable.ic_close),
                    contentDescription = stringResource(R.string.clear),
                    modifier = Modifier.clickable { onChange("") },
                )
            }
        },
        keyboardOptions = KeyboardOptions(imeAction = ImeAction.Search),
        modifier = Modifier.fillMaxWidth().padding(vertical = 4.dp),
    )
}
