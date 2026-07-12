package pl.kinowo.ui.list

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.Checkbox
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.R
import pl.kinowo.model.CinemaCatalog
import pl.kinowo.ui.theme.TextSecondary

/**
 * First-visit dialog for a SPLIT city (e.g. London): asks which areas to show,
 * all pre-selected (so confirming straight away shows everything — the flat
 * default). Unchecking an area excludes its cinemas via [onConfirm]. Shown once
 * per city (the caller gates on `areaPickerSeenCities`). Mirrors the web's and
 * iOS's first-visit area picker.
 */
@Composable
internal fun AreaPickerDialog(
    catalog: CinemaCatalog,
    onConfirm: (keptAreaSlugs: Set<String>) -> Unit,
) {
    // All areas pre-selected.
    var kept by remember { mutableStateOf(catalog.areas.map { it.slug }.toSet()) }
    fun toggle(slug: String) { kept = if (slug in kept) kept - slug else kept + slug }

    AlertDialog(
        // Back / outside tap accepts the current (default-all) selection so the
        // dialog doesn't get stuck; the choice is then remembered.
        onDismissRequest = { onConfirm(kept) },
        title = { Text(stringResource(R.string.areapicker_title)) },
        text = {
            Column(
                Modifier.verticalScroll(rememberScrollState()),
                verticalArrangement = Arrangement.spacedBy(2.dp),
            ) {
                Text(
                    stringResource(R.string.areapicker_subtitle),
                    color = TextSecondary, fontSize = 13.sp,
                    modifier = Modifier.padding(bottom = 8.dp),
                )
                catalog.areas.forEach { area ->
                    Row(
                        Modifier
                            .fillMaxWidth()
                            .clickable { toggle(area.slug) }
                            .testTag("areapicker.area.${area.slug}"),
                        verticalAlignment = Alignment.CenterVertically,
                    ) {
                        Checkbox(checked = area.slug in kept, onCheckedChange = { toggle(area.slug) })
                        Text(area.name, fontSize = 15.sp, fontWeight = FontWeight.Medium, modifier = Modifier.weight(1f))
                        Text("${area.cinemas.size}", color = TextSecondary, fontSize = 12.sp)
                    }
                }
            }
        },
        confirmButton = {
            TextButton(onClick = { onConfirm(kept) }, modifier = Modifier.testTag("areapicker.confirm")) {
                Text(stringResource(R.string.areapicker_confirm))
            }
        },
    )
}
