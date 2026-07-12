package pl.kinowo.ui.list

import androidx.compose.animation.animateContentSize
import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.ExpandMore
import androidx.compose.material3.Checkbox
import androidx.compose.material3.Icon
import androidx.compose.material3.Text
import androidx.compose.material3.TriStateCheckbox
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.rotate
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.state.ToggleableState
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.R
import pl.kinowo.filter.CinemaSection
import pl.kinowo.model.CinemaCatalog
import pl.kinowo.ui.theme.TextSecondary

/**
 * Multi-select cinema picker for SPLIT cities (e.g. London). A slim handle, like
 * [CinemaPillBar]'s, unfolds into an "all cinemas" master over collapsible AREA
 * groups — each area a (de)select checkbox plus a fold revealing its cinemas'
 * own checkboxes. Mirrors the web's area-grouped Filtry list and iOS's
 * `CinemaAreaBar`. Selection lives in the excluded [disabled] set; this
 * composable renders from it and calls back to mutate. Flat cities keep
 * [CinemaPillBar].
 */
@Composable
internal fun CinemaAreaBar(
    catalog: CinemaCatalog,
    disabled: Set<String>,
    onSetCinema: (cinema: String, enabled: Boolean) -> Unit,
    onSetArea: (cinemas: List<String>, enabled: Boolean) -> Unit,
    onSetAll: (cityCinemas: List<String>, enabled: Boolean) -> Unit,
) {
    if (catalog.cinemas.isEmpty()) return

    var expanded by rememberSaveable { mutableStateOf(false) }
    // Which area folds are open (reset on config change is fine — it's transient).
    var openAreas by remember { mutableStateOf(setOf<String>()) }
    val chevronRotation by animateFloatAsState(if (expanded) 180f else 0f, label = "areaChevron")

    val enabledCount = catalog.cinemas.count { it !in disabled }
    val handleLabel =
        if (enabledCount == catalog.cinemas.size) stringResource(R.string.areabar_all_cinemas)
        else stringResource(R.string.areabar_count, enabledCount, catalog.cinemas.size)

    fun state(of: List<String>): ToggleableState {
        val off = of.count { it in disabled }
        return when {
            off == 0 -> ToggleableState.On
            off == of.size -> ToggleableState.Off
            else -> ToggleableState.Indeterminate
        }
    }

    Column(Modifier.fillMaxWidth().animateContentSize()) {
        Row(
            Modifier
                .fillMaxWidth()
                .clickable(
                    interactionSource = remember { MutableInteractionSource() },
                    indication = null,
                ) { expanded = !expanded }
                .testTag("areabar.handle")
                .padding(start = 14.dp, end = 8.dp, top = 4.dp, bottom = 4.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Text(
                text = handleLabel,
                color = TextSecondary,
                fontSize = 13.sp,
                fontWeight = FontWeight.Medium,
                maxLines = 1,
                modifier = Modifier.weight(1f),
            )
            Icon(
                Icons.Filled.ExpandMore,
                contentDescription = stringResource(if (expanded) R.string.areabar_collapse else R.string.areabar_expand),
                tint = TextSecondary,
                modifier = Modifier.size(20.dp).rotate(chevronRotation),
            )
        }

        if (expanded) {
            Column(
                Modifier
                    .fillMaxWidth()
                    .heightIn(max = 320.dp)
                    .verticalScroll(rememberScrollState())
                    .padding(horizontal = 8.dp, vertical = 4.dp),
            ) {
                // "All cinemas" master.
                val allState = state(catalog.cinemas)
                Row(verticalAlignment = Alignment.CenterVertically) {
                    TriStateCheckbox(
                        state = allState,
                        onClick = { onSetAll(catalog.cinemas, allState != ToggleableState.On) },
                        modifier = Modifier.testTag("areabar.all"),
                    )
                    Text(stringResource(R.string.areabar_all_cinemas), fontSize = 15.sp, fontWeight = FontWeight.SemiBold)
                }

                catalog.areas.forEach { area ->
                    val areaState = state(area.cinemas)
                    val open = area.slug in openAreas
                    Row(verticalAlignment = Alignment.CenterVertically, modifier = Modifier.fillMaxWidth()) {
                        TriStateCheckbox(
                            state = areaState,
                            onClick = { onSetArea(area.cinemas, areaState != ToggleableState.On) },
                            modifier = Modifier.testTag("areabar.area.${area.slug}"),
                        )
                        Row(
                            Modifier
                                .weight(1f)
                                .clickable(
                                    interactionSource = remember { MutableInteractionSource() },
                                    indication = null,
                                ) { openAreas = if (open) openAreas - area.slug else openAreas + area.slug }
                                .testTag("areabar.header.${area.slug}")
                                .padding(vertical = 8.dp),
                            verticalAlignment = Alignment.CenterVertically,
                            horizontalArrangement = Arrangement.spacedBy(6.dp),
                        ) {
                            Text(area.name, fontSize = 15.sp, fontWeight = FontWeight.Medium, modifier = Modifier.weight(1f))
                            Text("${area.cinemas.size}", color = TextSecondary, fontSize = 12.sp)
                            Icon(
                                Icons.Filled.ExpandMore,
                                contentDescription = null,
                                tint = TextSecondary,
                                modifier = Modifier.size(18.dp).rotate(if (open) 180f else 0f),
                            )
                        }
                    }

                    if (open) {
                        area.cinemas.forEach { cinema ->
                            Row(
                                Modifier.fillMaxWidth().padding(start = 32.dp),
                                verticalAlignment = Alignment.CenterVertically,
                            ) {
                                Checkbox(
                                    checked = cinema !in disabled,
                                    onCheckedChange = { onSetCinema(cinema, it) },
                                    modifier = Modifier.testTag("areabar.cinema.$cinema"),
                                )
                                Text(CinemaSection.pillName(cinema), fontSize = 15.sp)
                            }
                        }
                    }
                }
            }
        }
    }
}
