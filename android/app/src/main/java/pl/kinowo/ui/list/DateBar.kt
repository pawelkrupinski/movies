package pl.kinowo.ui.list

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.RowScope
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.semantics.selected
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.R
import pl.kinowo.filter.DateFilter
import pl.kinowo.ui.TopBarLayout
import pl.kinowo.ui.common.labelText
import pl.kinowo.ui.theme.Brand
import pl.kinowo.ui.theme.TextSecondary

// The top-bar chrome: the 🎬 mark, the four date pills, the inline search
// field (wide screens only), and the Filtry button — all on one row. Extracted
// so DayPillFitTest can render it at a fixed viewport width and assert the day
// labels never clip. [filtersActive] tints the Filtry icon; [highlighted] drives
// which date pill reads as selected (see DatePills).
@Composable
internal fun DateBar(
    wide: Boolean,
    highlighted: DateFilter,
    filtersActive: Boolean,
    search: String,
    onSearch: (String) -> Unit,
    onSelect: (DateFilter) -> Unit,
    onOpenFilters: () -> Unit,
) {
    Row(
        Modifier.fillMaxWidth().padding(start = 10.dp, end = 4.dp, top = 8.dp),
        // Narrow phones tighten the inter-item gap so the weighted dated pills get
        // more of the row to fill, clearing their labels at 14sp (see DatePill).
        horizontalArrangement = Arrangement.spacedBy(if (wide) 6.dp else 4.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        // The decorative leading mark. The in-app country switcher lives in the
        // Filtry sheet (see FiltersSheet.CountrySection), beside the city picker —
        // keeping it off this row preserves the date pills' tight narrow-phone fit.
        Text("🎬", fontSize = 22.sp)
        // The date pills always spread to fill the row; on wide screens
        // the inline search field sits between them and Filtry, capped
        // at a fixed width rather than eating the leftover space.
        DatePills(wide, highlighted = highlighted, onSelect = onSelect)
        if (wide) {
            InlineSearchField(value = search, onValueChange = onSearch)
        }
        IconButton(onClick = onOpenFilters) {
            Icon(
                painterResource(R.drawable.ic_filter_list),
                contentDescription = stringResource(R.string.filters),
                tint = if (filtersActive) Brand else TextSecondary,
            )
        }
    }
}

// The four date presets, laid out inline in the top bar (mirroring iOS
// DatePillsRow). On wide screens all four pills share the row width equally
// via `weight` — including "Wszystkie" — so they read as one evenly-spaced
// segmented control. On narrow screens (portrait phones) only the three short
// pills (Dziś / Jutro / 7 dni) get weight while "Wszystkie" keeps its intrinsic
// width, so the dated pills fill all the width left between the 🎬 mark and the
// Filtry icon with no trailing slack. See TopBarLayout.datePillFillsRow.
// [highlighted] drives which pill reads as selected — it tracks viewModel.dateFilter at
// rest but flips to the swipe-preview day mid-drag (see ListScreen.previewDay).
// A TAP still sets viewModel.dateFilter directly, which becomes the new highlight.
@Composable
private fun RowScope.DatePills(wide: Boolean, highlighted: DateFilter, onSelect: (DateFilter) -> Unit) {
    for (preset in DateFilter.presets) {
        val fills = TopBarLayout.datePillFillsRow(preset == DateFilter.Anytime, wide)
        DatePill(
            label = preset.labelText(),
            selected = highlighted == preset,
            wide = wide,
            modifier = if (fills) Modifier.weight(1f) else Modifier,
        ) { onSelect(preset) }
    }
}

@Composable
private fun DatePill(label: String, selected: Boolean, wide: Boolean, modifier: Modifier = Modifier, onClick: () -> Unit) {
    Box(
        modifier
            .clip(CircleShape)
            .background(if (selected) Brand.copy(alpha = 0.85f) else Color.Transparent)
            .clickable(
                interactionSource = remember { MutableInteractionSource() },
                indication = null,
            ) { onClick() }
            // Which day is on, for TalkBack and for tests (`assertIsSelected`).
            .semantics { this.selected = selected }
            // Roomy 12dp inset on wide screens; narrow phones tighten to 6dp so
            // the three weighted dated pills, after filling the leftover width
            // between 🎬 and Filtry, still clear "Jutro" / "7 dni" at 14sp on a
            // 360dp phone (Galaxy S24) instead of clipping. Guarded by
            // DayPillFitTest.
            .padding(horizontal = if (wide) 12.dp else 6.dp, vertical = 7.dp),
        contentAlignment = Alignment.Center,
    ) {
        Text(
            label,
            fontSize = 14.sp,
            fontWeight = FontWeight.Medium,
            maxLines = 1,
            color = Color.White,
        )
    }
}
