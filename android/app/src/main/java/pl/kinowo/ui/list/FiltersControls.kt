package pl.kinowo.ui.list

import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.DragInteraction
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.IntrinsicSize
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.Checkbox
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.SegmentedButton
import androidx.compose.material3.SegmentedButtonDefaults
import androidx.compose.material3.SingleChoiceSegmentedButtonRow
import androidx.compose.material3.Surface
import androidx.compose.material3.Switch
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.input.nestedscroll.NestedScrollConnection
import androidx.compose.ui.input.nestedscroll.NestedScrollSource
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.IntRect
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.LayoutDirection
import androidx.compose.ui.unit.Velocity
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Popup
import androidx.compose.ui.window.PopupPositionProvider
import androidx.compose.ui.window.PopupProperties
import pl.kinowo.R
import pl.kinowo.filter.FormatFilter
import pl.kinowo.model.selected

@Composable
internal fun FilterSectionLabel(text: String) {
    Text(text, fontWeight = FontWeight.SemiBold, modifier = Modifier.padding(top = 8.dp, bottom = 2.dp))
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
internal fun <T> SegmentedChoice(options: List<Pair<String, T>>, selected: T, onSelect: (T) -> Unit) {
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
internal fun ToggleRow(label: String, checked: Boolean, onChange: (Boolean) -> Unit) {
    Row(Modifier.fillMaxWidth().padding(vertical = 2.dp), verticalAlignment = Alignment.CenterVertically) {
        Text(label, modifier = Modifier.weight(1f))
        Switch(checked = checked, onCheckedChange = onChange)
    }
}

@Composable
internal fun CheckRow(label: String, checked: Boolean, onChange: (Boolean) -> Unit) {
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
internal fun FromHourRow(filter: FormatFilter, onChange: (FormatFilter) -> Unit) {
    val hours = listOf(-1) + (0..23).toList()
    var hourExpanded by remember { mutableStateOf(false) }
    var minExpanded by remember { mutableStateOf(false) }
    Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.spacedBy(8.dp), verticalAlignment = Alignment.CenterVertically) {
        Dropdown(
            label = if (filter.fromHour < 0) stringResource(R.string.any) else "%02d".format(filter.fromHour),
            expanded = hourExpanded,
            onExpandedChange = { hourExpanded = it },
            items = hours.map { (if (it < 0) stringResource(R.string.any) else "%02d".format(it)) to it },
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
internal fun <T> Dropdown(
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
                if (expanded) painterResource(R.drawable.ic_expand_less) else painterResource(R.drawable.ic_expand_more),
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
