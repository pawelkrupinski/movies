package pl.kinowo.ui.list

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.RowScope
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.imePadding
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material3.Icon
import androidx.compose.material3.LocalTextStyle
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import dev.chrisbanes.haze.HazeInput
import dev.chrisbanes.haze.HazePerformanceMode
import dev.chrisbanes.haze.HazeState
import dev.chrisbanes.haze.blur.HazeBlurStyle
import dev.chrisbanes.haze.blur.hazeBlur
import dev.chrisbanes.haze.hazeSource
import pl.kinowo.R
import pl.kinowo.ui.theme.Background
import pl.kinowo.ui.theme.Brand
import pl.kinowo.ui.theme.TextSecondary

// The pill's clear-glass blur. Enough blur to visibly distort the cards
// underneath, but no tint and no noise grain — so it reads as clear,
// see-through glass (like the iOS `.ultraThinMaterial`) rather than a frosty
// pane. The blur composites over the opaque app background. Below API 31
// (no RenderEffect) Haze skips the blur and, with no tint to fall back on, the
// pill is plain see-through glass. Hoisted: a HazeBlurStyle compares by identity.
private val SearchPillGlass = HazeBlurStyle {
    backgroundColor(Background)
    blurRadius(1.dp)
    noiseFactor(0f)
}

// Floating search pill, pinned bottom-centre and overlaid on the grid —
// the Android counterpart to the iOS `SearchBar` (SearchBar.swift). A
// translucent capsule with a leading magnifier, an inline text field, and
// a trailing clear glyph that appears once there's a query. `imePadding`
// (applied by the caller) lifts it above the soft keyboard on focus; the
// grids reserve `SearchBarBottomInset` so their last row clears the pill.
//
// Internal (not private) so FloatingSearchBarBlurTest can render it over a
// known backdrop and pixel-sample the blur.
@Composable
internal fun FloatingSearchBar(
    value: String,
    onValueChange: (String) -> Unit,
    hazeState: HazeState,
    modifier: Modifier = Modifier,
) {
    val shape = RoundedCornerShape(28.dp)
    Row(
        modifier
            .padding(horizontal = 24.dp, vertical = 14.dp)
            .fillMaxWidth()
            .testTag(FloatingSearchBarTag)
            .clip(shape)
            // Real frosted glass: blur the grid captured by `hazeSource`
            // (SearchPillGlass). The hairline white border catches the edge
            // against busy posters.
            .hazeBlur(
                input = HazeInput.Sources(hazeState),
                style = SearchPillGlass,
                // Full-resolution input: at a 1dp radius any downscaling of the
                // backdrop shows as pixelation (Haze 2's default samples at ~0.8×).
                performanceMode = HazePerformanceMode.Quality,
            )
            .border(1.dp, Color.White.copy(alpha = 0.22f), shape)
            .padding(horizontal = 18.dp, vertical = 13.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        SearchFieldContent(value, onValueChange)
    }
}

// Inline search on the top bar (wide screens): the shared field in a
// translucent capsule sized to the date pills, sitting between the pills and
// the Filtry button. Capped at a fixed width so it stays a comfortable
// type-into size and the date pills get the rest of the row. The Android
// counterpart to the iOS InlineSearchField.
@Composable
internal fun InlineSearchField(value: String, onValueChange: (String) -> Unit) {
    val shape = RoundedCornerShape(20.dp)
    Row(
        Modifier
            .width(240.dp)
            .clip(shape)
            .background(Color.White.copy(alpha = 0.08f))
            .padding(horizontal = 12.dp, vertical = 7.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        SearchFieldContent(value, onValueChange, compact = true)
    }
}

// The magnifier + text field + clear glyph shared by both search placements:
// the floating bottom FloatingSearchBar and the inline InlineSearchField. In
// compact mode (inline) the glyphs and text shrink to match the date-pill height.
@Composable
private fun RowScope.SearchFieldContent(value: String, onValueChange: (String) -> Unit, compact: Boolean = false) {
    val iconSize = if (compact) 16.dp else 20.dp
    val baseStyle = LocalTextStyle.current
    val textStyle = baseStyle.copy(
        color = Color.White,
        fontSize = if (compact) 14.sp else baseStyle.fontSize,
    )
    Icon(painterResource(R.drawable.ic_search), contentDescription = null, tint = TextSecondary, modifier = Modifier.size(iconSize))
    Spacer(Modifier.width(if (compact) 8.dp else 10.dp))
    Box(Modifier.weight(1f)) {
        if (value.isEmpty()) {
            Text(stringResource(R.string.search_films), color = TextSecondary, fontSize = textStyle.fontSize)
        }
        BasicTextField(
            value = value,
            onValueChange = onValueChange,
            singleLine = true,
            textStyle = textStyle,
            cursorBrush = SolidColor(Brand),
            keyboardOptions = KeyboardOptions(imeAction = ImeAction.Search),
            modifier = Modifier.fillMaxWidth(),
        )
    }
    if (value.isNotEmpty()) {
        Spacer(Modifier.width(8.dp))
        Icon(
            painterResource(R.drawable.ic_close),
            contentDescription = stringResource(R.string.clear),
            tint = TextSecondary,
            modifier = Modifier.size(iconSize).clickable { onValueChange("") },
        )
    }
}
