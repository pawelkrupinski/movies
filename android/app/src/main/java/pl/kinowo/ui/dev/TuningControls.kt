package pl.kinowo.ui.dev

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Slider
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.ui.common.RatingBadgeMetrics
import pl.kinowo.ui.theme.Brand
import pl.kinowo.ui.theme.TextSecondary

/** Shows this device's pill scale and the resulting rendered font, the key
 *  difference from iOS — Android sizes rating pills relative to viewport width,
 *  so the same base sp renders differently per phone. */
@Composable
internal fun RatingScaleReadout(baseFontSize: TextUnit) {
    val screenWidthDp = LocalConfiguration.current.screenWidthDp
    val scale = RatingBadgeMetrics.scale(screenWidthDp)
    val rendered = baseFontSize.value * scale
    Text(
        "skala ×${twoDecimal(scale)} @${RatingBadgeMetrics.ReferenceWidthDp.toInt()}dp → ${oneDecimal(rendered)}sp",
        color = TextSecondary,
        fontSize = 11.sp,
    )
}

@Composable
internal fun Group(title: String, content: @Composable () -> Unit) {
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        Text(
            title.uppercase(),
            color = TextSecondary,
            fontSize = 10.sp,
            fontWeight = FontWeight.SemiBold,
        )
        content()
    }
}

/** The weight choices the picker offers, in ascending order, with their copy
 *  keyword (`copyText`) and short label for the segmented control. */
internal enum class WeightOption(val short: String, val key: String, val weight: FontWeight) {
    Regular("Reg", "regular", FontWeight.Normal),
    Medium("Med", "medium", FontWeight.Medium),
    SemiBold("Semi", "semibold", FontWeight.SemiBold),
    Bold("Bold", "bold", FontWeight.Bold),
    Black("Black", "black", FontWeight.Black);

    companion object {
        fun of(w: FontWeight): WeightOption = entries.firstOrNull { it.weight == w } ?: Medium
    }
}

@Composable
internal fun WeightRow(label: String, weight: FontWeight, onSelect: (FontWeight) -> Unit) {
    Row(verticalAlignment = Alignment.CenterVertically) {
        Text(label, color = Color.White, fontSize = 12.sp, modifier = Modifier.width(110.dp))
        val current = WeightOption.of(weight)
        Row(
            Modifier
                .weight(1f)
                .clip(RoundedCornerShape(8.dp))
                .background(Color.White.copy(alpha = 0.06f)),
        ) {
            for (opt in WeightOption.entries) {
                val selected = opt == current
                Box(
                    Modifier
                        .weight(1f)
                        .clip(RoundedCornerShape(8.dp))
                        .background(if (selected) Brand.copy(alpha = 0.85f) else Color.Transparent)
                        .clickable(
                            interactionSource = remember { MutableInteractionSource() },
                            indication = null,
                        ) { onSelect(opt.weight) }
                        .padding(vertical = 7.dp),
                    contentAlignment = Alignment.Center,
                ) {
                    Text(opt.short, color = Color.White, fontSize = 11.sp, fontWeight = if (selected) FontWeight.SemiBold else FontWeight.Normal)
                }
            }
        }
    }
}

@Composable
internal fun SpSlider(label: String, value: Float, range: ClosedFloatingPointRange<Float>, tag: String? = null, onChange: (Float) -> Unit) {
    LabeledSlider(label, value, range, tag) { onChange(it) }
}

@Composable
internal fun DpSlider(label: String, value: Dp, range: ClosedFloatingPointRange<Float>, tag: String? = null, onChange: (Dp) -> Unit) {
    LabeledSlider(label, value.value, range, tag) { onChange(it.dp) }
}

@Composable
internal fun LabeledSlider(
    label: String,
    value: Float,
    range: ClosedFloatingPointRange<Float>,
    tag: String? = null,
    onChange: (Float) -> Unit,
) {
    val step = 0.5f
    // Snap to 0.5 steps to match the iOS slider granularity.
    val steps = ((range.endInclusive - range.start) / step).toInt() - 1
    Row(verticalAlignment = Alignment.CenterVertically) {
        Text(label, color = Color.White, fontSize = 12.sp, modifier = Modifier.width(96.dp))
        StepButton("−") { onChange((value - step).coerceIn(range.start, range.endInclusive)) }
        Slider(
            value = value,
            onValueChange = { onChange((Math.round(it * 2f) / 2f)) },
            valueRange = range,
            steps = steps.coerceAtLeast(0),
            modifier = Modifier.weight(1f).then(if (tag != null) Modifier.testTag(tag) else Modifier),
        )
        StepButton("+") { onChange((value + step).coerceIn(range.start, range.endInclusive)) }
        Text(
            oneDecimal(value),
            color = Color.White,
            fontSize = 12.sp,
            fontWeight = FontWeight.Medium,
            modifier = Modifier.width(36.dp),
        )
    }
}

/** Small −/+ button flanking a slider, nudging it by one 0.5 step (clamped to
 *  the slider's range) for precise dialling without dragging. */
@Composable
internal fun StepButton(symbol: String, onClick: () -> Unit) {
    Box(
        Modifier
            .size(26.dp)
            .clip(RoundedCornerShape(5.dp))
            .background(Color.White.copy(alpha = 0.08f))
            .clickable(onClick = onClick),
        contentAlignment = Alignment.Center,
    ) {
        Text(symbol, color = Color.White, fontSize = 15.sp, fontWeight = FontWeight.Bold)
    }
}
