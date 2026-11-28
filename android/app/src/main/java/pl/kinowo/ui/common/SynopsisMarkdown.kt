package pl.kinowo.ui.common

import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.withStyle

/**
 * The synopsis arrives over `/api/details` carrying a tiny markdown subset —
 * `**bold**`, `*italic*` (and `***both***`) — plus `\n`/`\n\n` paragraph breaks,
 * emitted server-side by `SynopsisProse.blockText`. Compose has no markdown
 * renderer, so parse the inline emphasis into an [AnnotatedString] with
 * bold/italic spans; newlines pass through untouched (the `Text` renders them).
 *
 * The markers are always balanced pairs as emitted by the scraper, so a simple
 * toggle scan is enough: `**` flips bold, `*` flips italic, `***` flips both.
 */
object SynopsisMarkdown {
    fun annotated(s: String): AnnotatedString = buildAnnotatedString {
        for (segment in segments(s)) appendSegment(segment)
    }

    /** A run of text under one emphasis state. */
    private class Segment(val text: String, val bold: Boolean, val italic: Boolean)

    private fun segments(s: String): List<Segment> {
        val out = mutableListOf<Segment>()
        var bold = false
        var italic = false
        val buf = StringBuilder()

        fun flush() {
            if (buf.isEmpty()) return
            out += Segment(buf.toString(), bold, italic)
            buf.setLength(0)
        }

        var i = 0
        while (i < s.length) {
            when {
                s.startsWith("**", i) -> { flush(); bold = !bold; i += 2 }
                s[i] == '*'           -> { flush(); italic = !italic; i += 1 }
                else                  -> { buf.append(s[i]); i += 1 }
            }
        }
        flush()
        return out
    }

    private fun AnnotatedString.Builder.appendSegment(segment: Segment) {
        if (segment.bold || segment.italic) {
            withStyle(
                SpanStyle(
                    fontWeight = if (segment.bold) FontWeight.Bold else null,
                    fontStyle = if (segment.italic) FontStyle.Italic else null,
                )
            ) { append(segment.text) }
        } else {
            append(segment.text)
        }
    }
}
