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
 * emitted server-side by `ScraperParse.blockText`. Compose has no markdown
 * renderer, so parse the inline emphasis into an [AnnotatedString] with
 * bold/italic spans; newlines pass through untouched (the `Text` renders them).
 *
 * The markers are always balanced pairs as emitted by the scraper, so a simple
 * toggle scan is enough: `**` flips bold, `*` flips italic, `***` flips both.
 */
object SynopsisMarkdown {
    fun annotated(s: String): AnnotatedString = buildAnnotatedString {
        var bold = false
        var italic = false
        val buf = StringBuilder()

        fun flush() {
            if (buf.isEmpty()) return
            if (bold || italic) {
                withStyle(
                    SpanStyle(
                        fontWeight = if (bold) FontWeight.Bold else null,
                        fontStyle = if (italic) FontStyle.Italic else null,
                    )
                ) { append(buf.toString()) }
            } else {
                append(buf.toString())
            }
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
    }
}
