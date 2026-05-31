package pl.kinowo.ui.detail

import android.annotation.SuppressLint
import android.webkit.WebView
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.aspectRatio
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.material.icons.filled.OpenInNew
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.TopAppBarDefaults
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.viewinterop.AndroidView
import pl.kinowo.model.Film
import pl.kinowo.ui.common.PosterImage
import pl.kinowo.ui.common.RatingBadges
import pl.kinowo.ui.common.Showings
import pl.kinowo.ui.common.openUrl
import pl.kinowo.ui.theme.Brand
import pl.kinowo.ui.theme.CardElevated
import pl.kinowo.ui.theme.CinemaBlue
import pl.kinowo.ui.theme.TextSecondary

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class)
@Composable
fun DetailScreen(film: Film?, onBack: () -> Unit) {
    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text(film?.title ?: "", maxLines = 1, fontSize = 18.sp) },
                navigationIcon = {
                    IconButton(onClick = onBack) {
                        Icon(Icons.AutoMirrored.Filled.ArrowBack, contentDescription = "Wstecz")
                    }
                },
                colors = TopAppBarDefaults.topAppBarColors(),
            )
        },
    ) { padding ->
        if (film == null) {
            Box(Modifier.fillMaxSize().padding(padding), contentAlignment = Alignment.Center) {
                Text("Nie znaleziono filmu.", color = TextSecondary)
            }
            return@Scaffold
        }
        Column(
            Modifier.fillMaxSize().padding(padding).verticalScroll(rememberScrollState()).padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(16.dp),
        ) {
            // Header: poster + title/ratings.
            Row(horizontalArrangement = Arrangement.spacedBy(12.dp)) {
                PosterImage(
                    chain = film.posterChain,
                    contentDescription = film.title,
                    modifier = Modifier.width(150.dp).aspectRatio(2f / 3f).clip(RoundedCornerShape(10.dp)),
                )
                Column(Modifier.weight(1f), verticalArrangement = Arrangement.spacedBy(8.dp)) {
                    Text(film.title, fontSize = 20.sp, fontWeight = FontWeight.Bold, color = Color.White)
                    film.runtimeMinutes?.let {
                        Text(pl.kinowo.ui.list.formatRuntime(it), color = TextSecondary, fontSize = 13.sp)
                    }
                    RatingBadges(film.ratings)
                }
            }

            // Cinema links.
            if (film.cinemaLinks.isNotEmpty()) {
                FlowRow(horizontalArrangement = Arrangement.spacedBy(8.dp), verticalArrangement = Arrangement.spacedBy(8.dp)) {
                    val context = LocalContext.current
                    for (link in film.cinemaLinks) {
                        Row(
                            Modifier
                                .clip(RoundedCornerShape(8.dp))
                                .background(CardElevated)
                                .clickable { openUrl(context, link.url) }
                                .padding(horizontal = 10.dp, vertical = 6.dp),
                            verticalAlignment = Alignment.CenterVertically,
                            horizontalArrangement = Arrangement.spacedBy(4.dp),
                        ) {
                            Text(link.cinema, color = CinemaBlue, fontSize = 13.sp, fontWeight = FontWeight.Medium)
                            Icon(Icons.Filled.OpenInNew, contentDescription = null, tint = CinemaBlue, modifier = Modifier.size(13.dp))
                        }
                    }
                }
            }

            // Meta blocks.
            MetaBlock("Opis", film.synopsis)
            MetaBlock("Reżyseria", film.directors.joinToString(", ").ifEmpty { null })
            MetaBlock("Obsada", film.cast.joinToString(", ").ifEmpty { null })

            // Trailers.
            if (film.trailerURLs.isNotEmpty()) {
                TrailerSection(film.trailerURLs)
            }

            // Seanse (full).
            if (film.showings.isNotEmpty()) {
                Column(verticalArrangement = Arrangement.spacedBy(6.dp)) {
                    Text("Seanse", fontWeight = FontWeight.SemiBold, color = Color.White)
                    Showings(film = film, showCinemaHeaders = true, maxChips = null)
                }
            }
        }
    }
}

@Composable
private fun MetaBlock(label: String, value: String?) {
    if (value.isNullOrBlank()) return
    Column(verticalArrangement = Arrangement.spacedBy(2.dp)) {
        Text(label.uppercase(), color = TextSecondary, fontSize = 11.sp, fontWeight = FontWeight.SemiBold)
        Text(value, color = Color.White, fontSize = 14.sp, lineHeight = 20.sp)
    }
}

@Composable
private fun TrailerSection(trailers: List<String>) {
    var selected by remember(trailers) { mutableIntStateOf(0) }
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        Text("Zwiastuny", color = TextSecondary, fontSize = 11.sp, fontWeight = FontWeight.SemiBold)
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
            trailers.forEachIndexed { i, _ ->
                val active = i == selected
                Text(
                    "Zwiastun ${i + 1}",
                    color = if (active) Color.Black else CinemaBlue,
                    fontSize = 13.sp,
                    fontWeight = FontWeight.Medium,
                    modifier = Modifier
                        .clip(RoundedCornerShape(8.dp))
                        .background(if (active) Brand else CardElevated)
                        .clickable { selected = i }
                        .padding(horizontal = 12.dp, vertical = 6.dp),
                )
            }
        }
        TrailerPlayer(trailers[selected])
    }
}

@SuppressLint("SetJavaScriptEnabled")
@Composable
private fun TrailerPlayer(embedUrl: String) {
    val html = remember(embedUrl) { embedHtml(embedUrl) }
    AndroidView(
        modifier = Modifier.fillMaxWidth().aspectRatio(16f / 9f).clip(RoundedCornerShape(8.dp)),
        factory = { ctx ->
            WebView(ctx).apply {
                settings.javaScriptEnabled = true
                settings.mediaPlaybackRequiresUserGesture = false
                settings.domStorageEnabled = true
                setBackgroundColor(android.graphics.Color.BLACK)
            }
        },
        update = { it.loadDataWithBaseURL("https://www.youtube.com", html, "text/html", "utf-8", null) },
    )
}

/** Minimal full-bleed iframe page for a YouTube/Vimeo embed URL. */
private fun embedHtml(url: String): String {
    val src = if (url.contains("autoplay=")) url
    else url + (if (url.contains("?")) "&" else "?") + "autoplay=1&playsinline=1"
    val safe = src.replace("&", "&amp;").replace("\"", "&quot;")
    return """
        <!DOCTYPE html><html><head>
        <meta name="viewport" content="width=device-width,initial-scale=1">
        <style>*{margin:0;padding:0;overflow:hidden}body{background:#000}
        iframe{width:100%;height:100%;position:absolute;top:0;left:0;border:0}</style>
        </head><body>
        <iframe src="$safe" allow="autoplay; encrypted-media; picture-in-picture" allowfullscreen></iframe>
        </body></html>
    """.trimIndent()
}
