# Spanish message bundle. Selected when the deployment's country language is
# Spanish (KINOWO_COUNTRY=es). Poland renders from the default `messages`.

# ── Navbar / filters ────────────────────────────────────────────────────────
nav.filters=Filtros
nav.hiddenFilms=Películas ocultas
nav.sort=Ordenar
sort.earliest=Próxima sesión
sort.rating=Valoración
nav.cinemas=Cines
nav.allCinemas=Todos los cines
nav.country=País
areaPicker.title=Elige tus zonas
areaPicker.subtitle=Muestra solo los cines de las zonas que elijas. Puedes cambiarlo cuando quieras en Filtros.
areaPicker.all=Todas las zonas
areaPicker.confirm=Ver la cartelera
nav.genre=Género
nav.director=Director
nav.cast=Reparto
nav.rooms=Salas
nav.dimension=Formato
nav.all=Todo
nav.version=Versión
nav.subtitles=Subtítulos
nav.dubbing=Doblaje
nav.imaxOnly=Solo IMAX
nav.fromHour=Desde
nav.any=Cualquiera
nav.city=Ciudad
nav.locateMe=Usar mi ubicación
nav.language=Idioma

country.pl=Polonia
country.uk=Reino Unido
country.de=Alemania
country.us=Estados Unidos
country.es=España
nav.clear=Borrar
nav.copyLink=Copiar enlace
nav.copied=¡Copiado!
nav.search=Buscar…
nav.filterByDate=Filtrar por fecha
brand.title=Cartelera de cine en {0}

# ── Day pills ───────────────────────────────────────────────────────────────
day.today=Hoy
day.tomorrow=Mañana
day.week=7 días
day.all=Todo

# ── Auth ────────────────────────────────────────────────────────────────────
auth.login=Iniciar sesión
auth.loginLong=Iniciar sesión
auth.logout=Cerrar sesión
auth.ssoLogoutConfirm.title=¿Cerrar sesión aquí también?
auth.ssoLogoutConfirm.body=Has cerrado sesión en nuestro otro sitio, pero aquí sigues con la sesión iniciada.
auth.ssoLogoutConfirm.signOut=Cerrar sesión aquí también
auth.ssoLogoutConfirm.stay=Mantener la sesión
auth.account=Cuenta
auth.close=Cerrar
login.title=Inicia sesión
login.sub=Tus películas favoritas y ocultas se sincronizarán entre tus dispositivos.
login.google=Continuar con Google
login.facebook=Continuar con Facebook
login.nag=Estas <strong>películas favoritas y ocultas</strong> se guardan solo en este dispositivo. <strong>Inicia sesión</strong> para sincronizarlas.

# ── Hidden-films modal ──────────────────────────────────────────────────────
hidden.title=Películas ocultas

# ── Empty states ────────────────────────────────────────────────────────────
empty.repertoire=No hay cartelera.
empty.films=No hay películas.

# ── Seasonal venues (open-air, drive-in) ─────────────────────────────────────
# The filter panel second line "back in season · May–Sept", and the empty listing
# when only out-of-season venues are selected: "Out of season: {0} (May–Sept)".
season.back=vuelve en temporada
season.outOfSeason=Fuera de temporada: {0}
season.month.1=ene
season.month.2=feb
season.month.3=mar
season.month.4=abr
season.month.5=may
season.month.6=jun
season.month.7=jul
season.month.8=ago
season.month.9=sept
season.month.10=oct
season.month.11=nov
season.month.12=dic

# ── Browse (filtered film list) ─────────────────────────────────────────────
browse.htmlTitle={0} – Cartelera de cine en {1}
browse.ogTitle={0} — Cartelera de cine en {1}
browse.ogDescription=Películas en {0} cines: {1}. Con las sesiones de hoy, valoraciones y sinopsis.
browse.ogDescriptionNear=Películas en los cines {0}: {1}. Con las sesiones de hoy, valoraciones y sinopsis.

# ── Landing (city picker) ───────────────────────────────────────────────────
landing.title={0} — cartelera de cine en tu ciudad
landing.ogDescription=La cartelera de todos los cines en un mismo sitio, con valoraciones de IMDb, Rotten Tomatoes y Metacritic. Consulta las sesiones de hoy en tu ciudad.
landing.apexTitle={0} — cartelera de cine en tu país
landing.chooseCountry=Elige tu país
landing.chooseCity=Elige tu ciudad
landing.searchCity=Busca una ciudad…
landing.searchCityAria=Buscar una ciudad
landing.searchLevels1=Busca: {0} o ciudad…
landing.searchLevels2=Busca: {0}, {1} o ciudad…
landing.noCity=No hay ninguna ciudad con ese nombre.
landing.locating=Buscando tu ubicación…
landing.noNearby=No hay ninguna ciudad disponible cerca: elige una de la lista.
landing.loading=Cargando…
landing.loadError=No se pudieron cargar las ciudades: toca para intentarlo de nuevo.
landing.level.uk.region=nación
landing.level.uk.subregion=condado
landing.level.us.region=estado
landing.level.de.region=estado federado
landing.level.pl.region=voivodato
landing.level.es.region=provincia


# ── Poster / detail labels ──────────────────────────────────────────────────
poster.missing=Sin cartel
poster.close=Cerrar cartel
detail.synopsis=Sinopsis
detail.director=Director
detail.cast=Reparto
detail.countries=Países
detail.trailers=Tráileres
detail.trailer=Tráiler {0}
detail.trailerTitle=Tráiler: {0}
detail.showings=Sesiones
detail.moreCinemas=Ver {0} cines más
detail.otherCities=En otras ciudades
detail.otherCitiesTitle={0} – en otras ciudades
card.hide=Ocultar película
# {0} = genre name — the tap-to-filter tooltip on a card's genre pill.
card.genreFilter=Películas de {0}

# ── Film page (meta) ────────────────────────────────────────────────────────
# `<title>` + og tail, appended after "<film> (<year>)". {0} = city nominative,
# {1} = brand ("Kinowo" in PL, "Showtimes" elsewhere).
film.titleSuffix=– sesiones en {0} | {1}

# ── Film page back-nav ──────────────────────────────────────────────────────
back.back=Atrás
back.repertoire=Cartelera

# ── First-run swipe hint ────────────────────────────────────────────────────
hint.swipe=Desliza para cambiar de día

# ── App promotion banner ──────────────────────────────────────────────────────
# {0} = brand ("Kinowo" in PL, "Showtimes" elsewhere) — see Country.brandName.
appBanner.aria=Promoción de la app {0}
appBanner.headline={0} — app móvil
appBanner.subtitle=Más rápido, sin navegador
appBanner.appStoreLine1=Disponible en el
appBanner.playLine1=Disponible en
appBanner.close=Cerrar

# ── Errors ──────────────────────────────────────────────────────────────────
error.unknownCity=Ciudad desconocida: {0}

# A rating-less rating pill (a link, no score yet): its title and aria-label.
rating.imdb.pending=IMDb – sin valoración todavía
rating.filmweb.pending=Filmweb – sin valoración todavía
