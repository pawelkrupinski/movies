import re, unicodedata
def fold(s):
    s = unicodedata.normalize('NFD', s); s = ''.join(c for c in s if unicodedata.category(c) != 'Mn')
    return s.replace('ł','l').replace('Ł','L').lower()

COUNTRY_LANG = {'pl': 'pl', 'de': 'de', 'uk': 'en', 'us': 'en', 'es': 'es'}
LANG_NAMES = {'hindi':'hi','tamil':'ta','telugu':'te','malayalam':'ml','kannada':'kn','punjabi':'pa','mandarin':'zh',
              'cantonese':'cn','japanese':'ja','korean':'ko','polish':'pl','spanish':'es','turkish':'tr','bengali':'bn',
              'gujarati':'gu','marathi':'mr','urdu':'ur','french':'fr','italian':'it','german':'de','portuguese':'pt'}

# token -> (kind) ; kinds: 'notLocal' (original is not the venue country's language), 'notEn', 'foreign'
FORMAT_TOKENS = {
  'pl': {'nap':'notLocal','napisy':'notLocal','dub':'notLocal','dubbing':'notLocal','lek':'notLocal','lec':'notLocal',
         'lektor':'notLocal','org':'notLocal','oryginalny':'notLocal','|org':'notLocal','sub':'notLocal'},
  'de': {'omu':'notLocal','ov':'notLocal','omeu':'notEn','omdu':'notLocal'},
  'uk': {'subtitled':'notLocal','sub':'notLocal','subbed':'notLocal','dub':'notLocal'},
  'us': {'sub':'notLocal','dub':'notLocal'},
  'es': {'vose':'notLocal','vo':'notLocal','dob':'notLocal'},
}
TITLE_PATTERNS = {
  'pl': [(r'\bnapisy\b','notLocal'),(r'\bdubbing\w*\b','notLocal'),(r'\blektor\w*\b','notLocal'),(r'wersj\w+ oryginaln\w+','notLocal')],
  'de': [(r'\bomu\b','notLocal'),(r'\bo\.m\.u\.?','notLocal'),(r'\bomdu\b','notLocal'),(r'\bomeu\b','notEn'),(r'\bov\b','notLocal'),
         (r'originalfassung|originalversion|original mit','notLocal'),(r'mit (deutschen )?untertitel','notLocal')],
  'uk': [(r'\bsubtitled\b','notLocal')],
  'us': [(r'\bsubtitled\b','notLocal')],
  'es': [(r'\bv\.?o\.?s\.?e\.?\b','notLocal'),(r'\bv\.o\.?\b|\(vo\)','notLocal')],
}

def language_constraint(cc, formats, raw_title, listing_genres=()):
    """-> (equals:set, forbidden:set, sources:set of tokens)"""
    local = COUNTRY_LANG[cc]; eq=set(); forb=set(); src=set()
    def apply(kind, tok):
        src.add(tok)
        if kind == 'notLocal': forb.add(local)
        elif kind == 'notEn': forb.add('en')
    for f in formats:
        t = fold(f).strip()
        if t in FORMAT_TOKENS.get(cc, {}): apply(FORMAT_TOKENS[cc][t], 'fmt:'+t)
        elif t in LANG_NAMES: eq.add(LANG_NAMES[t]); src.add('fmt:'+t)
    ft = fold(raw_title or '')
    for pat, kind in TITLE_PATTERNS.get(cc, []):
        if re.search(pat, ft): apply(kind, 'title:'+pat)
    for m in re.finditer(r'[\(\[]\s*([a-z]+)\s*[\)\]]', ft):
        if m.group(1) in LANG_NAMES: eq.add(LANG_NAMES[m.group(1)]); src.add('title:'+m.group(1))
    for g in listing_genres:
        if fold(g) == 'subtitled foreign language': forb.add(local); src.add('genre:subtitled')
    return eq, forb, src

def language_level(constraint, film_lang):
    eq, forb, _ = constraint
    if not eq and not forb: return 'unknown:listing'
    if not film_lang: return 'unknown:film'
    norm = {'cn':'zh'}
    fl = norm.get(film_lang, film_lang); eqn = {norm.get(e,e) for e in eq}
    if eqn: return 'match' if fl in eqn else 'contradicts'
    return 'contradicts' if fl in forb else 'match'

# listing genre -> TMDB genre id
GENRE_MAP = [
  (99,  r'dokument|documental|documentary|^doc$'),
  (16,  r'animow|animac|animat|zeichentrick|trickfilm|anime'),
  (10402, r'^music|^musik|concert|konzert|koncert|concierto|^musica|live performance'),
  (27,  r'horror|^terror'),
  (35,  r'^kom[eo]d|^comed'),
  (10751, r'famil|kids|dla dzieci|kinderfilm|infantil'),
  (878, r'sci-?fi|science|ciencia'),
]
KEY = {99, 16}
def listing_genres(genres):
    out=set(); known=False
    for g in genres:
        fg = fold(g).strip()
        if not fg or fg in ('unknown',): continue
        known = True
        for gid, pat in GENRE_MAP:
            if re.search(pat, fg): out.add(gid)
    return (out if known else None)

def genre_level(lg, film_genres):
    if lg is None: return 'unknown:listing'
    if not film_genres: return 'unknown:film'
    fk = set(film_genres) & KEY; lk = lg & KEY
    if lk and lk <= fk: return 'agree'
    if lk and not (lk & fk): return 'contradicts'          # venue says doc/anim, film is neither
    if fk and not lk: return 'contradicts:film'            # film doc/anim, venue lists genres without it
    return 'neutral'                                       # neither side doc/anim (or partial)
