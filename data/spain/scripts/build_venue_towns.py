#!/usr/bin/env python3
"""Build data/spain/town-coords.json — the municipality every Spanish venue is in.

A Spanish page is a town or a cluster of towns (see `build_pages.py`), so each
venue needs a town that is RIGHT, and a position for it. SensaCine's own answer
is the section header its province listing files the venue under, which is
mostly the venue's town and sometimes not: "Estacion De Espiel" (Córdoba) heads
Huércal-Overa's municipal cinema (Almería), "Fraile" (Jaén) the Autocinema
Tenerife, "Grau I Platja" is a beach district of Gandia, "Maitino" a rural
partida of Elche.

So the town is the MUNICIPALITY, resolved from what the venue's own page says
(`venue-addresses.json`: postal code + locality; `ocine.json` for the venues
SensaCine does not list), against two GeoNames dumps:

  1. the locality or the header, matched to a municipality of the province the
     POSTAL CODE says (its first two digits are the INE province) — which is how
     La Garriga's cinema, filed under "Samalus", lands in La Garriga;
  2. failing that, the municipality the postal code itself belongs to, where
     GeoNames' postal dump names exactly one, or exactly one whose place name is
     the locality;
  3. failing both, `PINNED` below — a hand-checked answer per venue.

A municipality's position is its most populous populated place (the town
centre, not the municipal polygon's middle), its population the municipal one
(GeoNames ADM3) — which is what `build_pages.py` ranks anchors and majors by.

The output carries, per venue, its town; per town, its position, population and
province. Only that derived file is committed; the dumps are not:

    D=$(mktemp -d)
    curl -sL https://download.geonames.org/export/dump/ES.zip -o $D/ES.zip && unzip -o -q $D/ES.zip -d $D
    mkdir $D/zip && curl -sL https://download.geonames.org/export/zip/ES.zip -o $D/zip/ES.zip \\
        && unzip -o -q $D/zip/ES.zip -d $D/zip
    python3 data/spain/scripts/build_venue_towns.py $D/ES.txt $D/zip/ES.txt
"""
import collections
import json
import pathlib
import re
import sys
import unicodedata

ROOT = pathlib.Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "spain"
OUT = DATA / "town-coords.json"

sys.path.insert(0, str(ROOT / "data" / "scripts"))
import retired_venues  # noqa: E402
sys.path.insert(0, str(ROOT / "data" / "spain" / "scripts"))
from generate_roster import spanish_case  # noqa: E402

# INE province code (a postal code's first two digits) → the province as the
# roster names it.
PROVINCE_OF_INE = {
    "01": "Álava", "02": "Albacete", "03": "Alicante", "04": "Almería", "05": "Ávila", "06": "Badajoz",
    "07": "Islas Baleares", "08": "Barcelona", "09": "Burgos", "10": "Cáceres", "11": "Cádiz",
    "12": "Castellón", "13": "Ciudad Real", "14": "Córdoba", "15": "A Coruña", "16": "Cuenca",
    "17": "Girona", "18": "Granada", "19": "Guadalajara", "20": "Guipúzcoa", "21": "Huelva",
    "22": "Huesca", "23": "Jaén", "24": "León", "25": "Lérida", "26": "La Rioja", "27": "Lugo",
    "28": "Madrid", "29": "Málaga", "30": "Murcia", "31": "Navarra", "32": "Ourense", "33": "Asturias",
    "34": "Palencia", "35": "Las Palmas", "36": "Pontevedra", "37": "Salamanca",
    "38": "Santa Cruz de Tenerife", "39": "Cantabria", "40": "Segovia", "41": "Sevilla", "42": "Soria",
    "43": "Tarragona", "44": "Teruel", "45": "Toledo", "46": "Valencia", "47": "Valladolid",
    "48": "Vizcaya", "49": "Zamora", "50": "Zaragoza", "51": "Ceuta", "52": "Melilla",
}

# Venues whose header, locality AND postal code (as GeoNames files it) point at the
# wrong municipality, by displayName → the municipality, as GeoNames names it, in the
# postal code's province. Each checked by hand against the venue's street address.
PINNED = {
    "Gran Teatro de Villarrobledo": "Villarrobledo",        # header/locality "Albacete"; Calle de la Virgen 3, Villarrobledo
    "Cine Teatro Principal Requena": "Requena",              # header "Cofrentes"; Plaza Pascual Carrión 11, Requena
    "Cine Coria": "Coria",                                   # header "Guijo De Coria"; Calle Portezuelo 1, Coria
    "Teatro San Francisco": "Vejer de la Frontera",          # header "Manzanete"; postal 11150 is Vejer's
    "Cine Horadada": "Pilar de la Horadada",                 # header "Marina": Torre de la Horadada
    "Cine Las Villas": "Pilar de la Horadada",               # the same, GeoNames' postal dump says Orihuela
    "Teatro Fernández-Baldor": "Torrelodones",               # header "Fuente La Teja", a Torrelodones district
    "Nuevos Cines Cabos de Palos ": "Cartagena",             # Playa Honda / Cabo de Palos are Cartagena's
    "Cine Avenida Santo Domingo": "Santo Domingo de la Calzada",   # header "Sto Domingo De La Calzada"
    "Cines Van Dyck Tormes": "Santa Marta de Tormes",       # header "Salamanca"; C.C. El Tormes, 37900 Santa Marta
}

# How a municipality is SHOWN where GeoNames' name for it is not what a Spanish
# reader calls it and no venue's own town header supplies the Castilian form.
DISPLAY = {
    "41091": "Sevilla", "01059": "Vitoria-Gasteiz", "20069": "San Sebastián",
    "12040": "Castellón de la Plana", "33024": "Gijón", "08101": "L'Hospitalet de Llobregat",
    "01036": "Llodio",
}


def fold(s: str) -> str:
    s = unicodedata.normalize("NFKD", s)
    s = "".join(c for c in s if not unicodedata.combining(c))
    return " ".join(s.lower().replace("-", " ").replace("'", " ").replace("’", " ").split())


ARTICLES = ("el", "la", "los", "las", "l", "a", "o", "es", "s", "els", "les", "as", "os")


def uninvert(name: str) -> str:
    """GeoNames files some municipalities article-last: "Ejido, El" is El Ejido."""
    m = re.fullmatch(r"(.+), (\w+'?)", name)
    if m and fold(m[2]) in ARTICLES:
        sep = "" if m[2].endswith("'") else " "
        return f"{m[2]}{sep}{m[1]}"
    return name


def spellings(name: str, strip_article: bool = True) -> set[str]:
    """Every folded way a header may write a municipality: its name, each half of a
    bilingual one ("Gasteiz / Vitoria"), and each of those without its article —
    SensaCine heads El Masnou's cinema "Masnou" and l'Alfàs del Pi's "Alfas Del Pi"."""
    out = set()
    for part in [name] + re.split(r"\s*/\s*", name):
        part = fold(uninvert(part))
        out.add(part)
        head, _, rest = part.partition(" ")
        if strip_article and head in ARTICLES and rest:
            out.add(rest)
    return out


def load_municipalities(path):
    """INE code → {name, names (folded), pop, admin2, lat, lon} from the GeoNames dump."""
    munis, seats = {}, {}
    with open(path, encoding="utf-8") as f:
        for line in f:
            c = line.rstrip("\n").split("\t")
            if c[6] == "A" and c[7] == "ADM3" and c[12]:
                names, full = set(), set()
                for n in [c[1], c[2]] + [a for a in c[3].split(",") if a and not a.isdigit()]:
                    names |= spellings(n)
                    full |= spellings(n, strip_article=False)
                munis[c[12]] = {"name": uninvert(c[1]), "names": names, "full": full, "pop": int(c[14] or 0), "admin2": c[11],
                                "lat": float(c[4]), "lon": float(c[5])}
            elif c[6] == "P" and c[12]:
                pop = int(c[14] or 0)
                if c[12] not in seats or pop > seats[c[12]][0]:
                    seats[c[12]] = (pop, float(c[4]), float(c[5]))
    for code, m in munis.items():
        if code in seats and seats[code][0] > 0:
            m["lat"], m["lon"] = seats[code][1], seats[code][2]
    return munis


def load_postal(path):
    """Postal code → [(place name, INE municipality code)] from GeoNames' postal dump."""
    out = collections.defaultdict(list)
    with open(path, encoding="utf-8") as f:
        for line in f:
            c = line.rstrip("\n").split("\t")
            if c[8]:
                out[c[1]].append((c[2], c[8]))
    return out


def venues():
    """(displayName, harvested town, locality, postal code) for every rostered venue."""
    provinces = json.loads((DATA / "provinces.json").read_text())
    addresses = json.loads((DATA / "venue-addresses.json").read_text())
    retired = retired_venues.load(DATA)
    ocine = json.loads((DATA / "ocine.json").read_text())
    out = []
    for p in provinces:
        for c in p["cinemas"]:
            if c["theaterId"] in retired:
                continue
            a = addresses.get(c["theaterId"], {})
            out.append((c["displayName"], c["town"], a.get("addressLocality", ""), a.get("postalCode", "")))
    for u in ocine["unlisted"]:
        out.append((u["name"], u["town"], u["town"], u["postalCode"]))
    return out


def resolve(venue, munis, postal, corrections):
    """The venue's INE municipality code, and how it was found."""
    name, town, locality, code = venue
    code = code.zfill(5)
    province = PROVINCE_OF_INE.get(code[:2])
    in_province = {k: m for k, m in munis.items() if k[:2] == code[:2]}

    def named(label):
        hits = [k for k, m in in_province.items() if fold(label) in m["names"]]
        return hits[0] if len(hits) == 1 else None

    if name in PINNED:
        return named(PINNED[name]), "pinned"
    candidates = postal.get(code, [])
    codes = {ine for _, ine in candidates}
    # The locality and the header usually agree; where they name two different
    # municipalities, the one the postal code is in wins (Parque Astur: header
    # "Corvera de Asturias", locality "Aviles", postal code Corvera's).
    hits = list(dict.fromkeys(h for h in (named(lb) for lb in (locality, town, corrections.get(town, "")) if lb)
                              if h))
    if len(hits) > 1 and len([h for h in hits if h in codes]) == 1:
        return next(h for h in hits if h in codes), "name"
    if hits:
        return hits[0], "name"
    if len(codes) == 1:
        return codes.pop(), "postal"
    by_place = {ine for place, ine in candidates
                if fold(place).startswith(fold(locality or town))}
    if len(by_place) == 1:
        return by_place.pop(), "postal place"
    return None, f"unresolved (province {province}, postal municipalities {sorted(codes)})"


def build(venue_rows, munis, postal, corrections):
    towns, venue_town, problems, how = {}, {}, [], collections.Counter()
    shown = collections.defaultdict(collections.Counter)
    resolved = {}
    for v in venue_rows:
        ine, method = resolve(v, munis, postal, corrections)
        how[method.split(" (")[0]] += 1
        if ine is None or ine not in munis:
            problems.append(f"{v[0]!r} (town {v[1]!r}, locality {v[2]!r}, postal {v[3]!r}): {method}")
            continue
        resolved[v[0]] = ine
        # The venue's own header names the town the way Spanish writes it ("Alcoy",
        # not GeoNames' official "Alcoi") — when it names THIS municipality whole:
        # "Masnou" is El Masnou with its article dropped, not a name to show.
        header = spanish_case(corrections.get(v[1], v[1]))
        if fold(header) in munis[ine]["full"]:
            shown[ine][header] += 1
    for ine in set(resolved.values()):
        m = munis[ine]
        if ine in DISPLAY:
            label = DISPLAY[ine]
        elif any(fold(h) == fold(m["name"]) for h in shown[ine]):
            # The same name both ways: whichever spelling kept its accents.
            same = [h for h in shown[ine] if fold(h) == fold(m["name"])] + [m["name"]]
            label = max(same, key=lambda n: (sum(ord(ch) > 127 for ch in n), n == m["name"]))
        elif shown[ine]:
            label = max(shown[ine].items(), key=lambda kv: (kv[1], kv[0]))[0]
        else:
            label = m["name"]
        label = label[0].upper() + label[1:]
        if label in towns:
            problems.append(f"two municipalities shown as {label!r}: {towns[label]['ine']} and {ine}")
        towns[label] = {"ine": ine, "province": PROVINCE_OF_INE[ine[:2]], "lat": round(m["lat"], 5),
                        "lon": round(m["lon"], 5), "pop": m["pop"]}
    label_of = {t["ine"]: label for label, t in towns.items()}
    venue_town = {name: label_of[ine] for name, ine in resolved.items()}
    return towns, venue_town, problems, how


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    munis = load_municipalities(sys.argv[1])
    postal = load_postal(sys.argv[2])
    corrections = json.loads((DATA / "town-names.json").read_text())
    towns, venue_town, problems, how = build(venues(), munis, postal, corrections)
    for p in problems:
        print("ERROR:", p, file=sys.stderr)
    if problems:
        return 1
    OUT.write_text(json.dumps({"towns": dict(sorted(towns.items())), "venues": dict(sorted(venue_town.items()))},
                              ensure_ascii=False, indent=1) + "\n", encoding="utf-8")
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(venue_town)} venues in {len(towns)} towns ({dict(how)})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
