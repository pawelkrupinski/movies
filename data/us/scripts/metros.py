"""Turn a Flicks metro slug into the label a cinema picker shows.

Flicks files every US venue under a metro `region_slug` (`des-moines`,
`dallas-fort-worth`, `western-pa`, …) and never ships a display name for it, so
the label has to be derived. The slug stays the identity — clients persist it as
a group key — and the label is only what the group is CALLED.

Three rules, in order:

1. Title-case the hyphen-separated words (`des-moines` -> "Des Moines").
2. Upper-case the two-letter state codes Flicks uses to disambiguate a repeated
   town name (`birmingham-al` -> "Birmingham AL", `western-pa` -> "Western PA").
3. Drop a trailing qualifier naming the venue's OWN state — inside Georgia's
   picker "Albany Georgia" says nothing "Albany" doesn't. Kept when what's left
   is only a compass word or an abbreviation, because "Western" and "US" are not
   names ("Western PA", "US Virgin Islands"), and kept for BOTH metros when the
   trim would make two of a state's labels identical.
"""
import re

# The two-letter codes that appear as a Flicks disambiguating suffix, plus the
# "us" of `us-virgin-islands`. Upper-cased in a label, and never a name on their
# own (rule 3).
ABBREVIATIONS = {
    'al', 'ak', 'az', 'ar', 'ca', 'co', 'ct', 'dc', 'de', 'fl', 'ga', 'hi',
    'ia', 'id', 'il', 'in', 'ks', 'ky', 'la', 'ma', 'md', 'me', 'mi', 'mn',
    'mo', 'ms', 'mt', 'nc', 'nd', 'ne', 'nh', 'nj', 'nm', 'nv', 'ny', 'oh',
    'ok', 'or', 'pa', 'ri', 'sc', 'sd', 'tn', 'tx', 'ut', 'va', 'vt', 'wa',
    'wi', 'wv', 'wy', 'us',
}

# State names by their two-letter code, for rule 3's "does this suffix name the
# venue's own state?" test.
STATE_ABBREVIATION = {
    'Alabama': 'al', 'Alaska': 'ak', 'Arizona': 'az', 'Arkansas': 'ar',
    'California': 'ca', 'Colorado': 'co', 'Connecticut': 'ct', 'Delaware': 'de',
    'District of Columbia': 'dc', 'Florida': 'fl', 'Georgia': 'ga',
    'Hawaii': 'hi', 'Idaho': 'id', 'Illinois': 'il', 'Indiana': 'in',
    'Iowa': 'ia', 'Kansas': 'ks', 'Kentucky': 'ky', 'Louisiana': 'la',
    'Maine': 'me', 'Maryland': 'md', 'Massachusetts': 'ma', 'Michigan': 'mi',
    'Minnesota': 'mn', 'Mississippi': 'ms', 'Missouri': 'mo', 'Montana': 'mt',
    'Nebraska': 'ne', 'Nevada': 'nv', 'New Hampshire': 'nh', 'New Jersey': 'nj',
    'New Mexico': 'nm', 'New York': 'ny', 'North Carolina': 'nc',
    'North Dakota': 'nd', 'Ohio': 'oh', 'Oklahoma': 'ok', 'Oregon': 'or',
    'Pennsylvania': 'pa', 'Rhode Island': 'ri', 'South Carolina': 'sc',
    'South Dakota': 'sd', 'Tennessee': 'tn', 'Texas': 'tx', 'Utah': 'ut',
    'Vermont': 'vt', 'Virginia': 'va', 'Washington': 'wa',
    'West Virginia': 'wv', 'Wisconsin': 'wi', 'Wyoming': 'wy',
}

# Compass/position words: a label made only of these is a direction, not a place,
# so its state suffix has to stay.
DIRECTIONS = {
    'north', 'south', 'east', 'west', 'central', 'northern', 'southern',
    'eastern', 'western', 'northeast', 'northwest', 'southeast', 'southwest',
    'upper', 'lower', 'mid', 'greater',
}


def _words(slug):
    return [w for w in slug.split('-') if w]


def _titled(words):
    return ' '.join(w.upper() if w in ABBREVIATIONS else w.capitalize() for w in words)


def _trimmed(slug, state):
    """`slug` minus a trailing qualifier naming `state`, or unchanged."""
    words = _words(slug)
    suffixes = [_words(re.sub(r'[^a-z0-9]+', '-', state.lower()))]
    abbreviation = STATE_ABBREVIATION.get(state)
    if abbreviation:
        suffixes.append([abbreviation])
    for suffix in suffixes:
        if len(words) > len(suffix) and words[-len(suffix):] == suffix:
            rest = words[:-len(suffix)]
            if any(w not in DIRECTIONS and w not in ABBREVIATIONS for w in rest):
                return rest
    return words


def labels_by_slug(slugs_by_state):
    """{state: {metro slug: label}} for the metro slugs each state actually uses.

    Done per state rather than per slug because rule 3 is state-relative and its
    collision guard is state-wide.
    """
    out = {}
    for state, slugs in slugs_by_state.items():
        trimmed = {slug: _titled(_trimmed(slug, state)) for slug in slugs}
        clashing = {label for label in trimmed.values()
                    if list(trimmed.values()).count(label) > 1}
        out[state] = {
            slug: (_titled(_words(slug)) if label in clashing else label)
            for slug, label in trimmed.items()
        }
    return out
