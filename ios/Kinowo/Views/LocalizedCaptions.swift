import SwiftUI

/// Looks up `key` in `Localizable.xcstrings`, translated for `locale`
/// regardless of the device's actual preferred language / `AppleLanguages`.
///
/// `String(localized:locale:)` does NOT do this despite taking an explicit
/// `locale:` parameter — verified empirically: launched with `AppleLanguages`
/// forced to `pl`, then passing `locale: Locale(identifier: "en")` still
/// returned the Polish translation, so the String Catalog runtime resolves
/// against the bundle's own cached preferred localization and ignores the
/// per-call override. Loading `locale`'s own `.lproj` sub-bundle directly and
/// querying IT sidesteps that cache — the same mechanism `NSLocalizedString`
/// used for years before String Catalogs existed, and `.xcstrings` still
/// compiles down to a classic `<lang>.lproj/Localizable.strings` per
/// language, so it works unchanged.
func localizedString(_ key: String, locale: Locale) -> String {
    let languageCode = locale.language.languageCode?.identifier ?? locale.identifier
    guard let path = Bundle.main.path(forResource: languageCode, ofType: "lproj"),
          let bundle = Bundle(path: path) else {
        return String(localized: String.LocalizationValue(key))
    }
    return bundle.localizedString(forKey: key, value: nil, table: "Localizable")
}

// Display captions for the two filter enums. They live here, in the app
// target, rather than beside the enums in `Models/Filters.swift`: those
// compile into `KinowoCore`, which deliberately ships no localized bundle
// (see the `Localizable.xcstrings` exclude in Package.swift), so
// `String(localized:)` there would resolve to the bare key under
// `swift test`. `String` rather than `LocalizedStringKey` because
// `DatePillsRow` measures each caption's rendered width to decide whether
// the four pills fit at a uniform size.
//
// `locale` is an explicit parameter, not read from `Locale.current` /
// `localizedString`'s own default: that resolves against `AppleLanguages`,
// which iOS only re-reads at the NEXT process launch (see `KinowoApp.init`'s
// doc comment) — every caller instead passes the CURRENT
// `@Environment(\.locale)`, which the root `.environment(\.locale)` modifier
// (`KinowoApp.body`) updates in-session the moment the Filtry language picker
// changes it.
extension DateFilter {
    func label(locale: Locale) -> String {
        switch self {
        case .anytime:         return localizedString("datefilter.anytime", locale: locale)
        case .today:           return localizedString("datefilter.today", locale: locale)
        case .tomorrow:        return localizedString("datefilter.tomorrow", locale: locale)
        case .week:            return localizedString("datefilter.week", locale: locale)
        case .specific(let d): return d
        }
    }
}

extension SortOption {
    func label(locale: Locale) -> String {
        switch self {
        case .earliest: return localizedString("sort.earliest", locale: locale)
        case .rating:   return localizedString("sort.rating", locale: locale)
        }
    }
}
