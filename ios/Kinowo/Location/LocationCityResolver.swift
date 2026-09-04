// CoreLocation isn't on swift-corelibs, so on Linux this file compiles to
// nothing and `KinowoCore` is the Foundation-only module it has always been.
// On macOS / iOS it compiles into the module, which is what lets the resolver's
// timing be unit-tested through the `LocationRequesting` seam below. The Xcode
// app target compiles the same file directly.
#if canImport(CoreLocation)
import Foundation
import CoreLocation

/// The two CoreLocation commands the city gate issues. Behind a seam so a test
/// can drive the resolver's timing — a slow permission grant, a fix that never
/// lands — without a device, a dialog, or a real fix. `CLLocationManager` is
/// the production implementation; it already has all three members.
protocol LocationRequesting: AnyObject {
    var authorizationStatus: CLAuthorizationStatus { get }
    func requestWhenInUseAuthorization()
    func requestLocation()
}

extension CLLocationManager: LocationRequesting {}

/// One-shot Core Location wrapper for the first-launch city gate. Asks for
/// when-in-use authorization, takes a single fix, and resolves it to the
/// nearest known `City` (or `nil` when denied / restricted / unavailable /
/// timed out / out of range). Owns no persistence — the caller decides what
/// to do with the result.
///
/// The pure pick (`City.nearestWithin100km`) lives in `City.swift` and is
/// unit-tested cross-platform; what this class owns, and what
/// `LocationCityResolverTests` covers, is the ORDER and the DEADLINES.
@MainActor
final class LocationCityResolver: NSObject, ObservableObject, CLLocationManagerDelegate {
    enum Outcome: Equatable {
        /// A fix landed inside a known city's 100 km radius.
        case city(City)
        /// No city to offer — denied, restricted, no fix, timed out, or the
        /// nearest city is too far. The caller falls back to manual choice.
        case unavailable
    }

    /// A single coordinate fix — what the "switch city?" check needs from a
    /// re-open, separate from the gate's nearest-`City` `Outcome`.
    struct Coordinate: Equatable {
        let lat: Double
        let lon: Double
    }

    private let requester: LocationRequesting
    /// How long to wait on the PERMISSION DIALOG. Generous, because it measures
    /// the user reading a system alert, not the system doing work: its only job
    /// is to stop the gate spinning forever when no dialog ever appears
    /// (Location Services off device-wide and the "Turn On" alert dismissed,
    /// where no authorization callback follows).
    private let authorizationTimeout: TimeInterval
    /// How long to wait on the FIX once we've actually asked for one. Armed at
    /// each `requestLocation()` and never before — the dialog's wall-clock must
    /// not eat it, or a user who takes a moment to tap "Allow" is dropped on the
    /// manual city list the fix was meant to skip.
    private let fixTimeout: TimeInterval
    /// Country whose cities the gate resolves a fix against — set by `resolve`
    /// so a fix is matched only to cities the SELECTED country serves (a Polish
    /// fix never resolves to a UK region, or vice versa). Empty until then,
    /// which matches no city — `resolve` is the only caller that reads it.
    private var countryCode = ""
    /// The live catalog cities to match a fix against — passed by `resolve` so
    /// the resolver reflects a server-fetched catalog, not a static list.
    private var cities: [City] = []
    private var continuation: CheckedContinuation<Outcome, Never>?
    private var coordinateContinuation: CheckedContinuation<Coordinate?, Never>?
    private var timeoutTask: Task<Void, Never>?

    /// Production: a real `CLLocationManager`, held through `requester` and
    /// wired to deliver its callbacks here.
    convenience init(authorizationTimeout: TimeInterval = 60, fixTimeout: TimeInterval = 8) {
        let manager = CLLocationManager()
        self.init(requester: manager, authorizationTimeout: authorizationTimeout, fixTimeout: fixTimeout)
        manager.delegate = self
        manager.desiredAccuracy = kCLLocationAccuracyKilometer
    }

    init(requester: LocationRequesting, authorizationTimeout: TimeInterval = 60, fixTimeout: TimeInterval = 8) {
        self.requester = requester
        self.authorizationTimeout = authorizationTimeout
        self.fixTimeout = fixTimeout
        super.init()
    }

    /// Request authorization + a single fix and resolve to an `Outcome`,
    /// matching the fix against `countryCode`'s cities. Always returns (never
    /// throws): every failure mode maps to `.unavailable` so the gate can show
    /// the manual picker.
    func resolve(in countryCode: String, cities: [City]) async -> Outcome {
        self.countryCode = countryCode
        self.cities = cities
        return await withCheckedContinuation { (cont: CheckedContinuation<Outcome, Never>) in
            continuation = cont
            start(for: requester.authorizationStatus)
        }
    }

    /// Take a single fix **only when location is already authorized**, never
    /// prompting for permission. Used by the app-open "you're nearer another
    /// city" check, which must stay silent for users who haven't granted (or
    /// have denied) access. Returns the coordinate, or `nil` when not
    /// authorized / no fix / timed out.
    func resolveIfAuthorized() async -> Coordinate? {
        switch requester.authorizationStatus {
        case .authorizedWhenInUse, .authorizedAlways:
            break
        default:
            return nil
        }
        return await withCheckedContinuation { (cont: CheckedContinuation<Coordinate?, Never>) in
            coordinateContinuation = cont
            requestFix()
        }
    }

    // MARK: - Flow

    private func start(for status: CLAuthorizationStatus) {
        switch status {
        case .notDetermined:
            armTimeout(authorizationTimeout)
            requester.requestWhenInUseAuthorization()
        case .authorizedWhenInUse, .authorizedAlways:
            requestFix()
        case .denied, .restricted:
            finish(.unavailable)
        @unknown default:
            finish(.unavailable)
        }
    }

    private func requestFix() {
        armTimeout(fixTimeout)
        requester.requestLocation()
    }

    /// Whether a request is in flight. CoreLocation also calls the
    /// authorization delegate when the delegate is first set, so without this
    /// the resolver would consume a fix — and arm a deadline that later expires
    /// over someone else's request — before anyone asked it for anything.
    private var isAwaitingOutcome: Bool { continuation != nil || coordinateContinuation != nil }

    /// The authorization answer, however it arrived. Internal so a test can
    /// deliver it without a dialog.
    func authorizationChanged(to status: CLAuthorizationStatus) {
        guard isAwaitingOutcome else { return }
        switch status {
        case .authorizedWhenInUse, .authorizedAlways:
            requestFix()
        case .denied, .restricted:
            finish(.unavailable)
        case .notDetermined:
            break // still waiting on the prompt
        @unknown default:
            finish(.unavailable)
        }
    }

    /// A fix, however it arrived. A bare-coordinate request (the "switch city?"
    /// check) wants it raw; the gate's request wants it resolved to a `City`.
    func deliverFix(lat: Double, lon: Double) {
        guard isAwaitingOutcome else { return }
        if coordinateContinuation != nil {
            finishCoordinate(Coordinate(lat: lat, lon: lon))
        } else if let city = cities.nearestWithin100km(lat: lat, lon: lon, inCountry: countryCode) {
            finish(.city(city))
        } else {
            finish(.unavailable)
        }
    }

    /// Fail whichever request is in flight: the coordinate request resolves
    /// to `nil`, the gate's `Outcome` request to `.unavailable`.
    func deliverNoFix() {
        if coordinateContinuation != nil {
            finishCoordinate(nil)
        } else {
            finish(.unavailable)
        }
    }

    // MARK: - Deadlines

    private func armTimeout(_ seconds: TimeInterval) {
        timeoutTask?.cancel()
        timeoutTask = Task { [weak self] in
            try? await Task.sleep(nanoseconds: UInt64(seconds * 1_000_000_000))
            guard !Task.isCancelled else { return }
            self?.deliverNoFix()
        }
    }

    private func cancelTimeout() {
        timeoutTask?.cancel()
        timeoutTask = nil
    }

    private func finish(_ outcome: Outcome) {
        guard let cont = continuation else { return }
        cancelTimeout()
        continuation = nil
        cont.resume(returning: outcome)
    }

    private func finishCoordinate(_ coordinate: Coordinate?) {
        guard let cont = coordinateContinuation else { return }
        cancelTimeout()
        coordinateContinuation = nil
        cont.resume(returning: coordinate)
    }

    // MARK: - CLLocationManagerDelegate

    nonisolated func locationManagerDidChangeAuthorization(_ manager: CLLocationManager) {
        let status = manager.authorizationStatus
        Task { @MainActor in self.authorizationChanged(to: status) }
    }

    nonisolated func locationManager(_ manager: CLLocationManager, didUpdateLocations locations: [CLLocation]) {
        guard let loc = locations.last else {
            Task { @MainActor in self.deliverNoFix() }
            return
        }
        let lat = loc.coordinate.latitude
        let lon = loc.coordinate.longitude
        Task { @MainActor in self.deliverFix(lat: lat, lon: lon) }
    }

    nonisolated func locationManager(_ manager: CLLocationManager, didFailWithError error: Error) {
        Task { @MainActor in self.deliverNoFix() }
    }
}
#endif
