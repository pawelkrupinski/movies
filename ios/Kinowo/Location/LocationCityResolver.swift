import Foundation
import CoreLocation

/// One-shot Core Location wrapper for the first-launch city gate. Asks for
/// when-in-use authorization, takes a single fix, and resolves it to the
/// nearest known `City` (or `nil` when denied / restricted / unavailable /
/// timed out / out of range). Owns no persistence — the caller decides what
/// to do with the result.
///
/// CoreLocation isn't on Linux, so this file lives in the app target and is
/// excluded from `KinowoCore`. The pure pick (`City.nearestWithin100km`) is
/// in `KinowoCore` and unit-tested there.
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

    private let manager = CLLocationManager()
    private let timeout: TimeInterval
    private var continuation: CheckedContinuation<Outcome, Never>?
    private var coordinateContinuation: CheckedContinuation<Coordinate?, Never>?
    private var timeoutTask: Task<Void, Never>?

    init(timeout: TimeInterval = 8) {
        self.timeout = timeout
        super.init()
        manager.delegate = self
        manager.desiredAccuracy = kCLLocationAccuracyKilometer
    }

    /// Request authorization + a single fix and resolve to an `Outcome`.
    /// Always returns (never throws): every failure mode maps to
    /// `.unavailable` so the gate can show the manual picker.
    func resolve() async -> Outcome {
        await withCheckedContinuation { (cont: CheckedContinuation<Outcome, Never>) in
            continuation = cont
            timeoutTask = Task { [weak self] in
                try? await Task.sleep(nanoseconds: UInt64((self?.timeout ?? 8) * 1_000_000_000))
                guard !Task.isCancelled else { return }
                self?.finish(.unavailable)
            }
            start(for: manager.authorizationStatus)
        }
    }

    /// Take a single fix **only when location is already authorized**, never
    /// prompting for permission. Used by the app-open "you're nearer another
    /// city" check, which must stay silent for users who haven't granted (or
    /// have denied) access. Returns the coordinate, or `nil` when not
    /// authorized / no fix / timed out.
    func resolveIfAuthorized() async -> Coordinate? {
        switch manager.authorizationStatus {
        case .authorizedWhenInUse, .authorizedAlways:
            break
        default:
            return nil
        }
        return await withCheckedContinuation { (cont: CheckedContinuation<Coordinate?, Never>) in
            coordinateContinuation = cont
            timeoutTask = Task { [weak self] in
                try? await Task.sleep(nanoseconds: UInt64((self?.timeout ?? 8) * 1_000_000_000))
                guard !Task.isCancelled else { return }
                self?.finishCoordinate(nil)
            }
            manager.requestLocation()
        }
    }

    private func finishCoordinate(_ coordinate: Coordinate?) {
        timeoutTask?.cancel()
        timeoutTask = nil
        guard let cont = coordinateContinuation else { return }
        coordinateContinuation = nil
        cont.resume(returning: coordinate)
    }

    private func start(for status: CLAuthorizationStatus) {
        switch status {
        case .notDetermined:
            manager.requestWhenInUseAuthorization()
        case .authorizedWhenInUse, .authorizedAlways:
            manager.requestLocation()
        case .denied, .restricted:
            finish(.unavailable)
        @unknown default:
            finish(.unavailable)
        }
    }

    private func finish(_ outcome: Outcome) {
        timeoutTask?.cancel()
        timeoutTask = nil
        guard let cont = continuation else { return }
        continuation = nil
        cont.resume(returning: outcome)
    }

    // MARK: - CLLocationManagerDelegate

    nonisolated func locationManagerDidChangeAuthorization(_ manager: CLLocationManager) {
        let status = manager.authorizationStatus
        Task { @MainActor in
            switch status {
            case .authorizedWhenInUse, .authorizedAlways:
                manager.requestLocation()
            case .denied, .restricted:
                finish(.unavailable)
            case .notDetermined:
                break // still waiting on the prompt
            @unknown default:
                finish(.unavailable)
            }
        }
    }

    nonisolated func locationManager(_ manager: CLLocationManager, didUpdateLocations locations: [CLLocation]) {
        guard let loc = locations.last else {
            Task { @MainActor in self.deliverNoFix() }
            return
        }
        let lat = loc.coordinate.latitude
        let lon = loc.coordinate.longitude
        Task { @MainActor in
            // A bare-coordinate request (the "switch city?" check) wants the
            // raw fix; the gate's request wants it resolved to a `City`.
            if self.coordinateContinuation != nil {
                self.finishCoordinate(Coordinate(lat: lat, lon: lon))
            } else if let city = City.nearestWithin100km(lat: lat, lon: lon) {
                self.finish(.city(city))
            } else {
                self.finish(.unavailable)
            }
        }
    }

    nonisolated func locationManager(_ manager: CLLocationManager, didFailWithError error: Error) {
        Task { @MainActor in self.deliverNoFix() }
    }

    /// Fail whichever request is in flight: the coordinate request resolves
    /// to `nil`, the gate's `Outcome` request to `.unavailable`.
    private func deliverNoFix() {
        if coordinateContinuation != nil {
            finishCoordinate(nil)
        } else {
            finish(.unavailable)
        }
    }
}
