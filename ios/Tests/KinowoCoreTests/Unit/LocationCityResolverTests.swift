// CoreLocation isn't on swift-corelibs (Linux CI), so this suite compiles to
// nothing there and runs on the macOS `swift test` parity leg. No real
// `CLLocationManager` is touched: the resolver's two CoreLocation commands go
// through the `LocationRequesting` seam, and the delegate callbacks are driven
// directly, so nothing here can raise a permission dialog.
#if canImport(CoreLocation)
import CoreLocation
import XCTest
@testable import KinowoCore

/// Records the CoreLocation commands the resolver issues instead of performing
/// them, so a test can assert both WHAT it asked for and WHEN.
final class RecordingLocationRequester: LocationRequesting {
    var authorizationStatus: CLAuthorizationStatus
    /// The fix CoreLocation is holding, as `CLLocationManager.location` would
    /// report it. `nil` is a manager that has never had one.
    private let heldFix: CLLocation?
    /// How many times `location` was read ON THE MAIN THREAD. The real getter
    /// is a synchronous XPC round trip to locationd, which can stall for most
    /// of a minute (a freshly booted device), so any read there freezes the UI.
    private(set) var mainThreadLocationReads = 0
    private(set) var authorizationRequests = 0
    private(set) var locationRequests = 0

    var location: CLLocation? {
        if Thread.isMainThread { mainThreadLocationReads += 1 }
        return heldFix
    }

    init(status: CLAuthorizationStatus, location: CLLocation? = nil) {
        authorizationStatus = status
        heldFix = location
    }

    func requestWhenInUseAuthorization() { authorizationRequests += 1 }
    func requestLocation() { locationRequests += 1 }
}

/// The "user granted when-in-use" status. `.authorizedWhenInUse` doesn't exist
/// on macOS, where `swift test` runs, and the resolver treats the two grants
/// identically — so the suite names whichever one the platform has.
#if os(iOS)
private let granted = CLAuthorizationStatus.authorizedWhenInUse
#else
private let granted = CLAuthorizationStatus.authorizedAlways
#endif

@MainActor
final class LocationCityResolverTests: XCTestCase {

    /// A fix at a coordinate, aged as CoreLocation would report it.
    private func fix(lat: Double, lon: Double, secondsOld: TimeInterval) -> CLLocation {
        CLLocation(
            coordinate: CLLocationCoordinate2D(latitude: lat, longitude: lon),
            altitude: 0,
            horizontalAccuracy: 100,
            verticalAccuracy: -1,
            timestamp: Date().addingTimeInterval(-secondsOld)
        )
    }

    /// Poll until the resolver has reached the state `condition` names — it
    /// issues its CoreLocation commands only once it is awaiting the outcome.
    private func waitUntil(_ condition: () -> Bool, file: StaticString = #filePath, line: UInt = #line) async {
        let deadline = Date().addingTimeInterval(5)
        while !condition() {
            guard Date() < deadline else { return XCTFail("resolver never reached the awaited state", file: file, line: line) }
            try? await Task.sleep(nanoseconds: 2_000_000)
        }
    }

    private let cities = [
        City(slug: "poznan", name: "Poznań", lat: 52.4064, lon: 16.9252, country: "pl"),
        City(slug: "warszawa", name: "Warszawa", lat: 52.2297, lon: 21.0122, country: "pl"),
    ]

    /// The first-launch permission dialog is the USER reading a system alert —
    /// the time they spend there must not burn the deadline for the fix. It
    /// used to: one timer covered dialog + fix, so anyone who took more than
    /// `fixTimeout` to tap "Allow" got `.unavailable` and the gate dropped them
    /// on the manual city list — the very list the fix they just granted was
    /// meant to skip.
    func testSlowPermissionGrantStillResolvesTheDetectedCity() async {
        let requester = RecordingLocationRequester(status: .notDetermined)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 0.1)

        async let outcome = resolver.resolve(in: "pl", cities: cities)
        await waitUntil { requester.authorizationRequests == 1 }

        // The user takes longer than the fix deadline to answer the dialog.
        try? await Task.sleep(nanoseconds: 250_000_000)
        XCTAssertEqual(requester.authorizationRequests, 1)
        XCTAssertEqual(requester.locationRequests, 0, "no fix is asked for until the user has answered")

        requester.authorizationStatus = granted
        resolver.authorizationChanged(to: granted)
        // The held fix is read off the main thread first (none here), then the fix is asked for.
        await waitUntil { requester.locationRequests == 1 }
        XCTAssertEqual(requester.locationRequests, 1, "the grant is what asks for the fix")
        resolver.deliverFix(lat: 52.4064, lon: 16.9252)

        let result = await outcome
        XCTAssertEqual(result, .city(cities[0]))
    }

    /// The fix deadline still bites once we're actually waiting on the system:
    /// authorized, asked, nothing came back.
    func testAuthorizedButNoFixTimesOutToUnavailable() async {
        let requester = RecordingLocationRequester(status: granted)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 0.05)

        let outcome = await resolver.resolve(in: "pl", cities: cities)

        XCTAssertEqual(outcome, .unavailable)
        XCTAssertEqual(requester.locationRequests, 1)
        XCTAssertEqual(requester.authorizationRequests, 0, "an authorized user is never re-prompted")
    }

    /// A dialog that never gets answered — Location Services off device-wide and
    /// the "Turn On" alert dismissed, where no authorization callback ever comes
    /// — must not leave the gate spinning forever.
    func testUnansweredPermissionDialogGivesUpAfterTheAuthorizationDeadline() async {
        let requester = RecordingLocationRequester(status: .notDetermined)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 0.05, fixTimeout: 30)

        let outcome = await resolver.resolve(in: "pl", cities: cities)

        XCTAssertEqual(outcome, .unavailable)
        XCTAssertEqual(requester.locationRequests, 0)
    }

    func testDeniedResolvesImmediatelyWithoutAskingForAFix() async {
        let requester = RecordingLocationRequester(status: .denied)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        let outcome = await resolver.resolve(in: "pl", cities: cities)

        XCTAssertEqual(outcome, .unavailable)
        XCTAssertEqual(requester.locationRequests, 0)
        XCTAssertEqual(requester.authorizationRequests, 0)
    }

    /// A denial delivered through the dialog ends the wait too, rather than
    /// leaving it to the authorization deadline.
    func testDenialThroughTheDialogEndsTheWait() async {
        let requester = RecordingLocationRequester(status: .notDetermined)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        async let outcome = resolver.resolve(in: "pl", cities: cities)
        await waitUntil { requester.authorizationRequests == 1 } // the dialog is up
        resolver.authorizationChanged(to: .denied)

        let result = await outcome
        XCTAssertEqual(result, .unavailable)
        XCTAssertEqual(requester.locationRequests, 0)
    }

    /// A fix outside every city's radius is still no city to offer.
    func testFixOutOfRangeResolvesToUnavailable() async {
        let requester = RecordingLocationRequester(status: granted)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        async let outcome = resolver.resolve(in: "pl", cities: cities)
        await waitUntil { requester.locationRequests == 1 } // the resolver is now awaiting the fix
        resolver.deliverFix(lat: 0, lon: 0)

        let result = await outcome
        XCTAssertEqual(result, .unavailable)
    }

    /// `resolveAnyCountry` backs the manual picker's "use my location" button:
    /// unlike `resolve(in:cities:)`, it must resolve to a city whose country is
    /// NOT the one passed anywhere — there's no country to pass at all.
    func testResolveAnyCountryCrossesCountryBorders() async {
        let requester = RecordingLocationRequester(status: granted)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)
        let berlin = City(slug: "berlin", name: "Berlin", lat: 52.5200, lon: 13.4050, country: "de")
        let crossCountryCities = cities + [berlin]

        async let outcome = resolver.resolveAnyCountry(cities: crossCountryCities)
        await waitUntil { requester.locationRequests == 1 } // the resolver is now awaiting the fix
        resolver.deliverFix(lat: 52.5200, lon: 13.4050)

        let result = await outcome
        XCTAssertEqual(result, .city(berlin))
    }

    /// `resolveAnyCountry`'s genuine miss: a fix over 100 km from every city of
    /// every country stays `.unavailable`, same cutoff as the scoped resolver.
    func testResolveAnyCountryStillMissesWhenOutOfRangeOfEveryCountry() async {
        let requester = RecordingLocationRequester(status: granted)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)
        let berlin = City(slug: "berlin", name: "Berlin", lat: 52.5200, lon: 13.4050, country: "de")

        async let outcome = resolver.resolveAnyCountry(cities: cities + [berlin])
        await waitUntil { requester.locationRequests == 1 } // the resolver is now awaiting the fix
        resolver.deliverFix(lat: 0, lon: 0)

        let result = await outcome
        XCTAssertEqual(result, .unavailable)
    }

    /// The fix CoreLocation is already holding answers outright. A fresh
    /// `requestLocation()` on a cold radio is seconds away at best and can fail
    /// indoors, and this is the gap that left an iPhone with working location
    /// staring at the manual city list — Android has always read its
    /// `lastLocation` first.
    func testACachedFixAnswersWithoutWaitingForAFreshOne() async {
        let requester = RecordingLocationRequester(
            status: granted,
            location: fix(lat: 52.4064, lon: 16.9252, secondsOld: 60)
        )
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        let outcome = await resolver.resolve(in: "pl", cities: cities)

        XCTAssertEqual(outcome, .city(cities[0]))
        XCTAssertEqual(requester.locationRequests, 0, "the held fix is the answer — nothing to wait for")
    }

    /// Old enough to have travelled, so a fresh fix is worth asking for.
    func testAStaleCachedFixStillAsksForAFreshOne() async {
        let requester = RecordingLocationRequester(
            status: granted,
            location: fix(lat: 52.2297, lon: 21.0122, secondsOld: 3600)
        )
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        async let outcome = resolver.resolve(in: "pl", cities: cities)
        await waitUntil { requester.locationRequests == 1 } // the resolver is now awaiting the fix
        XCTAssertEqual(requester.locationRequests, 1)
        resolver.deliverFix(lat: 52.4064, lon: 16.9252)

        let result = await outcome
        XCTAssertEqual(result, .city(cities[0]), "the fresh fix wins over the stale one")
    }

    /// An hours-old fix names the right city far more often than not, and the
    /// alternative is the list the user was trying to skip.
    func testAStaleFixIsBetterThanNoCityWhenTheFreshOneNeverLands() async {
        let requester = RecordingLocationRequester(
            status: granted,
            location: fix(lat: 52.2297, lon: 21.0122, secondsOld: 3600)
        )
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 0.05)

        let outcome = await resolver.resolve(in: "pl", cities: cities)

        XCTAssertEqual(requester.locationRequests, 1)
        XCTAssertEqual(outcome, .city(cities[1]), "Warszawa, from the stale fix")
    }

    /// `kCLErrorLocationUnknown` is "not yet", not "never" — the radios warming
    /// up on a first launch. Giving up on it threw the whole gate away.
    func testATransientFailureAsksAgainWithinTheDeadline() async {
        let requester = RecordingLocationRequester(status: granted)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        async let outcome = resolver.resolve(in: "pl", cities: cities)
        await waitUntil { requester.locationRequests == 1 } // the resolver is now awaiting the fix
        XCTAssertEqual(requester.locationRequests, 1)

        resolver.fixFailed(transient: true)
        XCTAssertEqual(requester.locationRequests, 2, "a transient miss is retried, not surrendered to")
        resolver.deliverFix(lat: 52.4064, lon: 16.9252)

        let result = await outcome
        XCTAssertEqual(result, .city(cities[0]))
    }

    /// A non-transient CoreLocation error has nothing to retry.
    func testAFinalFailureGivesUpWithoutRetrying() async {
        let requester = RecordingLocationRequester(status: granted)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        async let outcome = resolver.resolve(in: "pl", cities: cities)
        await waitUntil { requester.locationRequests == 1 } // the resolver is now awaiting the fix
        resolver.fixFailed(transient: false)

        let result = await outcome
        XCTAssertEqual(result, .unavailable)
        XCTAssertEqual(requester.locationRequests, 1)
    }

    /// `resolveIfAuthorized` is the silent app-open check: never prompts, and
    /// hands back the raw coordinate rather than a city.
    func testResolveIfAuthorizedReturnsTheRawCoordinate() async {
        let requester = RecordingLocationRequester(status: granted)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        async let coordinate = resolver.resolveIfAuthorized()
        await waitUntil { requester.locationRequests == 1 } // the resolver is now awaiting the fix
        resolver.deliverFix(lat: 52.2297, lon: 21.0122)

        let result = await coordinate
        XCTAssertEqual(result, LocationCityResolver.Coordinate(lat: 52.2297, lon: 21.0122))
        XCTAssertEqual(requester.authorizationRequests, 0)
    }

    /// The app-open "switch city?" check runs on the main actor at launch, and
    /// reading the held fix is a synchronous round trip to locationd. On a
    /// freshly booted device that round trip stalled for up to a minute, and
    /// with it the whole UI: XCUITest run 36157983964 lost its first two tests
    /// on a new simulator clone to an app that never went idle, its main thread
    /// parked in `CLLocationManager.location`. The read belongs off the main
    /// thread, both for the held fix and for the stale fallback.
    func testTheHeldFixIsNeverReadOnTheMainThread() async {
        let fresh = RecordingLocationRequester(
            status: granted,
            location: fix(lat: 52.2297, lon: 21.0122, secondsOld: 60)
        )
        let coordinate = await LocationCityResolver(requester: fresh, authorizationTimeout: 30, fixTimeout: 30)
            .resolveIfAuthorized()
        XCTAssertEqual(coordinate, LocationCityResolver.Coordinate(lat: 52.2297, lon: 21.0122))
        XCTAssertEqual(fresh.mainThreadLocationReads, 0, "the held fix was read on the main thread")

        let stale = RecordingLocationRequester(
            status: granted,
            location: fix(lat: 52.2297, lon: 21.0122, secondsOld: 3600)
        )
        let outcome = await LocationCityResolver(requester: stale, authorizationTimeout: 30, fixTimeout: 0.05)
            .resolve(in: "pl", cities: cities)
        XCTAssertEqual(outcome, .city(cities[1]), "Warszawa, from the stale fix")
        XCTAssertEqual(stale.mainThreadLocationReads, 0, "the stale fallback was read on the main thread")
    }

    /// The no-fix fallback reads the held fix AFTER the fix deadline has
    /// passed, so nothing else bounds that read. A locationd round trip that
    /// stalls (a freshly booted device) used to hold the gate open for as long
    /// as the stall lasted; past its own short deadline there is simply no fix.
    func testAStalledStaleFixReadStillEndsTheGateOnTime() async {
        let requester = RecordingLocationRequester(status: granted)
        let stall = DispatchSemaphore(value: 0)
        defer { for _ in 0..<4 { stall.signal() } }
        let resolver = LocationCityResolver(
            requester: requester,
            readHeldFix: { _ = stall.wait(timeout: .now() + 5); return nil },
            authorizationTimeout: 30, fixTimeout: 0.05, staleFixReadTimeout: 0.1
        )

        let started = Date()
        let outcome = await resolver.resolve(in: "pl", cities: cities)

        XCTAssertEqual(outcome, .unavailable)
        XCTAssertLessThan(Date().timeIntervalSince(started), 2, "the stalled read held the gate open")
    }

    func testResolveIfAuthorizedStaysSilentWhenNotAuthorized() async {
        let requester = RecordingLocationRequester(status: .notDetermined)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        let coordinate = await resolver.resolveIfAuthorized()

        XCTAssertNil(coordinate)
        XCTAssertEqual(requester.authorizationRequests, 0, "the silent check never raises the dialog")
        XCTAssertEqual(requester.locationRequests, 0)
    }

    /// A no-fix timeout on the silent check resolves the coordinate request,
    /// not the gate's `Outcome` one.
    func testResolveIfAuthorizedTimesOutToNil() async {
        let requester = RecordingLocationRequester(status: granted)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 0.05)

        let coordinate = await resolver.resolveIfAuthorized()

        XCTAssertNil(coordinate)
        XCTAssertEqual(requester.locationRequests, 1)
    }
}
#endif
