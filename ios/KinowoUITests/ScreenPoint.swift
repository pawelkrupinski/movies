import XCTest

/// A screen point fixed when it is made: `element`'s frame is resolved ONCE,
/// and the point is anchored to the application, not to the element.
///
/// Why a press must not anchor to a grid element: XCUITest evaluates an
/// element query inside the app, on the app's MAIN thread, and a coordinate
/// anchored to an element has it re-resolved while the gesture is synthesized
/// (the "Find" lines under "Synthesize event" in the test log). On the CI
/// runner's Xcode 26 those finds overlap the touches, and a
/// `descendants(matching: .any)` search over the film grid holds the main
/// thread 0.4–1.5 s each. A touch-down and touch-up queued behind one are
/// handled back to back, so a long press arrives as a tap. Order independence
/// #74: `Press "filmgrid.cell" Any[0.50, 0.18] for 0.7 seconds` ran three
/// finds of 0.84, 1.32 and ~1.5 s, the screen recording shows the detail push
/// already under way during them, and the share menu never opened. An
/// application-anchored point is re-resolved as the application element alone,
/// which costs ~0.05 s, so the hold reaches the app as a hold.
enum ScreenPoint {
    static func of(
        _ element: XCUIElement,
        normalizedOffset offset: CGVector,
        in app: XCUIApplication
    ) -> XCUICoordinate {
        let frame = element.frame
        return app.coordinate(withNormalizedOffset: .zero).withOffset(CGVector(
            dx: frame.minX + frame.width * offset.dx,
            dy: frame.minY + frame.height * offset.dy
        ))
    }
}
