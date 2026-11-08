import SwiftUI

// Floating label that names the day the user just swiped to — the same
// caption as the matching date pill. Shown on every day-swipe; fades out
// after ~0.7 s.
// Positioned by the caller via `.overlay` so it sits centre-screen.
struct DayLabelOverlay: View {
    let text: String

    var body: some View {
        Text(text)
            .font(.system(size: 28, weight: .semibold))
            .foregroundStyle(.primary)
            .padding(.horizontal, 28)
            .padding(.vertical, 12)
            .background(.ultraThinMaterial, in: Capsule())
            .overlay(
                Capsule().strokeBorder(Color.white.opacity(0.12))
            )
            .accessibilityIdentifier(A11y.DayOverlay.label)
    }
}

// Once-a-day onboarding hint — a swipe-hand glyph over one line of copy,
// telling first-time users they can swipe to change the selected day. Shown
// centre-screen right after the first repertoire load and retired for good
// on the user's first-ever day-swipe. The Android counterpart is the
// `SwipeHintOverlay` composable in ListScreen.kt.
struct SwipeHintOverlay: View {
    var body: some View {
        VStack(spacing: 8) {
            Image(systemName: "hand.draw")
                .font(.system(size: 32, weight: .regular))
            Text("swipehint.label")
                .font(.system(size: 15, weight: .medium))
        }
        .foregroundStyle(.primary)
        .padding(.horizontal, 24)
        .padding(.vertical, 16)
        .background(.ultraThinMaterial, in: RoundedRectangle(cornerRadius: 20))
        .overlay(
            RoundedRectangle(cornerRadius: 20).strokeBorder(Color.white.opacity(0.12))
        )
        .accessibilityIdentifier(A11y.SwipeHint.overlay)
    }
}
