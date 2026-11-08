import SwiftUI

/// The listing before its first repertoire load lands.
struct ListingLoadingView: View {
    var body: some View {
        VStack(spacing: 12) {
            ProgressView()
            Text("content.loading").font(.callout).foregroundStyle(.secondary)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .accessibilityElement(children: .contain)
        .accessibilityIdentifier(A11y.EmptyState.loading)
    }
}

/// The listing when its first load failed and nothing is cached: the error and
/// a retry.
struct ListingErrorView: View {
    let error: Error
    let onRetry: () -> Void

    var body: some View {
        VStack(spacing: 12) {
            Image(systemName: "exclamationmark.triangle").font(.largeTitle).foregroundStyle(.orange)
            Text("content.error")
            Text(error.localizedDescription)
                .font(.caption).foregroundStyle(.secondary)
                .multilineTextAlignment(.center)
                .padding(.horizontal)
            Button("content.retry", action: onRetry)
                .buttonStyle(.borderedProminent)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .accessibilityElement(children: .contain)
        .accessibilityIdentifier(A11y.EmptyState.error)
    }
}
