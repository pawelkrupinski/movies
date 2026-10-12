import SwiftUI

/// Full-screen block shown when this build is below the server's minimum
/// supported version (`ClientSupportStore.requiredUpdate`): there is no way
/// past it but the App Store.
struct UpdateRequiredView: View {
    let storeURL: URL
    @Environment(\.openURL) private var openURL

    var body: some View {
        VStack(spacing: 16) {
            Spacer()
            Image(systemName: "arrow.down.app")
                .font(.system(size: 56))
                .foregroundStyle(.tint)
            Text("update_required.title")
                .font(.title2.bold())
                .multilineTextAlignment(.center)
            Text("update_required.message")
                .font(.body)
                .foregroundStyle(.secondary)
                .multilineTextAlignment(.center)
            Button {
                openURL(storeURL)
            } label: {
                Text("update_required.button")
                    .frame(maxWidth: .infinity)
            }
            .buttonStyle(.borderedProminent)
            .controlSize(.large)
            .padding(.top, 8)
            Spacer()
        }
        .padding(.horizontal, 32)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(Color.black.ignoresSafeArea())
    }
}
