// DevPanel — a small always-on-top floating palette of dev actions for the
// movies repo.
//
// It has no Dock icon (accessory app). The yellow traffic-light HIDES the
// panel; a menu-bar (☰) icon brings it back (left-click) or quits it
// (right-click) — so it can be tucked away without quitting.
//
// Every action runs `scripts/devpanel.py <action>` as a background subprocess, streaming live
// output into an in-panel console. There are two consoles:
//   • Web    — shared by the web server actions; KEEPS its scrollback across
//              runs (long-lived servers you want to keep watching).
//   • Device — shared by Android + iOS; CLEARED at the start of each run.
// Each console is independently collapsible. When BOTH are collapsed the panel
// shrinks to a fixed narrow size (just the buttons); when one is open the panel
// is freely resizable by hand, and its width is remembered. Log text is
// selectable and ⌘C copies it.
//
// Long-press (or right-click) any button to pick which git worktree to run the
// task in; the chosen path is handed to devpanel.py via DEVPANEL_REPO_ROOT.
//
// The absolute scripts directory is baked into Info.plist (DevPanelScriptsDir)
// at build time, so the .app keeps working if moved out of the repo tree.

import AppKit

// MARK: - LAN IP

/// The Mac's LAN IPv4, shown in the header so the local dev server can be
/// reached from another device (a phone on the same Wi-Fi). Loopback is useless
/// for that, so a site-local address (`192.168.x`, `10.x`, `172.16–31.x`) wins
/// when one exists.
enum LocalHostIp {

    /// RFC-1918 private ranges — the addresses a phone on the same network can
    /// actually reach.
    static func isSiteLocal(_ ip: String) -> Bool {
        if ip.hasPrefix("10.") || ip.hasPrefix("192.168.") { return true }
        if ip.hasPrefix("172.") {
            let octets = ip.split(separator: ".")
            if octets.count >= 2, let second = Int(octets[1]) { return (16...31).contains(second) }
        }
        return false
    }

    /// Pure selection rule (unit-testable, no IO): a site-local IPv4 if any,
    /// else the first non-loopback IPv4, else `nil`.
    static func pick(_ candidates: [String]) -> String? {
        let usable = candidates.filter { !$0.hasPrefix("127.") }
        return usable.first(where: isSiteLocal) ?? usable.first
    }

    /// Every IPv4 address bound to an up interface (loopback included — `pick`
    /// drops it), in the order `getifaddrs` reports them.
    static func ipv4Addresses() -> [String] {
        var out: [String] = []
        var head: UnsafeMutablePointer<ifaddrs>?
        guard getifaddrs(&head) == 0 else { return out }
        defer { freeifaddrs(head) }
        var cursor = head
        while let cur = cursor {
            defer { cursor = cur.pointee.ifa_next }
            let flags = Int32(cur.pointee.ifa_flags)
            guard (flags & IFF_UP) == IFF_UP, let sa = cur.pointee.ifa_addr,
                  sa.pointee.sa_family == UInt8(AF_INET) else { continue }
            var host = [CChar](repeating: 0, count: Int(NI_MAXHOST))
            if getnameinfo(sa, socklen_t(sa.pointee.sa_len), &host, socklen_t(host.count),
                           nil, 0, NI_NUMERICHOST) == 0 {
                out.append(String(cString: host))
            }
        }
        return out
    }

    /// The best LAN IPv4 for this Mac right now, or `nil` if none is bound.
    static func current() -> String? { pick(ipv4Addresses()) }
}

// MARK: - Command runner (streams a subprocess)

/// Runs a command as a child process, streaming merged stdout+stderr to
/// `onOutput` and the exit code to `onExit`, both on `callbackQueue`.
final class CommandRunner {
    private let callbackQueue: DispatchQueue
    private var process: Process?
    var onOutput: ((String) -> Void)?
    var onExit: ((Int32) -> Void)?

    init(callbackQueue: DispatchQueue = .main) { self.callbackQueue = callbackQueue }

    var isRunning: Bool { process?.isRunning ?? false }

    func run(executable: String, arguments: [String], environment: [String: String]? = nil) {
        let p = Process()
        p.executableURL = URL(fileURLWithPath: executable)
        p.arguments = arguments
        if let extra = environment {
            p.environment = ProcessInfo.processInfo.environment.merging(extra) { _, new in new }
        }
        let pipe = Pipe()
        p.standardOutput = pipe
        p.standardError = pipe

        let drained = DispatchSemaphore(value: 0)
        pipe.fileHandleForReading.readabilityHandler = { [weak self] handle in
            let data = handle.availableData
            guard !data.isEmpty else {          // EOF: every writer has closed the pipe
                handle.readabilityHandler = nil
                drained.signal()
                return
            }
            let text = String(decoding: data, as: UTF8.self)
            self?.callbackQueue.async { self?.onOutput?(text) }
        }
        p.terminationHandler = { [weak self] proc in
            // What the child printed just before exiting can still be in the
            // pipe; wait for EOF so it reaches onOutput before onExit. Bounded,
            // because a forked grandchild (a JVM sbt forks) can hold the pipe
            // open long after the action itself has exited.
            _ = drained.wait(timeout: .now() + 1)
            self?.process = nil
            self?.callbackQueue.async { self?.onExit?(proc.terminationStatus) }
        }

        self.process = p
        do {
            try p.run()
        } catch {
            self.process = nil
            callbackQueue.async {
                self.onOutput?("✗ failed to start: \(error.localizedDescription)\n")
                self.onExit?(-1)
            }
        }
    }

    /// SIGTERM the child's whole process group, so sbt/gradle children die too.
    func stop() {
        guard let p = process, p.isRunning else { return }
        kill(-p.processIdentifier, SIGTERM)
        p.terminate()
    }
}

// MARK: - Log text view (selectable, ⌘C / ⌘A even from a floating panel)

final class LogTextView: NSTextView {
    // A nonactivating panel won't get keyboard events unless the app is active,
    // so clicking the log activates DevPanel and makes this view first responder.
    override func mouseDown(with event: NSEvent) {
        NSApp.activate(ignoringOtherApps: true)
        window?.makeFirstResponder(self)
        super.mouseDown(with: event)
    }

    // Backstop in case the menu's key equivalents don't fire for an accessory app.
    override func performKeyEquivalent(with event: NSEvent) -> Bool {
        if event.modifierFlags.contains(.command) {
            switch event.charactersIgnoringModifiers {
            case "c": if NSApp.sendAction(#selector(NSText.copy(_:)), to: nil, from: self) { return true }
            case "a": if NSApp.sendAction(#selector(NSText.selectAll(_:)), to: nil, from: self) { return true }
            default: break
            }
        }
        return super.performKeyEquivalent(with: event)
    }
}

// MARK: - A collapsible console (controls row + log view + a runner)

private let minLogHeight: CGFloat = 180

final class ConsoleView: NSObject {
    let container = NSStackView()
    private let disclosure = NSButton()
    private let status = NSTextField(labelWithString: "idle")
    private let stopButton = NSButton()
    private let textView = LogTextView()
    private let scroll = NSScrollView()
    private(set) var isExpanded = false
    private var runner: CommandRunner?

    private let clearsOnRun: Bool
    private let titleText: String
    var onLayoutChange: (() -> Void)?
    /// Absolute path to the dir holding devpanel.py. Set by the app delegate after init.
    var scriptsDir = ""

    init(title: String, clearsOnRun: Bool) {
        self.clearsOnRun = clearsOnRun
        self.titleText = title
        super.init()

        // A clearly-labelled fold header: "▸ Web output" / "▾ Web output".
        disclosure.isBordered = false
        disclosure.bezelStyle = .inline
        disclosure.alignment = .left
        disclosure.font = .systemFont(ofSize: 11, weight: .medium)
        disclosure.contentTintColor = .secondaryLabelColor
        disclosure.target = self
        disclosure.action = #selector(toggle)
        disclosure.setContentHuggingPriority(.required, for: .horizontal)
        updateDisclosureTitle()

        status.font = .systemFont(ofSize: 10)
        status.textColor = .secondaryLabelColor
        status.setContentHuggingPriority(.required, for: .horizontal)

        stopButton.title = "Stop"
        stopButton.bezelStyle = .inline
        stopButton.controlSize = .small
        stopButton.font = .systemFont(ofSize: 10)
        stopButton.target = self
        stopButton.action = #selector(stopRunning)
        stopButton.isHidden = true
        stopButton.setContentHuggingPriority(.required, for: .horizontal)

        let spacer = NSView()
        spacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        let controls = NSStackView(views: [disclosure, status, spacer, stopButton])
        controls.orientation = .horizontal
        controls.distribution = .fill
        controls.alignment = .centerY

        textView.isEditable = false
        textView.isSelectable = true
        textView.font = .monospacedSystemFont(ofSize: 11, weight: .regular)
        textView.backgroundColor = NSColor(white: 0.10, alpha: 1)
        textView.textColor = NSColor(white: 0.92, alpha: 1)
        textView.textContainerInset = NSSize(width: 6, height: 6)
        textView.isVerticallyResizable = true
        textView.isHorizontallyResizable = false
        textView.autoresizingMask = [.width]
        textView.minSize = NSSize(width: 0, height: 0)
        textView.maxSize = NSSize(width: 1e7, height: 1e7)
        textView.textContainer?.widthTracksTextView = true
        textView.textContainer?.containerSize = NSSize(width: 0, height: 1e7)

        scroll.documentView = textView
        scroll.hasVerticalScroller = true
        scroll.borderType = .bezelBorder
        scroll.setContentHuggingPriority(.defaultLow, for: .vertical)
        scroll.heightAnchor.constraint(greaterThanOrEqualToConstant: minLogHeight).isActive = true

        container.orientation = .vertical
        container.alignment = .leading     // overridden to .fill by the parent stack
        container.spacing = 6
        container.addArrangedSubview(controls)
        container.addArrangedSubview(scroll)
        controls.leadingAnchor.constraint(equalTo: container.leadingAnchor).isActive = true
        controls.trailingAnchor.constraint(equalTo: container.trailingAnchor).isActive = true
        scroll.leadingAnchor.constraint(equalTo: container.leadingAnchor).isActive = true
        scroll.trailingAnchor.constraint(equalTo: container.trailingAnchor).isActive = true
        scroll.isHidden = true
    }

    func run(actionName: String, label: String, repoRoot: String?) {
        runner?.stop()
        if clearsOnRun { textView.string = "" }
        setExpanded(true)
        let where_ = repoRoot.map { " @ \(($0 as NSString).lastPathComponent)" } ?? ""
        append("\n── \(label)\(where_) ─────────────\n")
        status.stringValue = "running…"
        status.textColor = .secondaryLabelColor
        stopButton.isHidden = false

        let r = CommandRunner()
        r.onOutput = { [weak self] in self?.append($0) }
        r.onExit = { [weak self] code in
            self?.stopButton.isHidden = true
            self?.status.stringValue = code == 0 ? "done ✓" : "exited \(code)"
            self?.status.textColor = code == 0 ? .systemGreen : .systemRed
        }
        self.runner = r
        let env = repoRoot.map { ["DEVPANEL_REPO_ROOT": $0] }
        r.run(executable: "/bin/bash", arguments: ["-l"] + devpanelArguments(scriptsDir, actionName), environment: env)
    }

    func stop() { doStop() }

    @objc private func stopRunning() { doStop() }

    private func doStop() {
        runner?.stop()
    }

    @objc private func toggle() { setExpanded(!isExpanded) }

    func setOpen(_ on: Bool) { if isExpanded != on { setExpanded(on) } }

    private func setExpanded(_ on: Bool) {
        isExpanded = on
        scroll.isHidden = !on
        updateDisclosureTitle()
        onLayoutChange?()
    }

    private func updateDisclosureTitle() {
        disclosure.title = (isExpanded ? "▾ " : "▸ ") + titleText
    }

    private func append(_ text: String) {
        let clean = text.replacingOccurrences(
            of: "\u{1B}\\[[0-9;]*m", with: "", options: .regularExpression)
        textView.textStorage?.append(NSAttributedString(
            string: clean,
            attributes: [
                .font: NSFont.monospacedSystemFont(ofSize: 11, weight: .regular),
                .foregroundColor: NSColor(white: 0.92, alpha: 1),
            ]))
        textView.scrollToEndOfDocument(nil)
    }
}

// MARK: - Actions

private enum Console { case web, device }

/// The bash arguments that run one devpanel.py action. A login shell (`-l`, add
/// it in front) so the action sees the terminal's PATH/SDK setup (sbt, java); the
/// system python so it runs without Homebrew. The script path and the action go
/// in as positional parameters, never spliced into the command text, so a
/// checkout path holding a space, a quote or `$(…)` is a path, not shell.
func devpanelArguments(_ scriptsDir: String, _ actionName: String) -> [String] {
    let script = (scriptsDir as NSString).appendingPathComponent("devpanel.py")
    return ["-c", "exec /usr/bin/python3 \"$1\" \"$2\"", "devpanel", script, actionName]
}

private struct Action {
    let title: String
    let subtitle: String
    let actionName: String
    let console: Console
}

/// The palette's buttons, top to bottom: one per devpanel.py action.
private let allActions: [Action] = [
    Action(title: "Android → device", subtitle: "build · install · launch",
           actionName: "deploy-android", console: .device),
    Action(title: "iOS → device", subtitle: "build · install · launch",
           actionName: "deploy-ios", console: .device),
    Action(title: "Web server", subtitle: "sbt web/run · :9000",
           actionName: "run-web", console: .web),
    Action(title: "Kill web", subtitle: "free :9000",
           actionName: "kill-web", console: .web),
]

private let defaultExpandedWidth: CGFloat = 380

// MARK: - App

final class AppDelegate: NSObject, NSApplicationDelegate, NSWindowDelegate {
    private var panel: NSPanel!
    private var statusItem: NSStatusItem!
    private let scriptsDir: String =
        (Bundle.main.object(forInfoDictionaryKey: "DevPanelScriptsDir") as? String) ?? ""
    private lazy var repoRoot: String =
        URL(fileURLWithPath: scriptsDir)
            .deletingLastPathComponent().deletingLastPathComponent()
            .deletingLastPathComponent().path

    private let webConsole = ConsoleView(title: "Web output", clearsOnRun: false)
    private let deviceConsole = ConsoleView(title: "Device output", clearsOnRun: true)
    private var suppressClick: Set<String> = []
    private var expandedWidth = defaultExpandedWidth
    private var relayouting = false

    func applicationDidFinishLaunching(_ note: Notification) {
        installMenu()
        installStatusItem()
        webConsole.scriptsDir = scriptsDir
        deviceConsole.scriptsDir = scriptsDir

        let content = NSStackView()
        content.orientation = .vertical
        content.alignment = .leading
        content.spacing = 8
        content.translatesAutoresizingMaskIntoConstraints = false

        content.addArrangedSubview(headerRow())
        for action in allActions { content.addArrangedSubview(button(for: action)) }
        content.addArrangedSubview(deviceConsole.container)
        content.addArrangedSubview(webConsole.container)
        // Every row fills the content width, so buttons stay equal and stretch
        // when the window is resized (instead of floating at intrinsic width).
        for v in content.arrangedSubviews {
            v.widthAnchor.constraint(equalTo: content.widthAnchor).isActive = true
        }
        webConsole.onLayoutChange = { [weak self] in self?.relayout() }
        deviceConsole.onLayoutChange = { [weak self] in self?.relayout() }

        let root = NSView()
        root.addSubview(content)
        NSLayoutConstraint.activate([
            content.leadingAnchor.constraint(equalTo: root.leadingAnchor, constant: 12),
            content.trailingAnchor.constraint(equalTo: root.trailingAnchor, constant: -12),
            content.topAnchor.constraint(equalTo: root.topAnchor, constant: 12),
            content.bottomAnchor.constraint(equalTo: root.bottomAnchor, constant: -12),
        ])

        // Standard titled panel (not HUD) so all three macOS window buttons —
        // close, minimize, zoom — are shown.
        let panel = NSPanel(
            contentRect: NSRect(x: 0, y: 0, width: defaultExpandedWidth, height: 10),
            styleMask: [.titled, .closable, .miniaturizable, .resizable, .nonactivatingPanel],
            backing: .buffered, defer: false)
        panel.title = "movies"
        // An always-on-top floating panel can't use the OS's native minimize /
        // zoom (and as an accessory app there's no Dock icon to minimize into),
        // so the yellow + green buttons drive floating-palette equivalents:
        // minimize → hide the panel entirely (bring it back from the menu-bar
        // ☰ icon), zoom → open both consoles.
        panel.standardWindowButton(.closeButton)?.isHidden = false
        if let mini = panel.standardWindowButton(.miniaturizeButton) {
            mini.isHidden = false
            mini.target = self
            mini.action = #selector(hidePanel)
        }
        if let zoom = panel.standardWindowButton(.zoomButton) {
            zoom.isHidden = false
            zoom.target = self
            zoom.action = #selector(zoomPanel)
        }
        panel.isFloatingPanel = true
        panel.level = .floating
        panel.hidesOnDeactivate = false
        panel.becomesKeyOnlyIfNeeded = false
        panel.isMovableByWindowBackground = true
        panel.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary]
        panel.delegate = self
        panel.contentView = root
        self.panel = panel

        relayout()
        // Top-right corner, close to the right edge, same top offset as before.
        let vf = (panel.screen ?? NSScreen.main)?.visibleFrame ?? NSRect(x: 0, y: 0, width: 1440, height: 900)
        panel.setFrameTopLeftPoint(NSPoint(x: vf.maxX - panel.frame.width - 12, y: vf.maxY - 40))
        panel.orderFrontRegardless()
    }

    // MARK: views

    private func headerRow() -> NSView {
        // The title bar now carries the close button, so no custom ✕ here.
        let label = NSTextField(labelWithString: "kinowo dev")
        label.font = .systemFont(ofSize: 11, weight: .semibold)
        label.textColor = .secondaryLabelColor
        label.setContentHuggingPriority(.required, for: .horizontal)

        let spacer = NSView()
        spacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        var views: [NSView] = [label, spacer]

        // The Mac's LAN IP + a copy button, so the dev server running here is one
        // glance away from being opened on a phone on the same Wi-Fi. Hidden when
        // no LAN address is bound (e.g. Wi-Fi off).
        if let ip = LocalHostIp.current() {
            let ipField = NSTextField(labelWithString: ip)
            ipField.font = .monospacedSystemFont(ofSize: 11, weight: .regular)
            ipField.textColor = .secondaryLabelColor
            ipField.isSelectable = true
            ipField.toolTip = "LAN IP of this Mac — open the dev server from your phone"
            ipField.setContentHuggingPriority(.required, for: .horizontal)

            let copy = NSButton(title: "⧉", target: self, action: #selector(copyLanIp(_:)))
            copy.bezelStyle = .roundRect
            copy.font = .systemFont(ofSize: 11)
            copy.identifier = NSUserInterfaceItemIdentifier(ip)
            copy.toolTip = "Copy IP to clipboard"
            copy.setContentHuggingPriority(.required, for: .horizontal)
            copy.setContentCompressionResistancePriority(.required, for: .horizontal)

            views.append(ipField)
            views.append(copy)
        }

        let row = NSStackView(views: views)
        row.orientation = .horizontal
        row.distribution = .fill
        row.spacing = 6
        return row
    }

    /// Copy the LAN IP (carried on the button's identifier) to the clipboard,
    /// flashing a ✓ for a beat as confirmation.
    @objc private func copyLanIp(_ sender: NSButton) {
        guard let ip = sender.identifier?.rawValue else { return }
        NSPasteboard.general.clearContents()
        NSPasteboard.general.setString(ip, forType: .string)
        let original = sender.title
        sender.title = "✓"
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.2) { [weak sender] in
            sender?.title = original
        }
    }

    // The close traffic-light quits the app (accessory app — no Dock icon to
    // reopen a merely-closed window from).
    func windowWillClose(_ notification: Notification) { NSApp.terminate(nil) }

    // Yellow: hide the whole panel (it's an accessory app, so there's no Dock
    // icon to minimize into — the menu-bar ☰ icon brings it back). Green: open
    // both consoles (toggle).
    @objc private func hidePanel() { panel.orderOut(nil) }

    @objc private func zoomPanel() {
        let open = !(webConsole.isExpanded && deviceConsole.isExpanded)
        webConsole.setOpen(open)
        deviceConsole.setOpen(open)
    }

    /// Centred two-line title (title bold + subtitle muted). Centred text →
    /// left padding always equals right padding, symmetric as the button
    /// stretches with the window.
    private func twoLineTitle(_ title: String, _ subtitle: String) -> NSAttributedString {
        let para = NSMutableParagraphStyle()
        para.alignment = .center
        let s = NSMutableAttributedString(
            string: title + "\n",
            attributes: [.font: NSFont.systemFont(ofSize: 13, weight: .medium),
                         .foregroundColor: NSColor.labelColor,
                         .paragraphStyle: para])
        s.append(NSAttributedString(
            string: subtitle,
            attributes: [.font: NSFont.systemFont(ofSize: 10),
                         .foregroundColor: NSColor.secondaryLabelColor,
                         .paragraphStyle: para]))
        return s
    }

    /// A 44pt palette button with a two-line centred title and a long-press
    /// gesture (the worktree picker).
    private func button(for action: Action) -> NSButton {
        let b = NSButton(title: "", target: self, action: #selector(run(_:)))
        b.attributedTitle = twoLineTitle(action.title, action.subtitle)
        b.bezelStyle = .regularSquare
        b.alignment = .center
        b.imagePosition = .noImage
        b.heightAnchor.constraint(equalToConstant: 44).isActive = true
        let g = NSPressGestureRecognizer(target: self, action: #selector(longPress(_:)))
        g.minimumPressDuration = 0.4
        b.addGestureRecognizer(g)
        b.identifier = NSUserInterfaceItemIdentifier(action.actionName)
        b.toolTip = "Click to run · long-press or right-click to pick a worktree"
        return b
    }

    private func installMenu() {
        let main = NSMenu()

        let appItem = NSMenuItem()
        main.addItem(appItem)
        let appMenu = NSMenu()
        appMenu.addItem(withTitle: "Quit DevPanel", action: #selector(quit), keyEquivalent: "q")
        appItem.submenu = appMenu

        let editItem = NSMenuItem()
        main.addItem(editItem)
        let edit = NSMenu(title: "Edit")
        edit.addItem(withTitle: "Cut", action: #selector(NSText.cut(_:)), keyEquivalent: "x")
        edit.addItem(withTitle: "Copy", action: #selector(NSText.copy(_:)), keyEquivalent: "c")
        edit.addItem(withTitle: "Paste", action: #selector(NSText.paste(_:)), keyEquivalent: "v")
        edit.addItem(withTitle: "Select All", action: #selector(NSText.selectAll(_:)), keyEquivalent: "a")
        editItem.submenu = edit

        NSApp.mainMenu = main
    }

    // MARK: menu-bar status item

    /// A menu-bar (☰) icon so the panel can be hidden out of the way and brought
    /// back — there's no Dock icon (accessory app). Left-click toggles the panel;
    /// right/control-click opens a tiny menu (also the only way to quit while the
    /// panel is hidden).
    private func installStatusItem() {
        let item = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        if let button = item.button {
            if let img = NSImage(systemSymbolName: "slider.horizontal.3",
                                 accessibilityDescription: "DevPanel") {
                img.isTemplate = true
                button.image = img
            } else {
                button.title = "☰"
            }
            button.toolTip = "DevPanel — click to show/hide"
            button.target = self
            button.action = #selector(statusItemClicked)
            button.sendAction(on: [.leftMouseUp, .rightMouseUp])
        }
        self.statusItem = item
    }

    @objc private func statusItemClicked() {
        let event = NSApp.currentEvent
        let isSecondary = event?.type == .rightMouseUp
            || (event?.modifierFlags.contains(.control) ?? false)
        if isSecondary {
            let menu = NSMenu()
            let toggle = NSMenuItem(title: panel.isVisible ? "Hide DevPanel" : "Show DevPanel",
                                    action: #selector(togglePanel), keyEquivalent: "")
            toggle.target = self
            menu.addItem(toggle)
            menu.addItem(.separator())
            let quitItem = NSMenuItem(title: "Quit DevPanel", action: #selector(quit), keyEquivalent: "q")
            quitItem.target = self
            menu.addItem(quitItem)
            // Attach the menu just for this click, then detach so a plain
            // left-click keeps toggling instead of opening the menu.
            statusItem.menu = menu
            statusItem.button?.performClick(nil)
            statusItem.menu = nil
        } else {
            togglePanel()
        }
    }

    /// Show the panel where it was if hidden, otherwise hide it.
    @objc private func togglePanel() {
        if panel.isVisible {
            panel.orderOut(nil)
        } else {
            panel.orderFrontRegardless()
        }
    }

    // MARK: running

    @objc private func run(_ sender: NSButton) {
        guard let actionName = sender.identifier?.rawValue else { return }
        if suppressClick.remove(actionName) != nil { return }   // long-press already handled it
        start(actionName: actionName, repoRoot: nil)
    }

    private func start(actionName: String, repoRoot: String?) {
        guard !scriptsDir.isEmpty, let action = allActions.first(where: { $0.actionName == actionName }) else {
            NSSound.beep(); return
        }
        let console = action.console == .web ? webConsole : deviceConsole
        console.run(actionName: actionName, label: action.title, repoRoot: repoRoot)
    }

    // MARK: worktree picker

    @objc private func longPress(_ gr: NSPressGestureRecognizer) {
        guard gr.state == .began, let button = gr.view as? NSButton,
              let actionName = button.identifier?.rawValue else { return }
        suppressClick.insert(actionName)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.6) { self.suppressClick.remove(actionName) }
        worktreeMenu(forAction: actionName).popUp(positioning: nil, at: gr.location(in: button), in: button)
    }

    @objc private func runOnWorktree(_ item: NSMenuItem) {
        guard let info = item.representedObject as? [String: String], let actionName = info["action"] else { return }
        let root = (info["root"]?.isEmpty == false) ? info["root"] : nil
        start(actionName: actionName, repoRoot: root)
    }

    private func worktreeMenu(forAction actionName: String) -> NSMenu {
        let menu = NSMenu()
        let header = NSMenuItem(title: "Run on worktree:", action: nil, keyEquivalent: "")
        header.isEnabled = false
        menu.addItem(header)
        let trees = worktrees()
        for wt in trees {
            let isMain = wt.path == repoRoot
            let item = NSMenuItem(title: isMain ? "\(wt.name)  (main)" : wt.name,
                                  action: #selector(runOnWorktree(_:)), keyEquivalent: "")
            item.target = self
            item.representedObject = ["action": actionName, "root": isMain ? "" : wt.path]
            menu.addItem(item)
        }
        if trees.isEmpty {
            let none = NSMenuItem(title: "(no worktrees found)", action: nil, keyEquivalent: "")
            none.isEnabled = false
            menu.addItem(none)
        }
        return menu
    }

    private func worktrees() -> [(name: String, path: String)] {
        let out = runGit(["-C", repoRoot, "worktree", "list", "--porcelain"])
        return out.split(separator: "\n").compactMap { line in
            guard line.hasPrefix("worktree ") else { return nil }
            let path = String(line.dropFirst("worktree ".count))
            return ((path as NSString).lastPathComponent, path)
        }
    }

    private func runGit(_ args: [String]) -> String {
        let p = Process()
        p.executableURL = URL(fileURLWithPath: "/usr/bin/git")
        p.arguments = args
        let pipe = Pipe()
        p.standardOutput = pipe
        p.standardError = Pipe()
        guard (try? p.run()) != nil else { return "" }
        let data = pipe.fileHandleForReading.readDataToEndOfFile()
        p.waitUntilExit()
        return String(decoding: data, as: UTF8.self)
    }

    // MARK: sizing

    /// Folded (both consoles closed) → fixed narrow size that just fits the
    /// buttons. Expanded → freely resizable, width remembered. The panel grows
    /// downward from its top-left and is kept within the visible screen.
    private func relayout() {
        guard let root = panel.contentView else { return }
        let anyExpanded = webConsole.isExpanded || deviceConsole.isExpanded
        let fit = root.fittingSize
        let topLeft = NSPoint(x: panel.frame.minX, y: panel.frame.maxY)
        relayouting = true
        if anyExpanded {
            panel.contentMinSize = NSSize(width: fit.width, height: fit.height)
            panel.contentMaxSize = NSSize(width: 4000, height: 4000)
            let w = max(expandedWidth, fit.width)
            let h = max(root.frame.height, fit.height)
            panel.setContentSize(NSSize(width: w, height: h))
        } else {
            panel.contentMinSize = fit               // lock to the narrow size
            panel.contentMaxSize = fit
            panel.setContentSize(fit)
        }
        panel.setFrameTopLeftPoint(topLeft)          // grow downward, not upward
        clampToScreen()
        relayouting = false
    }

    /// Slide (and if necessary shrink) the panel so it stays fully on screen.
    private func clampToScreen() {
        guard let vf = (panel.screen ?? NSScreen.main)?.visibleFrame else { return }
        var f = panel.frame
        f.size.width = min(f.size.width, vf.width)
        f.size.height = min(f.size.height, vf.height)
        if f.maxX > vf.maxX { f.origin.x = vf.maxX - f.width }
        if f.minX < vf.minX { f.origin.x = vf.minX }
        if f.maxY > vf.maxY { f.origin.y = vf.maxY - f.height }
        if f.minY < vf.minY { f.origin.y = vf.minY }
        if f != panel.frame { panel.setFrame(f, display: true) }
    }

    func windowDidResize(_ notification: Notification) {
        guard !relayouting, webConsole.isExpanded || deviceConsole.isExpanded,
              let w = panel.contentView?.frame.width else { return }
        expandedWidth = w                            // remember the hand-set width
    }

    @objc private func quit() {
        webConsole.stop()
        deviceConsole.stop()
        NSApp.terminate(nil)
    }
}

// MARK: - Entry point

// Headless self-test: drives the real CommandRunner — output streaming AND the
// environment passthrough the worktree picker relies on — then exits 0/1. Lets
// test_devpanel.py verify the runtime path without a GUI/click.
if ProcessInfo.processInfo.environment["DEVPANEL_SELFTEST"] == "1" {
    func runOnce(_ exec: String, _ args: [String], _ env: [String: String]?) -> (String, Int32) {
        let q = DispatchQueue(label: "devpanel.selftest")
        let r = CommandRunner(callbackQueue: q)
        var out = ""; var st: Int32 = -999
        let done = DispatchSemaphore(value: 0)
        r.onOutput = { out += $0 }
        r.onExit = { st = $0; done.signal() }
        r.run(executable: exec, arguments: args, environment: env)
        _ = done.wait(timeout: .now() + 10)
        return (out, st)
    }

    let (o1, s1) = runOnce("/bin/sh", ["-c", "printf 'SELFTEST_OK\\n'"], nil)
    // Repeated: output printed right before exit used to be dropped when the
    // exit won the race against the pipe reader (rarely, hence the repeats).
    var (o2, s2) = ("", Int32(0))
    for _ in 0..<200 {
        (o2, s2) = runOnce("/bin/sh", ["-c", "printf 'ROOT=%s\\n' \"$DEVPANEL_REPO_ROOT\""],
                           ["DEVPANEL_REPO_ROOT": "/tmp/devpanel-selftest-root"])
        if s2 != 0 || !o2.contains("ROOT=/tmp/devpanel-selftest-root") { break }
    }

    // The real action command line reaches devpanel.py (an unknown action
    // prints its usage and exits 2), when the test passes the scripts dir.
    let scriptsDir = ProcessInfo.processInfo.environment["DEVPANEL_SELFTEST_SCRIPTS"] ?? ""
    let (o3, s3) = runOnce("/bin/bash", devpanelArguments(scriptsDir, "bogus"), nil)
    // A scripts dir that is shell syntax stays a path: nothing runs, python just
    // finds no such file (exit 2, its own "can't open file" message).
    let marker = "/tmp/devpanel-selftest-injected-\(getpid())"
    let (_, s4) = runOnce("/bin/bash", devpanelArguments("/nonexistent/$(touch \(marker))", "bogus"), nil)
    let injectionOK = s4 == 2 && !FileManager.default.fileExists(atPath: marker)
    let streamOK = s1 == 0 && o1.contains("SELFTEST_OK")
        && s2 == 0 && o2.contains("ROOT=/tmp/devpanel-selftest-root")
        && s3 == 2 && o3.contains("usage: devpanel.py")

    // LAN-IP selection rule: site-local wins over a public address regardless of
    // order, loopback is never offered, and a lone non-loopback is the fallback.
    let ipOK =
        LocalHostIp.pick(["8.8.8.8", "192.168.1.5"]) == "192.168.1.5"
        && LocalHostIp.pick(["8.8.8.8", "10.0.0.4"]) == "10.0.0.4"
        && LocalHostIp.pick(["172.20.0.3"]) == "172.20.0.3"
        && LocalHostIp.pick(["172.32.0.1"]) == "172.32.0.1"   // 172.32 isn't site-local → fallback
        && LocalHostIp.pick(["8.8.8.8"]) == "8.8.8.8"
        && LocalHostIp.pick(["127.0.0.1", "127.0.0.53"]) == nil
        && LocalHostIp.pick([]) == nil
        && LocalHostIp.isSiteLocal("172.16.0.1") && LocalHostIp.isSiteLocal("172.31.9.9")
        && !LocalHostIp.isSiteLocal("172.15.0.1") && !LocalHostIp.isSiteLocal("172.32.0.1")

    let ok = streamOK && injectionOK && ipOK
    print(ok ? "SELFTEST_OK stream+env+ip status=\(s1),\(s2)"
             : "SELFTEST_FAIL stream=\(streamOK) injection=\(injectionOK) ip=\(ipOK) "
               + "o1=\(o1.debugDescription) o2=\(o2.debugDescription) o3=\(o3.debugDescription) st=\(s1),\(s2),\(s3)")
    exit(ok ? 0 : 1)
}

let app = NSApplication.shared
app.setActivationPolicy(.accessory)
let delegate = AppDelegate()
app.delegate = delegate
app.run()
