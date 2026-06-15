// DevPanel — a small always-on-top floating palette of dev actions for the
// movies repo.
//
// Every action runs its script as a background subprocess, streaming live
// output into an in-panel console. There are two consoles:
//   • Web    — shared by the two server actions; KEEPS its scrollback across
//              runs (long-lived servers you want to keep watching).
//   • Device — shared by Android + iOS; CLEARED at the start of each run.
// Each console is independently collapsible. When BOTH are collapsed the panel
// shrinks to a fixed narrow size (just the buttons); when one is open the panel
// is freely resizable by hand, and its width is remembered. Log text is
// selectable and ⌘C copies it.
//
// Long-press (or right-click) any button to pick which git worktree to run the
// task in; the chosen path is handed to the script via DEVPANEL_REPO_ROOT.
//
// The absolute scripts directory is baked into Info.plist (DevPanelScriptsDir)
// at build time, so the .app keeps working if moved out of the repo tree.

import AppKit

// MARK: - Command runner (streams a subprocess)

/// Runs a shell script as a child process, streaming merged stdout+stderr to
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

        pipe.fileHandleForReading.readabilityHandler = { [weak self] handle in
            let data = handle.availableData
            guard !data.isEmpty else { return }
            let text = String(decoding: data, as: UTF8.self)
            self?.callbackQueue.async { self?.onOutput?(text) }
        }
        p.terminationHandler = { [weak self] proc in
            pipe.fileHandleForReading.readabilityHandler = nil
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
    private let reapsWorkerOnStop: Bool
    private let titleText: String
    var onLayoutChange: (() -> Void)?
    /// Absolute path to the action scripts dir (for reaping the fixture worker
    /// via lib.sh's kill_stale_worker). Set by the app delegate after init.
    var scriptsDir = ""

    init(title: String, clearsOnRun: Bool, reapsWorkerOnStop: Bool = false) {
        self.clearsOnRun = clearsOnRun
        self.reapsWorkerOnStop = reapsWorkerOnStop
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

    func run(scriptPath: String, label: String, repoRoot: String?) {
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
        r.run(executable: "/bin/bash", arguments: ["-lc", "exec bash \"\(scriptPath)\""], environment: env)
    }

    func stop() { doStop() }

    @objc private func stopRunning() { doStop() }

    /// Stop the running action AND, for the web console, reap a stale fixture
    /// worker. The web+worker stack forks the worker (LocalFixtureWorkerMain)
    /// into its own process tree via sbt's bgRunMain, so SIGTERM-ing the
    /// script's process group leaves it running; kill_stale_worker (lib.sh)
    /// pattern-kills it the same way the action scripts do at launch.
    private func doStop() {
        runner?.stop()
        if reapsWorkerOnStop { reapStaleWorker() }
    }

    private func reapStaleWorker() {
        guard !scriptsDir.isEmpty else { return }
        let lib = (scriptsDir as NSString).appendingPathComponent("lib.sh")
        let p = Process()
        p.executableURL = URL(fileURLWithPath: "/bin/bash")
        p.arguments = ["-lc", "source \"\(lib)\"; kill_stale_worker"]
        let pipe = Pipe()
        p.standardOutput = pipe
        p.standardError = pipe
        pipe.fileHandleForReading.readabilityHandler = { [weak self] handle in
            let data = handle.availableData
            guard !data.isEmpty else { return }
            let text = String(decoding: data, as: UTF8.self)
            DispatchQueue.main.async { self?.append(text) }
        }
        p.terminationHandler = { _ in pipe.fileHandleForReading.readabilityHandler = nil }
        try? p.run()
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

private struct Action {
    let title: String
    let subtitle: String
    let script: String
    let console: Console
}

private let actions: [Action] = [
    Action(title: "Android → device", subtitle: "build · install · launch",
           script: "deploy-android.sh", console: .device),
    Action(title: "iOS → device", subtitle: "build · install · launch",
           script: "deploy-ios.sh", console: .device),
    Action(title: "Web server", subtitle: "sbt web/run · :9000",
           script: "run-web.sh", console: .web),
    Action(title: "Web + worker", subtitle: "sbt localStack · fixtures",
           script: "run-local-stack.sh", console: .web),
    Action(title: "Kill web + worker", subtitle: "free :9000 · reap worker",
           script: "kill-stack.sh", console: .web),
    Action(title: "Reset local corpus", subtitle: "drop kinowo_local · re-scrape",
           script: "reset-local-corpus.sh", console: .web),
]

private let defaultExpandedWidth: CGFloat = 380

// MARK: - App

final class AppDelegate: NSObject, NSApplicationDelegate, NSWindowDelegate {
    private var panel: NSPanel!
    private let scriptsDir: String =
        (Bundle.main.object(forInfoDictionaryKey: "DevPanelScriptsDir") as? String) ?? ""
    private lazy var repoRoot: String =
        URL(fileURLWithPath: scriptsDir)
            .deletingLastPathComponent().deletingLastPathComponent()
            .deletingLastPathComponent().path

    private let webConsole = ConsoleView(title: "Web output", clearsOnRun: false, reapsWorkerOnStop: true)
    private let deviceConsole = ConsoleView(title: "Device output", clearsOnRun: true)
    private var suppressClick: Set<String> = []
    private var expandedWidth = defaultExpandedWidth
    private var relayouting = false

    func applicationDidFinishLaunching(_ note: Notification) {
        installMenu()
        webConsole.scriptsDir = scriptsDir

        let content = NSStackView()
        content.orientation = .vertical
        content.alignment = .leading
        content.spacing = 8
        content.translatesAutoresizingMaskIntoConstraints = false

        content.addArrangedSubview(headerRow())
        for action in actions { content.addArrangedSubview(button(for: action)) }
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
        // zoom, so the yellow + green buttons drive floating-palette equivalents:
        // minimize → collapse to the compact panel, zoom → open both consoles.
        panel.standardWindowButton(.closeButton)?.isHidden = false
        if let mini = panel.standardWindowButton(.miniaturizeButton) {
            mini.isHidden = false
            mini.target = self
            mini.action = #selector(minimizePanel)
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
        let row = NSStackView(views: [label, spacer])
        row.orientation = .horizontal
        row.distribution = .fill
        return row
    }

    // The close traffic-light quits the app (accessory app — no Dock icon to
    // reopen a merely-closed window from).
    func windowWillClose(_ notification: Notification) { NSApp.terminate(nil) }

    // Yellow: collapse to the compact panel. Green: open both consoles (toggle).
    @objc private func minimizePanel() {
        webConsole.setOpen(false)
        deviceConsole.setOpen(false)
    }

    @objc private func zoomPanel() {
        let open = !(webConsole.isExpanded && deviceConsole.isExpanded)
        webConsole.setOpen(open)
        deviceConsole.setOpen(open)
    }

    private func button(for action: Action) -> NSButton {
        // Centred text → left padding always equals right padding, and stays
        // symmetric as the button stretches with the window.
        let para = NSMutableParagraphStyle()
        para.alignment = .center
        let title = NSMutableAttributedString(
            string: action.title + "\n",
            attributes: [.font: NSFont.systemFont(ofSize: 13, weight: .medium),
                         .foregroundColor: NSColor.labelColor,
                         .paragraphStyle: para])
        title.append(NSAttributedString(
            string: action.subtitle,
            attributes: [.font: NSFont.systemFont(ofSize: 10),
                         .foregroundColor: NSColor.secondaryLabelColor,
                         .paragraphStyle: para]))

        let b = NSButton(title: "", target: self, action: #selector(run(_:)))
        b.attributedTitle = title
        b.identifier = NSUserInterfaceItemIdentifier(action.script)
        b.bezelStyle = .regularSquare
        b.alignment = .center
        b.imagePosition = .noImage
        b.toolTip = "Click to run · long-press or right-click to pick a worktree"
        b.heightAnchor.constraint(equalToConstant: 44).isActive = true

        let lp = NSPressGestureRecognizer(target: self, action: #selector(longPress(_:)))
        lp.minimumPressDuration = 0.4
        b.addGestureRecognizer(lp)
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

    // MARK: running

    @objc private func run(_ sender: NSButton) {
        guard let script = sender.identifier?.rawValue else { return }
        if suppressClick.remove(script) != nil { return }   // long-press already handled it
        start(script: script, repoRoot: nil)
    }

    private func start(script: String, repoRoot: String?) {
        guard !scriptsDir.isEmpty, let action = actions.first(where: { $0.script == script }) else {
            NSSound.beep(); return
        }
        let path = (scriptsDir as NSString).appendingPathComponent(script)
        let console = action.console == .web ? webConsole : deviceConsole
        console.run(scriptPath: path, label: action.title, repoRoot: repoRoot)
    }

    // MARK: worktree picker

    @objc private func longPress(_ gr: NSPressGestureRecognizer) {
        guard gr.state == .began, let button = gr.view as? NSButton,
              let script = button.identifier?.rawValue else { return }
        suppressClick.insert(script)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.6) { self.suppressClick.remove(script) }
        worktreeMenu(forScript: script).popUp(positioning: nil, at: gr.location(in: button), in: button)
    }

    @objc private func runOnWorktree(_ item: NSMenuItem) {
        guard let info = item.representedObject as? [String: String], let script = info["script"] else { return }
        let root = (info["root"]?.isEmpty == false) ? info["root"] : nil
        start(script: script, repoRoot: root)
    }

    private func worktreeMenu(forScript script: String) -> NSMenu {
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
            item.representedObject = ["script": script, "root": isMain ? "" : wt.path]
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
// test.sh verify the runtime path without a GUI/click.
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
    let (o2, s2) = runOnce("/bin/sh", ["-c", "printf 'ROOT=%s\\n' \"$DEVPANEL_REPO_ROOT\""],
                           ["DEVPANEL_REPO_ROOT": "/tmp/devpanel-selftest-root"])
    let ok = s1 == 0 && o1.contains("SELFTEST_OK")
        && s2 == 0 && o2.contains("ROOT=/tmp/devpanel-selftest-root")
    print(ok ? "SELFTEST_OK stream+env status=\(s1),\(s2)"
             : "SELFTEST_FAIL o1=\(o1.debugDescription) o2=\(o2.debugDescription) st=\(s1),\(s2)")
    exit(ok ? 0 : 1)
}

let app = NSApplication.shared
app.setActivationPolicy(.accessory)
let delegate = AppDelegate()
app.delegate = delegate
app.run()
