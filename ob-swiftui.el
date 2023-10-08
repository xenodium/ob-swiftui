;;; ob-swiftui.el --- Org babel functions for SwiftUI evaluation -*- lexical-binding: t; -*-

;; Copyright (C) Alvaro Ramirez

;; Author: Alvaro Ramirez
;; Package-Requires: ((emacs "25.1") (swift-mode "8.2.0") (org "9.2.0"))
;; URL: https://github.com/xenodium/ob-swiftui
;; Version: 0.8

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Run and render SwiftUI blocks using org babel.
;;
;; Install with:
;;
;;   (require 'ob-swiftui)
;;   (ob-swiftui-setup)
;;
;; Relevant header arguments:
;;
;; `:results' window
;;
;;   Runs SwiftUI in a separate window (default and can be omitted).
;;
;; `:results' file
;;
;;   Runs SwiftUI in the background and saves an image snapshot to
;;   a file.
;;
;; `:view' FooView
;;
;;   If `view:' is given, use FooView as the root view.  Otherwise,
;;   generate a root view and embed source block in body.
;;
;; Examples:
;;
;;   Use generated root view and render in external window (default):
;;
;;     #+begin_src swiftui
;;       Rectangle()
;;         .fill(Color.yellow)
;;         .frame(maxWidth: .infinity, maxHeight: .infinity)
;;     #+end_src
;;
;;     is equivalent to:
;;
;;     #+begin_src swiftui :results window :view none
;;       Rectangle()
;;         .fill(Color.yellow)
;;         .frame(maxWidth: .infinity, maxHeight: .infinity)
;;     #+end_src
;;
;;   Using your own root view:
;;
;;     #+begin_src swiftui :results window :view FooView
;;       struct FooView: View {
;;         var body: some View {
;;           VStack(spacing: 10){
;;             BarView()
;;             BazView()
;;           }
;;         }
;;       }
;;
;;       struct BarView: View {
;;         var body: some View {
;;           Rectangle()
;;             .fill(Color.yellow)
;;             .frame(maxWidth: .infinity, maxHeight: .infinity)
;;         }
;;       }
;;
;;       struct BazView: View {
;;         var body: some View {
;;           Rectangle()
;;             .fill(Color.blue)
;;             .frame(maxWidth: .infinity, maxHeight: .infinity)
;;         }
;;       }
;;     #+end_src

;;; Requirements:

;; Depends on `swift-mode' for editing Swift code.

;;; Code:
(require 'ob)
(require 'org)
(require 'swift-mode)
(require 'map)

;; Aliasing enables block syntax highlighting.
(defalias 'swiftui-mode #'swift-mode)

(defvar org-babel-default-header-args:swiftui
  '((:results . "window")
    (:view . "none")
    (:file . (lambda ()
               (make-temp-file "ob-swiftui-output-file-" nil ".png"))))
  "Default ob-swiftui header args.
Must be named `org-babel-default-header-args:swiftui' to integrate with `ob'.")

(defun org-babel-execute:swiftui (body params)
  "Execute a block of SwiftUI code in BODY with org-babel header PARAMS.
This function is called by `org-babel-execute-src-block'"
  (message "executing SwiftUI source code block")
  (let ((target (make-temp-file "ob-swiftui-exe-")))
    (with-temp-buffer
      (insert (ob-swiftui--expand-body body params))
      (shell-command-on-region
       (point-min)
       (point-max)
       (format "swiftc -o %s -" target)))
    (shell-command target))
    nil)

(defun ob-swiftui-setup ()
  "Set up babel SwiftUI support."
  (add-to-list 'org-babel-tangle-lang-exts '("swiftui" . "swift"))
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((swiftui . t))))
  (add-to-list 'org-src-lang-modes '("swiftui" . swift)))

(defun ob-swiftui--expand-body (body params)
  "Expand BODY according to PARAMS and PROCESSED-PARAMS, return the expanded body."
  (let ((write-to-file (member "file" (map-elt params :result-params)))
        (root-view (if (and (map-elt params :view)
                            (not (string-equal (map-elt params :view) "none")))
                       (map-elt params :view)
                     "ContentView")))
    (when (and (not (string-match root-view body))
               (or (string-match "struct" body)
                   (string-match "class" body)))
      (user-error "Either name one of the views ContentView or specify :view param."))
    (if write-to-file
        (let ((output-file (cdr (assoc :file params))))
          (when (functionp output-file)
            (setq output-file (funcall output-file)))
          (ob-swiftui--expand-body-preview body root-view output-file))
      (message (ob-swiftui--expand-body-window body root-view))
      (ob-swiftui--expand-body-window body root-view))))

(defun ob-swiftui--expand-body-preview (body root-view output-file)
  (format
     "
import SwiftUI

let timer = Timer.scheduledTimer(withTimeInterval: 0.1, repeats: false) { timer in
  Task.detached { @MainActor in
    let renderer = ImageRenderer(content: %s())
    renderer.scale = NSScreen.main?.backingScaleFactor ?? 1.0
    let data = renderer.cgImage?.pngData(compressionFactor: 1)
    do {
      let url = URL(fileURLWithPath: \"%s\")
      try data?.write(to: url)
      exit(0)
    } catch {
      print(\"Error: \\(error.localizedDescription)\")
      exit(1)
    }
  }
}

RunLoop.current.run()

extension CGImage {
  func pngData(compressionFactor: Float) -> Data? {
    NSBitmapImageRep(cgImage: self).representation(
      using: .png, properties: [NSBitmapImageRep.PropertyKey.compressionFactor: compressionFactor])
  }
}

// Additional view definitions.
%s
"
     root-view
     output-file
     (if (string-match root-view body)
         body
       (format "
struct ContentView: View {
  var body: some View {
    VStack{
      %s
    }
    .frame(maxWidth:.infinity, maxHeight:.infinity)
  }
}
" body))))

(defun ob-swiftui--expand-body-window (body root-view)
  (format
   "
// Swift snippet based on Chris Eidhof's code at:
// https://gist.github.com/chriseidhof/26768f0b63fa3cdf8b46821e099df5ff

import Cocoa
import Foundation
import SwiftUI

extension NSApplication {
  public func start() {
    let appDelegate = AppDelegate()
    NSApp.setActivationPolicy(.regular)
    mainMenu = customMenu
    delegate = appDelegate
    run()
  }
}

extension NSApplication {
  var customMenu: NSMenu {
    let appMenu = NSMenuItem()
    appMenu.submenu = NSMenu()

    let quitItem = NSMenuItem(
      title: \"Quit \(ProcessInfo.processInfo.processName)\",
      action: #selector(NSApplication.terminate(_:)), keyEquivalent: \"q\")
    quitItem.keyEquivalentModifierMask = []
    appMenu.submenu?.addItem(quitItem)

    let mainMenu = NSMenu(title: \"Main Menu\")
    mainMenu.addItem(appMenu)
    return mainMenu
  }
}

class AppDelegate: NSObject, NSApplicationDelegate, NSWindowDelegate {
  var window = NSWindow(
    contentRect: NSRect(x: 0, y: 0, width: 414 * 0.2, height: 896 * 0.2),
    styleMask: [.titled, .closable, .miniaturizable, .resizable, .fullSizeContentView],
    backing: .buffered, defer: false)

  var contentView = %s()

  func applicationDidFinishLaunching(_ notification: Notification) {
    window.delegate = self
    window.center()
    window.contentView = NSHostingView(rootView: contentView)
    window.makeKeyAndOrderFront(nil)
    window.title = \"press \\\"q\\\" to exit\"
    window.setFrameAutosaveName(\"Main Window\")
    NSApp.activate(ignoringOtherApps: true)
  }
}

NSApplication.shared.start()

// Additional view definitions.
%s
"
   root-view
   (if (string-match root-view body)
       body
     (format "
struct ContentView: View {
  var body: some View {
    VStack{
      %s
    }
    .frame(maxWidth:.infinity, maxHeight:.infinity)
  }
}
" body))))

(provide 'ob-swiftui)
;;; ob-swiftui.el ends here
