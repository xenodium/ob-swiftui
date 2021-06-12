;;; ob-swiftui.el --- org-babel functions for SwiftUI evaluation

;; Copyright (C) Alvaro Ramirez

;; Author: Alvaro Ramirez
;; Keywords: swiftui, literate programming, reproducible research
;; Homepage: https://github.com/xenodium/ob-swiftui
;; Version: 0.01

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
;; :results window
;;
;;   Runs SwiftUI in a separate window.
;;
;; :results graphics
;;
;;   Runs SwiftUI in the background and saves an image snapshot to
;;   a file to be rendered as a graphic.
;;
;; :view FooView
;;
;;   If given, use FooView as the root view. Otherwise, generate a
;;   root view and embed source block in body.
;;
;; For using your own root view:
;;
;;   #+begin_src swiftui :results window :view FooView
;;     struct FooView: View {
;;       var body: some View {
;;         VStack(spacing: 10) {
;;           HStack(spacing: 10) {
;;             Rectangle().fill(Color.yellow)
;;             Rectangle().fill(Color.green)
;;           }
;;           Rectangle().fill(Color.blue)
;;           HStack(spacing: 10) {
;;             Rectangle().fill(Color.green)
;;             Rectangle().fill(Color.yellow)
;;           }
;;         }
;;         .frame(maxWidth: .infinity, maxHeight: .infinity)
;;       }
;;     }
;;   #+end_src
;;
;; For generating a root view:
;;
;;   #+begin_src swiftui :results window :view none
;;     VStack(spacing: 10) {
;;         HStack(spacing: 10) {
;;           Rectangle().fill(Color.yellow)
;;           Rectangle().fill(Color.green)
;;         }
;;         Rectangle().fill(Color.blue)
;;         HStack(spacing: 10) {
;;           Rectangle().fill(Color.green)
;;           Rectangle().fill(Color.yellow)
;;         }
;;       }
;;       .frame(maxWidth: .infinity, maxHeight: .infinity)
;;   #+end_src

;;; Requirements:

;; Depends on `swift-mode' for editing SwiftUI Swift code.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'swift-mode)
(require 'map)

(defvar org-babel-default-header-args:swiftui '((:results . "window")
                                                (:view . "none")))

(defun ob-swiftui-setup ()
  "Set up babel SwiftUI support."
  (add-to-list 'org-babel-tangle-lang-exts '("swiftui" . "swift"))
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((swiftui . t))))
  (add-to-list 'org-src-lang-modes '("swiftui" . swift)))

(defun org-babel-expand-body:swiftui (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((results (map-elt params :result-params))
        (view-name (map-elt params :view)))
    (setq body (format
                "
// Snippet heavily based on Chris Eidhof's code at:
// https://gist.github.com/chriseidhof/26768f0b63fa3cdf8b46821e099df5ff

import Cocoa
import SwiftUI
import Foundation

let screenshotURL = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true).appendingPathComponent(ProcessInfo.processInfo.globallyUniqueString + \".png\")
let preview = %s

// Body to run.
%s

// Additional view definitions.
%s

extension NSApplication {
  public func run<V: View>(_ view: V) {
    let appDelegate = AppDelegate(view)
    NSApp.setActivationPolicy(.regular)
    mainMenu = customMenu
    delegate = appDelegate
    run()
  }

  public func run<V: View>(@ViewBuilder view: () -> V) {
    let appDelegate = AppDelegate(view())
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

class AppDelegate<V: View>: NSObject, NSApplicationDelegate, NSWindowDelegate {
  var window = NSWindow(
    contentRect: NSRect(x: 0, y: 0, width: 414 * 0.2, height: 896 * 0.2),
    styleMask: [.titled, .closable, .miniaturizable, .resizable, .fullSizeContentView],
    backing: .buffered, defer: false)

  var contentView: V

  init(_ contentView: V) {
    self.contentView = contentView
  }

  func applicationDidFinishLaunching(_ notification: Notification) {
    window.delegate = self
    window.center()
    window.contentView = NSHostingView(rootView: contentView)
    window.makeKeyAndOrderFront(nil)

    if preview {
      screenshot(view: window.contentView!, saveTo: screenshotURL)
      // Write path (without newline) so org babel can parse it.
      print(screenshotURL.path, terminator: \"\")
      NSApplication.shared.terminate(self)
      return
    }

    window.title = \"press q to exit\"
    window.setFrameAutosaveName(\"Main Window\")
    NSApp.activate(ignoringOtherApps: true)
  }
}

func screenshot(view: NSView, saveTo fileURL: URL) {
  let rep = view.bitmapImageRepForCachingDisplay(in: view.bounds)!
  view.cacheDisplay(in: view.bounds, to: rep)
  let pngData = rep.representation(using: .png, properties: [:])
  try! pngData?.write(to: fileURL)
}
"
                (cond ((seq-contains-p results "graphics")
                       "true")
                      ((seq-contains-p results "window")
                       "false")
                      (t
                       (user-error ":results must be either window or graphics")))
                (cond ((or (not view-name)
                           (string-equal view-name "none"))
                       (format "NSApplication.shared.run {%s}" body))
                      (t
                       (format "NSApplication.shared.run(%s())" view-name)))
                (cond ((or (not view-name)
                           (string-equal view-name "none"))
                       "")
                      (t
                       body))))))

(defun org-babel-execute:swiftui (body params)
  "Execute a block of SwiftUI code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing SwiftUI source code block")
  (let ((processed-params (org-babel-process-params params)))
    (with-temp-buffer
      (insert (org-babel-expand-body:swiftui body params processed-params))
      (print (org-babel-expand-body:swiftui body params processed-params))
      (shell-command-on-region
       (point-min)
       (point-max)
       "swift -" nil 't)
      (buffer-string))))

(provide 'ob-swiftui)
;;; ob-swiftui.el ends here
