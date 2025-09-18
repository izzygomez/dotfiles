# macOS Keyboard Shortcuts

Custom keyboard shortcuts providing system-wide & app-specific shortcuts across macOS.

## Overview

This directory contains two complementary configuration systems to ensure consistent keyboard behavior across all applications:

- **Karabiner-Elements** (`karabiner/karabiner.json`) - System-wide keyboard modifications
- **DefaultKeyBinding.dict** - Native macOS Cocoa text field bindings

Rules should be implemented in **both** locations to maximize compatibility:

- Karabiner-Elements provides broad system-wide coverage including web browsers & non-Cocoa apps
- DefaultKeyBinding.dict provides native Cocoa support with proper text editing semantics
- Smart exclusions prevent conflicts in terminal environments (Emacs, Terminal, iTerm2)

## Documentation

- [Karabiner-Elements Complex Modifications](https://karabiner-elements.pqrs.org/docs/json/complex-modifications-manipulator-definition/)
- [macOS DefaultKeyBinding.dict Reference](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/EventOverview/TextDefaultsBindings/TextDefaultsBindings.html)
