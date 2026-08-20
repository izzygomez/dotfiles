#!/bin/sh
#
# Restyle the scroll markers in tmux's window list (the middle of the status bar).
# tmux draws that list from a template, status-format[0], which has "<" / ">"
# markers built in — shown at the edges when there are more windows than fit.
# tmux-powerline resets that template to tmux's default on every load, so tmux.conf
# re-runs this right after (see its run-shell line) to reapply our version:
#   - "..." instead of "<" / ">"
#   - a 2-space gap between the window list and the left/right powerline sections
# Idempotent: exits early if already applied.
#
# Each edit below is a plain find/replace on that template string. "#[...]" is
# tmux's own markup for parts of the status line.

fmt=$(tmux show -gv 'status-format[0]')

# Already patched (our "..." is present)? Nothing to do.
case "$fmt" in
*'list=left-marker]...'*) exit 0 ;;
esac

# The markers themselves: "<" and ">"  ->  "..." and "..."
fmt=$(printf '%s' "$fmt" | sed 's/#\[list=left-marker\]<#\[list=right-marker\]>/#[list=left-marker]...#[list=right-marker].../')

# Two spaces where the window list meets the LEFT powerline section.
fmt=$(printf '%s' "$fmt" | sed 's/#\[norange default\]#\[list=on align=/#[norange default]  #[list=on align=/')

# Two spaces where the window list meets the RIGHT powerline section ("&" = the match).
fmt=$(printf '%s' "$fmt" | sed 's/#\[nolist align=right range=right #{E:status-right-style}\]/&  /')

tmux set -g 'status-format[0]' "$fmt"
