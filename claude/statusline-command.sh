#!/bin/bash

# Draws the status line at the bottom of a Claude Code session. See README.md in
# this directory for how it gets wired into settings.json.
#
# The refreshInterval there is load-bearing. Claude Code re-runs this script only
# on session start, a new assistant message, /compact, & a couple of mode toggles.
# /rename & /effort don't trigger it, so without the timer their new values sit
# stale until the next reply.

data=$(cat)

# One jq call for every field, joined with 0x1f rather than a tab: bash collapses
# runs of whitespace even when IFS names them, so an empty field would vanish &
# shift every value after it.
fields=$(jq -r '[
  .session_name // "",
  .workspace.project_dir // "",
  .model.display_name // "",
  .effort.level // "",
  (.context_window.used_percentage // 0 | floor),
  .cost.total_cost_usd // 0,
  .cost.total_lines_added // 0,
  .cost.total_lines_removed // 0,
  .worktree.name // .workspace.git_worktree // "",
  .pr.number // "",
  .pr.review_state // "",
  (if .fast_mode then "fast" else "" end),
  .session_id // ""
] | map(tostring) | join("\u001f")' <<<"$data")

IFS=$'\x1f' read -r session_name project_dir model effort context_pct cost lines_added lines_removed worktree pr_number pr_state fast_mode session_id <<<"$fields"

# Fallbacks: session_name is empty until /rename or an AI-generated title, &
# context_pct is null until the first API response and again after /compact.
[[ -z $session_name ]] && session_name="unnamed"
[[ -z $context_pct ]] && context_pct="0"
project_dir="${project_dir/#$HOME/\~}"
cost=$(printf '%.2f' "${cost:-0}")

line1="session: $session_name  |  project: $project_dir"
[[ -n $worktree ]] && line1+="  |  worktree: $worktree"
[[ -n $pr_number ]] && line1+="  |  PR #${pr_number}${pr_state:+ ($pr_state)}"

line2="model: $model"
[[ -n $effort ]] && line2+="  |  effort: $effort"
[[ -n $fast_mode ]] && line2+="  |  $fast_mode"

line3="context: ${context_pct}%  |  cost: \$${cost}  |  +${lines_added:-0}/-${lines_removed:-0}"

echo "$line1"
echo "$line2"
echo "$line3"

# `claude --resume <id>` reopens this session after a crash, even if it was never
# named. Printed last so it's easy to select & copy.
if [[ -n $session_id ]]; then
    # \033[3m is ANSI italic. Claude Code passes escapes through, so italics need
    # a font with a real italic face.
    printf '\033[3mclaude --resume %s\033[0m\n' "$session_id"
fi
