#!/bin/bash

# Copies every settings-*.json in this directory into ~/.claude/settings.json,
# leaving all other keys alone. See README.md for why that file isn't tracked.
#
# Run by dotbot from install.conf.yaml. Safe to re-run.

set -euo pipefail

settings_dir="$HOME/.claude"
settings_file="$settings_dir/settings.json"
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if ! command -v jq >/dev/null; then
    echo "install-claude-settings: jq not found, cannot merge settings." >&2
    exit 1
fi

if [[ ! -d $settings_dir ]]; then
    echo "install-claude-settings: no $settings_dir, skipping."
    exit 0
fi

fragments=("$script_dir"/settings-*.json)
if [[ ! -e ${fragments[0]} ]]; then
    echo "install-claude-settings: no settings-*.json fragments, skipping."
    exit 0
fi

# jq's `*` merges recursively, so a fragment can set one nested key without
# dropping its siblings. A later fragment wins.
fragment="$(jq -s 'reduce .[] as $f ({}; . * $f)' "${fragments[@]}")"

current="{}"
if [[ -f $settings_file ]]; then
    if ! jq empty "$settings_file" 2>/dev/null; then
        echo "install-claude-settings: $settings_file is not valid JSON, refusing to write." >&2
        exit 1
    fi
    # Bail out if the fragments would change nothing, so jq never reformats the
    # file for no reason.
    if jq -e --argjson frag "$fragment" '. * $frag == .' "$settings_file" >/dev/null; then
        echo "install-claude-settings: settings.json already current."
        exit 0
    fi
    current="$(cat "$settings_file")"
fi

merged="$(jq -n --indent 2 --argjson cur "$current" --argjson frag "$fragment" '$cur * $frag')"

# Write via a temp file so a failure can't truncate settings.json. Copying the
# original first preserves its mode.
tmp="$settings_file.dotfiles-tmp"
trap 'rm -f "$tmp"' EXIT
if [[ -f $settings_file ]]; then
    cp -p "$settings_file" "$tmp"
fi
printf '%s\n' "$merged" >"$tmp"
mv "$tmp" "$settings_file"

echo "install-claude-settings: applied fragments to $settings_file"
