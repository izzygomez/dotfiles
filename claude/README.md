# Claude Code config

Config for [Claude Code](https://code.claude.com/docs). Files here are symlinked individually into `~/.claude/` via `install.conf.yaml`. The entire directory is deliberately not symlinked, since it holds untracked state.

| File                         | Purpose                                                        |
| ---------------------------- | -------------------------------------------------------------- |
| `keybindings.json`           | Key bindings.                                                  |
| `statusline-command.sh`      | Draws the status line at the bottom of a session.              |
| `settings-statusline.json`   | Points `settings.json` at the script above.                    |
| `install-claude-settings.sh` | Merges every `settings-*.json` into `~/.claude/settings.json`. |

`~/.claude/settings.json` is not tracked here: it mixes personal settings with machine-specific ones, e.g. corp permission rules & enabled MCP servers.

Claude Code can't load a partial settings file alongside it, so each setting lives here as a `settings-*.json` fragment. `./install-dotfiles.sh` copies those keys into the real `settings.json` & touches nothing else.

To add a setting, create `settings-<name>.json` with only the keys you want, then run `./install-dotfiles.sh`.
