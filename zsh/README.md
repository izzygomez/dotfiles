# Zsh config

These zsh config files differ in when they're loaded, & so should contain different stuff.


| File       | When it runs                  | What it should contain                                                 |
|------------|-------------------------------|------------------------------------------------------------------------|
| `zshenv`   | Every shell                   | Universal env vars (e.g. `PATH`, `LANG`, `HOMEBREW_NO_ANALYTICS`)      |
| `zprofile` | Login shells only             | One-time setup like starting `ssh-agent`, exporting `GPG_TTY`, etc.    |
| `zshrc`    | Interactive shells only       | Aliases, functions, prompt setup, shell options, completions, etc.     |

> Use `zshenv` for portability-critical env vars. Avoid putting interactive config or long-running logic in `zshenv` — it runs for every shell, including scripts.
