# Zsh config

The zsh config files here differ in when they're loaded, & so should contain different things. The following table lists the files, in the order they're loaded, & provides a brief overview of what each file is for:

| File                | Purpose                                                                                  |
| ------------------- | ---------------------------------------------------------------------------------------- |
| `zprofile`          | Login shells. One-time setup like `PATH` & env vars.                                     |
| `zshrc`             | Interactive shells. Aliases, prompt, completions, & customization.                       |
| `fragments/zshrc_*` | Device/package-specific config, sourced from `zshrc`. Exist for organizational purposes. |

**See also:** [Zsh Configuration Files Differences](https://filethings.net/zsh-configuration-files/), [.zshrc or .zprofile](https://mac.install.guide/terminal/zshrc-zprofile)
