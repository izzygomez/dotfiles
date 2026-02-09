# iTerm2 Config

This directory contains `com.googlecode.iterm2.plist`, which iTerm2 loads on startup (see [`zshrc_macos`](../zsh/fragments/zshrc_macos) for setup details).

## Syncing Safely

**Important**: iTerm2 does **not** continuously read from this file. Instead:

- iTerm2 **loads the file once** at startup.
- It then **stores preferences in memory**.
- While running, it will **overwrite the file on disk** to match its current in-memory state.

This means:

- If you `git pull` changes to the plist **while iTerm2 is open**, iTerm2 will soon write over those changes with its own version — effectively undoing your pull.
- If you've already pulled while iTerm2 was open, the changes may already be overwritten on disk, but can be restored using Git.

### What to do

If a commit includes iTerm2 preference changes:

- **Use a different terminal** (like Terminal.app) to pull, so iTerm2 isn't running & can't interfere.

If you already pulled while iTerm2 was open:

1. **Quit iTerm2 completely.**
2. Open Terminal.app.
3. Run `git restore iterm2/com.googlecode.iterm2.plist` to bring back the pulled version.
4. Relaunch iTerm2 so it loads the updated config.

This ensures you get the intended settings & avoid silent overwrites.
