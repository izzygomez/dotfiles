# Izzy's Dotfiles

Personal [dotfiles](https://dotfiles.github.io/) setup using [dotbot](https://github.com/anishathalye/dotbot). Also setting other preferences, e.g. Firefox, iTerm2, macOS keybindings.

## Usage

For more instructions, check this [blog post](https://www.elliotdenolf.com/posts/bootstrap-your-dotfiles-with-dotbot) out. This section will briefly detail common flows.

### Adding a dotfile or dotdir

1. Copy into this repo (no leading dot!), & make backup

```shell
cd dotfiles
# dotfile
cp ~/.dotfile ./dotfile
mv ~/.dotfile ~/.dotfile-backup
# dotdir
cp -r ~/.dotdir ./dotdir
mv ~/.dotdir ~/.dotdir-backup
```

2. Add into `install.conf.yaml`. Note that there are OS-specific sections & instructions inside that file.

```yaml
- link:
    # dotfile
    ~/.dotfile: dotfile
    # dotdir
    ~/.dotdir:
      create: true # optional
      force: true # optional
      path: dotdir
```

3. Run `install` script, & delete backup if successful

```shell
./install
  ...
  ==> All tasks executed successfully
# dotfile
rm ~/.dotfile-backup
# dotdir
rm -r ~/.dotdir-backup
```

4. (Optional) Add file mapping in `.gitattributes` to enable correct syntax highlighting on Github.

### Installing dotfiles on new machine

1. Clone & install dotfiles

```shell
git clone https://github.com/izzygomez/dotfiles --recursive
cd dotfiles
./install
```

2. Install new dotfile updates

```shell
git pull
./install
```

## Misc

### Machine-specific configuration

Ideally, this dotfiles setup should work out-of-the-box on any machine, regardless of OS & availability of packages. In order to accomplish this, config dotfiles should be split & organized in such a way that settings are only applied for certain machine specs & depending on package availability.

This effort is a 🚧 work in progress 🚧. As an example, see usage of methods in `zsh/checkers.sh` to see how this can be accomplished.

### Pre-commit

This repo uses [`pre-commit`](https://pre-commit.com/) to automatically format & lint files before they are committed, & also as part of the required checks before a PR can be merged via [pre-commit.ci](https://pre-commit.ci/). See `.pre-commit-config.yaml` for configuration details.

### Submodules

Update a submodule to track its remote's default branch, & commit the changes:

```shell
git submodule update --remote {submodule}
git add {submodule} && git commit -m "updated submodule {submodule}" && git push
```

To revert submodule pointers to what's currently committed, run `git submodule update --init`.

<details>
<summary><h2>TODOs</h2></summary>

- Consider writing a script that can be run to enumerate dot files in `~` that need to be ported into this repo & output diff to console output. Can create some sort of "ignore" list to not print out, e.g. `.zsh_history`.
- Figure out how to synchronize `tmux` plugins across machines, i.e. automate the following currently-manual steps of having to install plugins the first time `tmux` is run:

```shell
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
tmux source ~/.tmux.conf
# in tmux, press C-a (prefix) + I (capital "i")
```

- Speaking of `tmux`, enforce using [my PR branch](https://github.com/tmux-plugins/tmux-resurrect/pull/502) for the `tmux-resurrect` plugin (at least until it's merged):

```shell
cd ~/.tmux/plugins/tmux-resurrect
git fetch origin pull/502/head:izzygomez/add-confirm-option-for-save-and-restore
gco izzygomez/add-confirm-option-for-save-and-restore
```

- `emacs`, `undo-tree` package: currently setting up undo tree history files to be saved in `~/.emacs.d/undo-tree-histories`, but there's no mechanism to delete history files for files that have been deleted (or possibly even handle cases where files are renamed?). Investigate this & fix.
- `emacs` is not working well on raspi setup: the `diff-hl-mode` related code for uncommitted changes is not working well (something related to the `add-hook` line?).
- Seems like "Save Changes" setting on iTerm2 > Settings > General > Preferences is not configurable via `defaults` (see [this commit](https://github.com/izzygomez/dotfiles/commit/1407f3b27a351d58c169057d94a67605bab54878) for usage example), so TODO here is to set value of this setting to "Automatically" via some other method; for the moment, am doing this via an `echo` statement in `zshrc_macos`.
- This README is a bit bulky & verbose, consider editing it down to something more streamlined [like this](https://github.com/denolfe/dotfiles/blob/9bb9957a0055cce71071f5c0ad5d050d95f2c255/README.md).
- Address all TODOs littered throughout code. Consider using LLM to enumerate.
- Figure out how to add em-dashes when typing in `emacs`. Some leads here: [1](https://www.reddit.com/r/emacs/comments/lp85tx/this_might_sound_stupid_but_how_do_i_go_about/), [2](https://www.reddit.com/r/emacs/comments/5eb1ay/replacing_2_x_hyphen_with_ndash/), [3](https://github.com/jorgenschaefer/typoel).
- Consider adding [`zsh-completions`](https://github.com/zsh-users/zsh-completions?tab=readme-ov-file) package. For context, tried adding it while setting up [`ngrok completion`](https://ngrok.com/docs/agent/cli/#ngrok-completion) stuff in `.zshrc`, but I found out that the ordering of initializing `zsh-completions` & `ngrok` mattered, & I'd need to refactor dotfiles to get this working. Also, per `zsh-completions` README, it is also worth considering installing via something like `antigen` instead of, say, `brew` so that it can be setup on any OS.
- `rectangle.config.json` is not automatically updated if settings are changed, need to manually export. Fix this. Might require upgrading to [pro version](https://rectangleapp.com/pro).
- Consider adding VSCode/Cursor settings in here.
- Consider installing dotbot as a [Homebrew package](https://formulae.brew.sh/formula/dotbot) & remove as submodule. Would have to update `install` script.

</details>
