# Izzy's Dotfiles

Personal [dotfiles](https://dotfiles.github.io/) setup using [dotbot](https://github.com/anishathalye/dotbot). Also setting other preferences, e.g. Firefox, iTerm2, macOS keybindings.

## Usage

For more instructions, check this [blog post](https://www.elliotdenolf.com/posts/bootstrap-your-dotfiles-with-dotbot) out. This section will briefly detail common flows.

### Adding a dotfile

1. Copy dotfile into this repo (no leading dot!), & make backup

```shell
cd dotfiles
cp ~/.dotfile ./dotfile
mv ~/.dotfile ~/.dotfile-backup
```

2. Add new dotfile into `install.conf.yaml`

```yaml
link:
  ~/.dotfile: dotfile
```

3. Run `install` script, & delete backup if successful

```shell
./install
  ...
  ==> All tasks executed successfully
rm ~/.dotfile-backup
```

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

### Shell script formatting

Shell scripts in this repo should be formatted with the following command:

```shell
shfmt -i 4 -w FILE-NAME
```

The `-i 4` flag specifies to use 4 spaces for indentation, & the `-w` flag overwrites `FILE-NAME` in place. This is not automatically enforced yet (see TODOs below). `pre-commit` hooks could be used to enforce this formatting, & this has already been set up in the `pre-commit-config.yaml` file.

### Pre-commit hooks

After installing [`pre-commit`](https://pre-commit.com/) (e.g., via `brew install pre-commit`), run `pre-commit install` to install the hooks. This will run the hooks before every commit. To run the hooks manually, use `pre-commit run --all-files --verbose`.

### Submodules

Our submodule pointers can be updated by simply pulling new changes

```shell
cd submodule
git checkout {main,master,...}
git pull
```

& then returning to the `dotfiles` repo & commiting the updates

```shell
cd ..  # should be in dotfiles/ after running this
git add submodule; git commit -m "update `submodule`"; git push
```

Note that `git restore submodule` does not discard working directory changes as one might expect. To revert the submodule pointer to what it was before, either do as above but instead `git checkout` the previous commit hash, or run `git submodule update --init`.

## TODOs

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

- Auto-enforce formatting of shell-like files. See "Shell script formatting" section above.
- `emacs`, `undo-tree` package: currently setting up undo tree history files to be saved in `~/.emacs.d/undo-tree-histories`, but there's no mechanism to delete history files for files that have been deleted (or possibly even handle cases where files are renamed?). Investigate this & fix.
- `emacs` is not working well on raspi setup: the `diff-hl-mode` related code for uncommitted changes is not working well (something related to the `add-hook` line?).
- Seems like "Save Changes" setting on iTerm2 > Settings > General > Preferences is not configurable via `defaults` (see [this commit](https://github.com/izzygomez/dotfiles/commit/1407f3b27a351d58c169057d94a67605bab54878) for usage example), so TODO here is to set value of this setting to "Automatically" via some other method; for the moment, am doing this via an `echo` statement in `zshrc_macos`.
- This README is a bit bulky & verbose, consider editing it down to something more streamlined [like this](https://github.com/denolfe/dotfiles/blob/9bb9957a0055cce71071f5c0ad5d050d95f2c255/README.md).
- Address all TODOs littered throughout code.
- Figure out how to add em-dashes when typing in `emacs`. Some leads here: [1](https://www.reddit.com/r/emacs/comments/lp85tx/this_might_sound_stupid_but_how_do_i_go_about/), [2](https://www.reddit.com/r/emacs/comments/5eb1ay/replacing_2_x_hyphen_with_ndash/), [3](https://github.com/jorgenschaefer/typoel).
- Consider adding [`zsh-completions`](https://github.com/zsh-users/zsh-completions?tab=readme-ov-file) package. For context, tried adding it while setting up [`ngrok completion`](https://ngrok.com/docs/agent/cli/#ngrok-completion) stuff in `.zshrc`, but I found out that the ordering of initializing `zsh-completions` & `ngrok` mattered, & I'd need to refactor dotfiles to get this working. Also, per `zsh-completions` README, it is also worth considering installing via something like `antigen` instead of, say, `brew` so that it can be setup on any OS.
- Consider adding [Alfred](https://www.alfredapp.com/) preferences sync; requires ["Powerpack" upgrade](https://www.alfredapp.com/help/powerpack/license-types/).
- `rectangle.config.json` is not automatically updated if settings are changed, need to manually export. Fix this. Might require upgrading to [pro version](https://rectangleapp.com/pro).
