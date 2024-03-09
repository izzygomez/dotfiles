# Izzy's Dotfiles

Personal dotfiles setup ([huh?](https://dotfiles.github.io/)) using [anishathalye/dotbot](https://github.com/anishathalye/dotbot).

## Usage

For more instructions, check this [blog post](https://www.elliotdenolf.com/posts/bootstrap-your-dotfiles-with-dotbot) out. This section will briefly detail common flows.

### Adding a dotfile

1. Copy dotfile into this repo (no leading dot!), & make backup

```shell
> cd dotfiles
> cp ~/.dotfile ./dotfile
> mv ~/.dotfile ~/.dotfile-backup
```

2. Add new dotfile into `install.conf.yaml`

```yaml
link:
  ~/.dotfile: dotfile
```

3. Run `install` script, & delete backup if successful

```shell
> ./install
...
==> All tasks executed successfully
> rm ~/.dotfile-backup
```

### Installing dotfiles on new machine

1. Clone & install dotfiles

```shell
> git clone https://github.com/izzygomez/dotfiles --recursive
> cd dotfiles
> ./install
```

2. Install new dotfile updates

```shell
> git pull
> ./install
```

## Misc

### Machine-specific configuration

Ideally, this dotfiles setup should work out-of-the-box on any machine, regardless of OS & availability of packages. In order to accomplish this, config dotfiles should be split in such a way that certain settings apply for only certain machine specs.

This effort is a 🚧 work in progress 🚧. See `zsh/checkers.sh` & its usage for examples on how this can be accomplished.

### Shell script formatting

Shell scripts in this repo should be formatted with the following command:

```shell
shfmt -i 4 -w FILE-NAME
```

The `-i 4` flag specifies to use 4 spaces for indentation, & the `-w` flag overwrites `FILE-NAME` in place. This is not automatically enforced yet (see TODOs below).

### Submodules

Our submodule pointers can be updated by simply pulling new changes

```shell
cd submodule
git checkout {main,master,...}
git pull
```

& then returning to the `dotfiles` repo & commiting the updates

```shell
cd ..  # should be in dotfiles/
git add submodule; git commit -m "update `submodule`"; git push
```

Note that `git restore submodule` does not discard working directory changes as expected. To revert the submodule pointer to what it was before, either do as above but instead `git checkout` the previous commit hash, or run `git submodule update --init`.

## TODOs

- Consider writing a script that can be run to enumerate dot files in `~` that need to be ported into this repo & output diff to console output. Can create some sort of "ignore" list to not print out, e.g. `.zsh_history`.
- Figure out how to synchronize `tmux` plugins across machines, i.e. automate the following currently-manual steps of having to install plugins the first time `tmux` is run:

```shell
> git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
> tmux source ~/.tmux.conf
# in tmux, press C-a (prefix) + I (capital "i")
```

- Auto-enforce formatting of shell-like files. See "Shell script formatting" section above.
- `emacs`, `undo-tree` package: currently setting up undo tree history files to be saved in `~/.emacs.d/undo-tree-histories`, but there's no mechanism to delete history files for files that have been deleted (or possibly even handle cases where files are renamed?). Investigate this & fix.
- `emacs` is not working well on raspi setup: the `diff-hl-mode` related code for uncommitted changes is not working well (something related to the `add-hook` line?).
- Seems like "Save Changes" setting on iTerm2 > Settings > General > Preferences is not configurable via `defaults` (see [this commit](https://github.com/izzygomez/dotfiles/commit/1407f3b27a351d58c169057d94a67605bab54878) for usage example), so TODO here is to set value of this setting to "Automatically" via some other method; for the moment, am doing this via an `echo` statement in `zshrc_macos`.
