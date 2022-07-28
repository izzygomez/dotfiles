# Izzy's Dotfiles

Personal dotfiles setup ([why?](https://dotfiles.github.io/)) using [anishathalye/dotfiles](https://github.com/anishathalye/dotfiles).

## Usage

For more instructions, check this [blog post](https://www.elliotdenolf.com/posts/bootstrap-your-dotfiles-with-dotbot) out. This section will briefly detail common flows.

### Adding a dotfile

1. Copy dotfile into this repo (no leading dot!), & make backup
```shell
> cd dotfiles
> cp ~/.dotfile ./dotfile
> mv ~/.dotfile ~/.dotfile-backup
```

1. Add new dotfile into `install.conf.yaml`
```yaml
link:
    ~/.dotfile: dotfile
```

1. Run `install` script, & delete backup if successful.
```shell
> ./install
...
==> All tasks executed successfully
> rm ~/.dotfile-backup
```

### Installing dotfiles on new machine

1. Clone & install
```shell
> git clone git@github.com:izzygomez/dotfiles.git --recursive
> cd dotfiles
> ./install
```

1. Install new updates
```shell
> git pull
> ./install
```

## TODOs

* figure out how to synchronize `tmux` plugins across machines, i.e. automate the following currently-manual steps of having to install plugins the first time `tmux` is run:
```shell
> git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
> tmux source ~/.tmux.conf
# in tmux, press C-a (prefix) + I (capital "i")
```


