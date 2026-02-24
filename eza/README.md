# eza themes

This directory contains [eza](https://github.com/eza-community/eza) themes from the [eza-themes repo](https://github.com/eza-community/eza-themes).

You can install a new theme by running `ln -sf ./<theme-name>.yml ./theme.yml` within this directory.

Note that this entire directory is symlinked to `~/.config/eza` per `install.conf.yaml`, & eza knows to look for a `theme.yml` file in that directory per `EZA_CONFIG_DIR` in `~/.zshrc`.
