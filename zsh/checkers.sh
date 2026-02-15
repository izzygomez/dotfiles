#!/bin/zsh

# Checker functions to use in other config files for conditional config-setting.

is_mac_os() {
    [[ $(uname) == "Darwin" ]]
}

is_linux() {
    [[ $(uname) == "Linux" ]]
}

is_raspberry_pi() {
    is_linux && [[ "$(cat /sys/firmware/devicetree/base/model)" =~ "Raspberry Pi" ]]
}

is_command_available() {
    command -v "$1" &>/dev/null
}

is_izzys_mba() {
    is_mac_os && [[ $(hostname) == "izzys-mba" ]]
}

is_izzys_mbp() {
    is_mac_os && [[ $(hostname) == "izzys-mbp" ]]
}

is_izzys_stripe_mbp() {
    is_mac_os && [[ $(hostname) == "izzys-metro-mbp" || $(hostname) == "st-ig1" ]]
}

is_izzys_raspberry_pi() {
    is_raspberry_pi && [[ $(hostname) == "izzys-raspberrypi" ]]
}
