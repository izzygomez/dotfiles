#!/bin/zsh

# Checker functions to use in other config files for conditional config-setting.

isMacOS() {
    [[ $(uname) == "Darwin" ]]
}

isLinux() {
    [[ $(uname) == "Linux" ]]
}

isRaspberryPi() {
    isLinux && [[ "$(cat /sys/firmware/devicetree/base/model)" =~ "Raspberry Pi" ]]
}

isCommandAvailable() {
    command -v "$1" >/dev/null 2>&1
}

isIzzysMBA() {
    [[ $(hostname) == "izzys-mba" ]]
}

isIzzyMBP() {
    [[ $(hostname) == "izzys-mbp" ]]
}

isIzzysRaspberryPi() {
    [[ $(hostname) == "izzys-raspberrypi" ]]
}
