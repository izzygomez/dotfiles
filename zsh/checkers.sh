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
    isMacOS && [[ $(hostname) == "izzys-mba" ]]
}

isIzzysMBP() {
    isMacOS && [[ $(hostname) == "izzys-mbp" ]]
}

isIzzysRaspberryPi() {
    isRaspberryPi && [[ $(hostname) == "izzys-raspberrypi" ]]
}
