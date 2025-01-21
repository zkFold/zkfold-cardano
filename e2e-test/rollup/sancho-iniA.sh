#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

sanchomagic=4
assets=../assets
keypath=./rollup/keys
privpath=./rollup/priv

if [ -d "./rollup" ]; then
    mkdir -p $assets
    mkdir -p $keypath
    mkdir -p $privpath
else
    echo "Please run script from directory 'e2e-test'."
    exit 1
fi

printf "$sanchomagic" > $privpath/testnet.flag

./rollup/init-alice.sh
