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

mN=$(cat $privpath/testnet.flag)
if [ $mN -ne $sanchomagic ]; then
    echo "This script is only meant to be run on SanchoNet."
    exit 1
fi

./rollup/split-alice.sh
