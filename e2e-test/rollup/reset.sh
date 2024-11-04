#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

assets=../assets
keypath=./rollup/keys

find $keypath/ -type f ! -name "alice.*" -exec rm {} +
rm -r $assets

echo ""
echo "Keys & assets reset."
echo ""
