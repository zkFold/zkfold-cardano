#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../../assets


echo ""
echo "Cleaning up..."
echo ""

mv $keypath/nextRollupOutB.tx $keypath/rollupOutB.tx
mv $assets/nextRedeemerRollupA.cbor $assets/redeemerRollupA.cbor
mv $assets/nextRedeemerRollupA.json $assets/redeemerRollupA.json

echo "Done, you can now resume rollup loop."
echo ""
