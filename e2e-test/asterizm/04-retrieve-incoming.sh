#! /bin/bash

set -e
set -u
set -o pipefail

configpath=./assets/config.json
keypath=./keys

cabal run zkfold-cli:asterizm -- retrieve-messages \
  --core-config-file $configpath \
  --client-vkey-file $keypath/client.vkey \
  --relayer-vkey-file $keypath/relayer.vkey \
  --incoming
