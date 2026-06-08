#! /bin/bash

set -e
set -u
set -o pipefail

configpath=./config.json
keypath=./keys
trustedAddress="000000000000000100000000000000000000000039d2ba91296029afbe725436b4824ca803e27391"

cabal_run() {
  cabal ${CABAL_FLAGS:-} -v0 run "$@"
}

cabal_run zkfold-cli:asterizm -- retrieve-messages \
  --core-config-file $configpath \
  --client-vkey-file $keypath/client.vkey \
  --relayer-vkey-file $keypath/relayer.vkey \
  --trusted-address "$trustedAddress" \
  --outgoing
