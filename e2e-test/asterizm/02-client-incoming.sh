#! /bin/bash

set -e
set -u
set -o pipefail

configpath=./config.json
keypath=./keys

cabal_run() {
  cabal ${CABAL_FLAGS:-} -v0 run "$@"
}

hash_args=()
if [ "${CROSSCHAIN_HASH:-0}" = "1" ]; then
  hash_args+=(--crosschain-hash)
fi

# Read the incoming message (same message the relayer attested)
message=$(cat ./assets/message-incoming.hex)
trustedAddress="${message:0:80}"

echo "Submitting incoming message to client..."

cabal_run zkfold-cli:asterizm -- client receive \
  --core-config-file $configpath \
  --signing-key-file $keypath/client.skey \
  --client-vkey-file $keypath/client.vkey \
  --relayer-vkey-file $keypath/relayer.vkey \
  --trusted-address "$trustedAddress" \
  --beneficiary-address $(cat $keypath/client.addr) \
  "${hash_args[@]}" \
  --message "$message"
