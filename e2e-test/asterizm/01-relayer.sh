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

# Build an Asterizm message with 112-byte header + payload
# Header structure (112 bytes total):
#   srcChainId  (8 bytes):  0x01
#   srcAddress  (32 bytes): 0x39d2ba91296029aFBE725436B4824cA803e27391
#   dstChainId  (8 bytes):  0x38
#   dstAddress  (32 bytes): Cardano client policy ID, left-padded to 32 bytes
#   txId        (32 bytes): 0x01
# Payload: "Hello, Asterizm!" in hex

srcChainId="0000000000000001"
srcAddress="00000000000000000000000039d2ba91296029afbe725436b4824ca803e27391"
dstChainId="0000000000000038"
trustedAddress="${srcChainId}${srcAddress}"
clientPolicyId=$(cabal_run zkfold-cli:asterizm -- policy client \
  --client-vkey-file $keypath/client.vkey \
  --relayer-vkey-file $keypath/relayer.vkey \
  --trusted-address "$trustedAddress" \
  --incoming)
dstAddress=$(printf "%064s" "$clientPolicyId" | tr ' ' '0')
txId="0000000000000000000000000000000000000000000000000000000000000001"
payload=$(echo -n "Hello, Asterizm!" | xxd -p | tr -d '\n')

message="${srcChainId}${srcAddress}${dstChainId}${dstAddress}${txId}${payload}"

# Compute hash for relayer
messageHash=$(cabal_run zkfold-cli:asterizm -- hash "${hash_args[@]}" --message "$message" | tr -d '"')

echo "Message: $message"
echo "Message hash: $messageHash"

# Save message for use by client-incoming script
mkdir -p ./assets
echo "$message" > ./assets/message-incoming.hex

cabal_run zkfold-cli:asterizm -- relayer \
  --core-config-file $configpath \
  --signing-key-file $keypath/relayer.skey \
  --relayer-vkey-file $keypath/relayer.vkey \
  --beneficiary-address $(cat $keypath/relayer.addr) \
  --message-hash "$messageHash"
