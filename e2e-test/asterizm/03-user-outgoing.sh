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

# Build an outgoing Asterizm message (Cardano -> other chain)
# This example uses `user send`, so the minted token will be under the
# universal user policy (`asterizm policy user`), not the client policy.
# Header structure (112 bytes total):
#   srcChainId  (8 bytes):  0x38 (Cardano)
#   srcAddress  (32 bytes): Cardano client policy ID, left-padded to 32 bytes
#   dstChainId  (8 bytes):  0x01 (destination chain)
#   dstAddress  (32 bytes): 0x39d2ba91296029aFBE725436B4824cA803e27391
#   txId        (32 bytes): 0x03
# Payload: abi.decode(payload, (uint, uint, uint)):
#   dstAddressUint: destination-chain address
#   amount:         100
#   txId:           same txId as in the message header

srcChainId="0000000000000038"
dstChainId="0000000000000001"
dstAddress="00000000000000000000000039d2ba91296029afbe725436b4824ca803e27391"
trustedAddress="${dstChainId}${dstAddress}"
clientPolicyId=$(cabal_run zkfold-cli:asterizm -- policy client \
  --client-vkey-file $keypath/client.vkey \
  --trusted-address "$trustedAddress" \
  --outgoing)
srcAddress=$(printf "%064s" "$clientPolicyId" | tr ' ' '0')
txId="0000000000000000000000000000000000000000000000000000000000000003"
amount=$(printf "%064x" 100)
payload="${dstAddress}${amount}${txId}"

message="${srcChainId}${srcAddress}${dstChainId}${dstAddress}${txId}${payload}"
omniPolicyId=$(cabal_run zkfold-cli:asterizm -- policy token \
  --client-vkey-file $keypath/client.vkey \
  --relayer-vkey-file $keypath/relayer.vkey \
  --trusted-address "$trustedAddress")

echo "Message: $message"
echo "Omni-chain token policy ID: $omniPolicyId"
mkdir -p ./assets
echo "$message" > ./assets/message-user-outgoing.hex
echo "Submitting user outgoing message under the universal user policy..."

cabal_run zkfold-cli:asterizm -- user send \
  --core-config-file $configpath \
  --signing-key-file $keypath/client.skey \
  --beneficiary-address $(cat $keypath/client.addr) \
  --omni-policy-id "$omniPolicyId" \
  --omni-token-amt 100 \
  "${hash_args[@]}" \
  --message "$message"
